%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains functions for handling and formatting REST API
%%% responses.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_replier).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("modules/onepanel_dns.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").

%% API
-export([throw_on_service_error/2]).
-export([handle_error/3, reply_with_error/3, handle_service_action_async/3,
    handle_service_step/4, handle_session/3]).
-export([format_error/2, format_service_status/2, format_dns_check_result/1,
    format_service_host_status/2, format_service_task_results/1, format_service_step/3,
    format_configuration/0, format_service_configuration/1, format_storage_details/1]).

-type response() :: map() | term().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_utils:throw_on_error/1}
%% @end
%%--------------------------------------------------------------------
-spec throw_on_service_error(Req :: cowboy_req:req(),
    Results :: #error{} | service_executor:results()) ->
    Req :: cowboy_req:req() | no_return().
throw_on_service_error(Req, Results) ->
    service_utils:throw_on_error(Results),
    Req.


%%--------------------------------------------------------------------
%% @doc Sets an error description in the response body.
%% @end
%%--------------------------------------------------------------------
-spec handle_error(Req :: cowboy_req:req(), Type :: atom(), Reason :: term()) ->
    Req :: cowboy_req:req().
handle_error(Req, Type, Reason) ->
    Body = json_utils:encode(format_error(Type, Reason)),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @doc Replies with an error and 500 HTTP code.
%% @end
%%--------------------------------------------------------------------
-spec reply_with_error(Req :: cowboy_req:req(), Type :: atom(), Reason :: term()) ->
    Req :: cowboy_req:req().
reply_with_error(Req, Type, Reason) ->
    Body = json_utils:encode(format_error(Type, Reason)),
    cowboy_req:reply(500, #{}, Body, Req).


%%--------------------------------------------------------------------
%% @doc Sets the task ID as a location in the response header.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_action_async(Req :: cowboy_req:req(),
    TaskId :: service_executor:task_id(), ApiVersion :: integer()) ->
    Req :: cowboy_req:req().
handle_service_action_async(Req, TaskId, ApiVersion) ->
    Prefix = https_listener:get_prefix(ApiVersion),
    Location = <<Prefix/binary, "/tasks/", TaskId/binary>>,
    cowboy_req:set_resp_header(<<"location">>, Location, Req).


%%--------------------------------------------------------------------
%% @doc Sets a formatted result of the single host service step in the response
%% body.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_step(Req :: cowboy_req:req(), Module :: module(),
    Function :: atom(), Results :: service_executor:results()) ->
    Req :: cowboy_req:req().
handle_service_step(Req, Module, Function, Results) ->
    Body = json_utils:encode(format_service_step(Module, Function, Results)),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @doc Sets session details in the response header and body.
%% @end
%%--------------------------------------------------------------------
-spec handle_session(Req :: cowboy_req:req(), SessionId :: onepanel_session:id(),
    Username :: onepanel_user:name()) -> Req :: cowboy_req:req().
handle_session(Req, SessionId, Username) ->
    Req2 = cowboy_req:set_resp_cookie(<<"sessionId">>, SessionId, Req, #{
        max_age => onepanel_env:get(session_ttl) div 1000,
        path => "/",
        secure => true,
        http_only => true
    }),
    Body = json_utils:encode(
        #{<<"sessionId">> => SessionId,
          <<"username">> => Username}
    ),
    cowboy_req:set_resp_body(Body, Req2).


%%--------------------------------------------------------------------
%% @doc Returns a formatted error description.
%% @end
%%--------------------------------------------------------------------
-spec format_error(Type :: atom(), Reason :: term()) -> Response :: response().
format_error(Type, #error{reason = #service_error{} = Error}) ->
    format_error(Type, Error);

format_error(_Type, #service_error{service = Service, action = Action,
    module = Module, function = Function, bad_results = Results}) ->
        #{<<"error">> => <<"Service Error">>,
          <<"description">> => onepanel_utils:join(["Action '", Action,
            "' for a service '", Service, "' terminated with an error."]),
          <<"module">> => Module,
          <<"function">> => Function,
          <<"hosts">> => format_service_hosts_results(Results)};

format_error(Type, Reason) ->
    {Name, Description} = onepanel_errors:translate(Type, Reason),
    #{<<"error">> => Name, <<"description">> => Description}.


%%--------------------------------------------------------------------
%% @doc Returns a formatted service status.
%% @end
%%--------------------------------------------------------------------
-spec format_service_status(SModule :: module(),
    Results :: service_executor:results()) -> Response :: response().
format_service_status(SModule, Results) ->
    {HostsResults, []} = select_service_step(SModule, status, Results),
    lists:foldl(fun
        ({Node, Result}, Acc) -> maps:put(
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            Result, Acc)
    end, #{}, HostsResults).


-spec format_dns_check_result(Results :: service_executor:results()) -> response().
format_dns_check_result(Results) ->
    {[{_Node, Result} | _], []} = select_service_step(dns_check, get, Results),

    IpOrTxtToBinary = fun(IpOrString) ->
        case inet:ntoa(IpOrString) of
            {error, einval} -> onepanel_utils:convert(IpOrString, binary);
            IP -> list_to_binary(IP)
        end
    end,

    maps:map(fun
        (_Key, #dns_check{summary = Summary, expected = E, got = G, bind_records = Records}) ->
        #{
            summary => Summary,
            expected => lists:map(IpOrTxtToBinary, E),
            got => lists:map(IpOrTxtToBinary, G),
            recommended => Records
        };
        (timestamp, Epoch) -> time_utils:epoch_to_iso8601(Epoch);
        (_Key, LiteralValue) -> LiteralValue
    end, Result).


%%--------------------------------------------------------------------
%% @doc @equiv format_service_step(SModule, status, Results)
%% @end
%%--------------------------------------------------------------------
-spec format_service_host_status(SModule :: module(),
    Results :: service_executor:results()) -> Response :: response().
format_service_host_status(SModule, Results) ->
    format_service_step(SModule, status, Results).


%%--------------------------------------------------------------------
%% @doc Returns a formatted service asynchronous task result.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_results(Results :: #error{} | service_executor:results()) ->
    Response :: response().
format_service_task_results(#error{} = Error) ->
    ?throw_error(Error);

format_service_task_results(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            maps:put(<<"status">>, <<"error">>, format_error(error, Error));

        [{task_finished, {Service, Action, #error{}}}, Step | Steps] ->
            {Module, Function, {_, BadResults}} = Step,
            maps:merge(
                #{
                    <<"status">> => <<"error">>,
                    <<"steps">> => format_service_task_steps(lists:reverse(Steps))
                },
                format_error(error, #service_error{
                    service = Service, action = Action, module = Module,
                    function = Function, bad_results = BadResults
                })
            );

        [{task_finished, {_, _, ok}} | Steps] -> #{
                <<"status">> => <<"ok">>,
                <<"steps">> => format_service_task_steps(lists:reverse(Steps))};

        Steps -> #{
                <<"status">> => <<"running">>,
                <<"steps">> => format_service_task_steps(lists:reverse(Steps))}
    end.


%%--------------------------------------------------------------------
%% @doc All storage helper parameters are stored as binaries in op-worker.
%% This functions attempts to find their desired type in API model
%% used for creating them and convert accordingly.
%% @end
%%--------------------------------------------------------------------
format_storage_details(Results) ->
    {[{_Node, Result} | _], []} =
        select_service_step(service_op_worker, get_storages, Results),
    StorageType = onepanel_utils:typed_get(type, Result, atom),
    Model = get_storage_model(StorageType),
    Typemap = model_to_typemap(Model),
    maps:map(fun
        (Key, Value) when is_binary(Value) ->
            case maps:find(onepanel_utils:convert(Key, atom), Typemap) of
                {ok, string} -> onepanel_utils:convert(Value, binary);
                {ok, Type} -> onepanel_utils:convert(Value, Type);
                error -> Value
            end;
        (_Key, Value) -> Value
    end, Result).


%%--------------------------------------------------------------------
%% @doc Returns a formatted result of the single host service step.
%% @end
%%--------------------------------------------------------------------
-spec format_service_step(Module :: module(), Function :: atom(),
    Results :: service_executor:results()) -> Response :: response().
format_service_step(Module, Function, Results) ->
    {[{_, HostResult}], []} = select_service_step(Module, Function, Results),
    case erlang:is_list(HostResult) of
        true -> json_utils:list_to_map(HostResult);
        false -> HostResult
    end.


-spec format_configuration() -> map().
format_configuration() ->
    try
        format_configuration(onepanel_env:get(release_type))
    catch _:_  ->
        % it is preferable for this endpoint to return something
        format_configuration(common)
    end.


%%--------------------------------------------------------------------
%% @doc Returns formatted cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec format_service_configuration(SModule :: service_onezone | service_oneprovider) ->
    Response :: response().
format_service_configuration(SModule) ->
    DbHosts = service_couchbase:get_hosts(),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(service_cluster_manager:name()),
    WrkHosts = case SModule of
        service_onezone -> service_oz_worker:get_hosts();
        service_oneprovider -> service_op_worker:get_hosts()
    end,
    {SName, Ctx, Details} = case SModule of
        service_onezone ->
            {ok, #service{ctx = ServiceCtx}} = service:get(service_onezone:name()),
            OzDetails = #{domainName => service_oz_worker:get_domain()},
            {maps:get(name, ServiceCtx, null), ServiceCtx, OzDetails};
        service_oneprovider ->
            {ok, #service{ctx = ServiceCtx}} = service:get(service_oneprovider:name()),
            Name = case service_oneprovider:is_registered(ServiceCtx) of
                true ->
                    case oz_providers:get_details(provider) of
                        {ok, #provider_details{name = N}} -> N;
                        {error, _Reason} -> null
                    end;
                false -> null
            end,
            {Name, ServiceCtx, #{}}
    end,
    MasterHostBin = case maps:get(master_host, Ctx, null) of
        null -> null;
        MasterHost -> onepanel_utils:convert(MasterHost, binary)
    end,
    #{
        <<"cluster">> => #{
            <<"master">> => MasterHostBin,
            <<"databases">> => #{
                <<"hosts">> => onepanel_utils:convert(DbHosts, {seq, binary})},
            <<"managers">> => #{
                <<"mainHost">> => onepanel_utils:convert(MainCmHost, binary),
                <<"hosts">> => onepanel_utils:convert(CmHosts, {seq, binary})},
            <<"workers">> => #{
                <<"hosts">> => onepanel_utils:convert(WrkHosts, {seq, binary})}
        },
        SModule:name() => Details#{
            <<"name">> => SName,
            <<"configured">> => is_service_configured()
        }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns hosts results of the selected service step.
%% Throws an exception if the step is not found.
%% @end
%%--------------------------------------------------------------------
-spec select_service_step(Module :: module(), Function :: atom(),
    Results :: service_executor:results()) ->
    HostsResults :: service_executor:hosts_results().
select_service_step(Module, Function, []) ->
    ?throw_error({?ERR_SERVICE_STEP_NOT_FOUND, Module, Function});

select_service_step(Module, Function, [{Module, Function, Results} | _]) ->
    Results;

select_service_step(Module, Function, [_ | Results]) ->
    select_service_step(Module, Function, Results).


%%--------------------------------------------------------------------
%% @doc Returns a list of formatted service steps.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_steps(Steps :: [service_executor:step_result()]) ->
    [StepName :: binary()].
format_service_task_steps(Steps) ->
    lists:filtermap(fun
        ({Module, Function}) ->
            {true, onepanel_utils:join([Module, Function], <<":">>)};
        (_) ->
            false
    end, Steps).


%%--------------------------------------------------------------------
%% @doc Returns a formatted service step hosts results.
%% @end
%%--------------------------------------------------------------------
-spec format_service_hosts_results(Results :: onepanel_rpc:results()) ->
    Response :: response().
format_service_hosts_results(Results) ->
    lists:foldl(fun
        ({Node, #error{} = Error}, Acc) -> maps:put(
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            format_error(error, Error), Acc);
        ({Node, Result}, Acc) -> maps:put(
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            Result, Acc)
    end, #{}, Results).


%% @private
-spec format_configuration(ReleaseType :: onezone | oneprovider | common) ->
    #{atom() := term()}.
format_configuration(onezone) ->
    Defaults = #{serviceType => onezone, zoneDomain => null, zoneName => null},
    try
        Details = service_oz_worker:get_details(#{}),
        Configuration = onepanel_maps:get_store_multiple([
            {domain, zoneDomain},
            {name, zoneName}
        ], Details, Defaults),

        maps:merge(Configuration, format_configuration(common))
    catch _:_ ->
        % probably no oz_worker nodes
        maps:merge(Defaults, format_configuration(common))
    end;

format_configuration(oneprovider) ->
    Common = #{serviceType => oneprovider},
    OpConfiguration = case service_oneprovider:is_registered() of
        false ->
            Common#{
                zoneDomain => null,
                providerId => null,
                isRegistered => false
            };
        true ->
            try #{} = service_oneprovider:get_details(#{}) of
                Details ->
                    onepanel_maps:get_store_multiple([
                        {id, providerId},
                        {onezoneDomainName, zoneDomain}
                    ], Details, Common#{isRegistered => true})
            catch
                _:_ ->
                    % If op_worker was configured, the Onezone domain can begin
                    % read even if it's down.
                    Common#{
                        zoneDomain => list_to_binary(service_oneprovider:get_oz_domain()),
                        providerId => null,
                        isRegistered => true
                    }
            end
    end,
    maps:merge(format_configuration(common), OpConfiguration);

format_configuration(_ReleaseType) ->
    BuildVersion = list_to_binary(application:get_env(
        ?APP_NAME, build_version, "unknown")),
    {_AppId, _AppName, AppVersion} = lists:keyfind(
        ?APP_NAME, 1, application:loaded_applications()
    ),
    Version = list_to_binary(AppVersion),

    #{
        version => Version,
        build => BuildVersion,
        deployed => is_service_configured()
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc Checks if all configuration steps have been performed.
%% @end
%%--------------------------------------------------------------------
-spec is_service_configured() -> boolean().
is_service_configured() ->
    onepanel_deployment:is_completed(?PROGRESS_READY).


%%--------------------------------------------------------------------
%% @private
%% @doc Simplifies API model by stripping optionality modifiers
%% and filtering out more complex specs such as "anyOf".
%% @end
%%--------------------------------------------------------------------
-spec model_to_typemap(Model :: #{atom() := atom() | tuple()}) ->
    #{Key :: atom() := Type :: atom()}.
model_to_typemap(Model) ->
    maps:fold(fun
        (Key, Type, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, optional}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, required}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, {optional, _}}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (_Key, _ValueSpec, Acc) -> Acc
    end, #{}, Model).


%%--------------------------------------------------------------------
%% @private
%% @doc Finds REST model used for creating storage of given type.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_model(StorageType :: atom()) -> onepanel_parser:spec().
get_storage_model(posix) -> rest_model:posix_model();
get_storage_model(s3) -> rest_model:s3_model();
get_storage_model(ceph) -> rest_model:ceph_model();
get_storage_model(cephrados) -> rest_model:cephrados_model();
get_storage_model(swift) -> rest_model:swift_model();
get_storage_model(glusterfs) -> rest_model:glusterfs_model();
get_storage_model(nulldevice) -> rest_model:nulldevice_model();
get_storage_model(webdav) -> rest_model:webdav_model().

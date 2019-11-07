%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
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
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("modules/onepanel_dns.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_providers.hrl").

%% API
-export([throw_on_service_error/2]).
-export([handle_error/3, reply_with_error/3, handle_service_action_async/3,
    handle_service_step/4]).
-export([format_error/2, format_service_status/2, format_dns_check_result/1,
    format_service_host_status/2, format_service_task_results/1, format_service_step/3,
    format_onepanel_configuration/0, format_service_configuration/1,
    format_storage_details/1, format_deployment_progress/0]).
-export([format_ceph_pools/0]).

-type response() :: map() | term().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_utils:throw_on_error/1}
%% @end
%%--------------------------------------------------------------------
-spec throw_on_service_error(Req, Results) -> Req | no_return() when
    Req :: cowboy_req:req(), Results :: #error{} | {error, term()} | service_executor:results().
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
reply_with_error(Req, Type, {badrpc, nodedown}) ->
    reply_with_error(Req, Type, ?make_error(?ERR_NODE_DOWN));

reply_with_error(Req, Type, Reason) ->
    Body = json_utils:encode(format_error(Type, Reason)),
    Code = error_code(Type, Reason),
    cowboy_req:reply(Code, #{}, Body, Req).


%%--------------------------------------------------------------------
%% @private
%% @doc Determines response HTTP code based on error reason.
%% @end
%%--------------------------------------------------------------------
-spec error_code(Type :: atom(), Reason :: term()) -> cowboy:http_status().
error_code(_Type, #error{reason = Reason}) ->
    case Reason of
        ?ERR_NOT_FOUND -> ?HTTP_404_NOT_FOUND;
        ?ERR_NODE_DOWN -> ?HTTP_503_SERVICE_UNAVAILABLE;
        ?ERR_ONEZONE_NOT_AVAILABLE -> ?HTTP_503_SERVICE_UNAVAILABLE;
        _ -> ?HTTP_500_INTERNAL_SERVER_ERROR
    end;

error_code(Type, Reason) ->
    error_code(Type, ?make_error(Reason)).


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
            onepanel_utils:convert(hosts:from_node(Node), binary),
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
-spec format_service_task_results(Results :: {service_executor:results(), Total} | #error{}) ->
    Response :: response()
    when Total :: non_neg_integer() | #error{}.
format_service_task_results(#error{} = Error) ->
    ?throw_error(Error);

format_service_task_results({#error{} = Error, _}) ->
    ?throw_error(Error);

format_service_task_results({Results, TotalSteps}) ->
    Base = case TotalSteps of
        _ when is_integer(TotalSteps) -> #{totalSteps => TotalSteps};
        _Error -> #{}
    end,
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            maps:merge(Base#{status => <<"error">>}, format_error(error, Error));

        [{task_finished, {Service, Action, {error, _}}}, Step | Steps] ->
            {Module, Function, {_, BadResults}} = Step,
            maps:merge(
                Base#{
                    status => <<"error">>,
                    steps => format_service_task_steps(lists:reverse(Steps))
                },
                format_error(error, #service_error{
                    service = Service, action = Action, module = Module,
                    function = Function, bad_results = BadResults
                })
            );

        [{task_finished, {_, _, ok}} | Steps] -> Base#{
            status => <<"ok">>,
            steps => format_service_task_steps(lists:reverse(Steps))
        };

        Steps -> Base#{
            status => <<"running">>,
            steps => format_service_task_steps(lists:reverse(Steps))
        }
    end.


%%--------------------------------------------------------------------
%% @doc All storage helper parameters are stored as binaries in op_worker.
%% This function attempts to find their desired type in API model
%% used for creating them and convert accordingly.
%% @end
%%--------------------------------------------------------------------
-spec format_storage_details(service_executor:results()) ->
    #{atom() => term()}.
format_storage_details(Results) ->
    {[{_Node, Result} | _], []} =
        select_service_step(service_op_worker, get_storages, Results),
    StorageType = onepanel_utils:get_converted(type, Result, atom),
    Model = get_storage_model(StorageType),
    Typemap = model_to_typemap(Model),
    maps:map(fun
        (Key, Value) when is_binary(Value) ->
            KeyAtom = onepanel_utils:convert(Key, atom),
            case maps:find(KeyAtom, Typemap) of
                {ok, string} -> Value;
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


-spec format_ceph_pools() -> #{pools := [map()]}.
format_ceph_pools() ->
    Results = service_utils:throw_on_error(service:apply_sync(
            ?SERVICE_CEPH, get_all_pools, #{}
        )),
    {[{_, HostResult}], []} = select_service_step(ceph_pool, get_all, Results),
    #{pools => HostResult}.


%%--------------------------------------------------------------------
%% @doc Formats public configuration details.
%% @end
%%--------------------------------------------------------------------
-spec format_onepanel_configuration() -> map().
format_onepanel_configuration() ->
    try
        format_onepanel_configuration(onepanel_env:get_cluster_type())
    catch _:_ ->
        % it is preferable for this endpoint to return something
        format_onepanel_configuration(common)
    end.


%%--------------------------------------------------------------------
%% @doc Returns formatted cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec format_service_configuration(SModule :: service_onezone | service_oneprovider) ->
    Response :: response().
format_service_configuration(SModule) ->
    DbHosts = hosts:all(?SERVICE_CB),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(?SERVICE_CM),
    WrkHosts = case SModule of
        service_onezone -> hosts:all(?SERVICE_OZW);
        service_oneprovider -> hosts:all(?SERVICE_OPW)
    end,
    {SName, Ctx, Details} = case SModule of
        service_onezone ->
            {ok, #service{ctx = ServiceCtx}} = service:get(?SERVICE_OZ),
            OzDetails = #{domainName => service_oz_worker:get_domain()},
            {maps:get(name, ServiceCtx, null), ServiceCtx, OzDetails};
        service_oneprovider ->
            {ok, #service{ctx = ServiceCtx}} = service:get(?SERVICE_OP),
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
    Ceph = case service:get_hosts(?SERVICE_CEPH) /= [] of
        true -> #{
            <<"ceph">> => onepanel_maps:merge([
                service_ceph_mgr:list(),
                service_ceph_mon:list(),
                service_ceph_osd:list()
            ])
        };
        false ->
            #{}
    end,
    Ceph#{
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


-spec format_deployment_progress() ->
    JsonMap :: #{atom() => boolean()}.
format_deployment_progress() ->
    Fields = rest_onepanel:rest_to_marker_mapping(),
    Fields2 = case onepanel_env:get_cluster_type() of
        oneprovider ->
            [{isRegistered, fun service_oneprovider:is_registered/0} | Fields];
        onezone -> Fields
    end,

    lists:foldl(fun
        ({Key, Fun}, Acc) when is_function(Fun) -> Acc#{Key => Fun()};
        ({Key, Mark}, Acc) -> Acc#{Key => onepanel_deployment:is_set(Mark)}
    end, #{}, Fields2).


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
            onepanel_utils:convert(hosts:from_node(Node), binary),
            format_error(error, Error), Acc);
        ({Node, {error, _} = Error}, Acc) -> maps:put(
            onepanel_utils:convert(hosts:from_node(Node), binary),
            format_error(error, Error), Acc);
        ({Node, Result}, Acc) -> maps:put(
            onepanel_utils:convert(hosts:from_node(Node), binary),
            Result, Acc)
    end, #{}, Results).


%% @private
-spec format_onepanel_configuration(ClusterType :: onedata:cluster_type() | common) ->
    #{atom() := term()}.
format_onepanel_configuration(onezone) ->
    Defaults = #{serviceType => onezone, zoneDomain => null, zoneName => null},
    try
        Details = service_oz_worker:get_details(#{}),
        Configuration = kv_utils:copy_found([
            {domain, zoneDomain},
            {name, zoneName}
        ], Details, Defaults),

        onepanel_maps:undefined_to_null(
            maps:merge(Configuration, format_onepanel_configuration(common))
        )
    catch _:_ ->
        % probably no oz_worker nodes
        maps:merge(Defaults, format_onepanel_configuration(common))
    end;

format_onepanel_configuration(oneprovider) ->
    Common = #{serviceType => oneprovider},
    OpConfiguration = case service_oneprovider:is_registered() of
        false ->
            Common#{
                zoneDomain => null,
                providerId => null,
                isRegistered => false
            };
        true ->
            try
                Details = service_oneprovider:get_details(),
                kv_utils:copy_found([
                    {id, providerId},
                    {onezoneDomainName, zoneDomain}
                ], Details, Common#{isRegistered => true})
            catch
                _:_ ->
                    % If op_worker was configured, the Onezone domain can be
                    % read even when the worker is down.
                    Common#{
                        zoneDomain => list_to_binary(service_oneprovider:get_oz_domain()),
                        providerId => null,
                        isRegistered => true
                    }
            end
    end,
    maps:merge(format_onepanel_configuration(common), OpConfiguration);

format_onepanel_configuration(_ClusterType) ->
    ClusterId = try clusters:get_id() catch _:_ -> null end,
    {BuildVersion, AppVersion} = onepanel_app:get_build_and_version(),
    #{
        version => AppVersion,
        build => BuildVersion,
        deployed => is_service_configured(),
        clusterId => ClusterId
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc Checks if all configuration steps have been performed.
%% @end
%%--------------------------------------------------------------------
-spec is_service_configured() -> boolean().
is_service_configured() ->
    onepanel_deployment:is_set(?PROGRESS_READY).


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
get_storage_model(localceph) -> rest_model:localceph_model();
get_storage_model(swift) -> rest_model:swift_model();
get_storage_model(glusterfs) -> rest_model:glusterfs_model();
get_storage_model(nulldevice) -> rest_model:nulldevice_model();
get_storage_model(webdav) -> rest_model:webdav_model().

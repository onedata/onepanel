%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_panel type.
%%% Handles various operations which are not specific to oz/op panels.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
operation_supported(create, emergency_passphrase, private) -> true;
operation_supported(create, invite_token, private) -> true;

operation_supported(get, configuration, private) -> true;
operation_supported(get, cookie, private) -> true;
operation_supported(get, test_image, private) -> true;
operation_supported(get, progress, private) -> true;
operation_supported(get, web_cert, private) -> true;
operation_supported(get, dns_check, private) -> true;
operation_supported(get, dns_check_configuration, private) -> true;
operation_supported(get, emergency_passphrase, private) -> true;
operation_supported(get, {task, _Id}, private) -> true;

operation_supported(update, progress, private) -> true;
operation_supported(update, web_cert, private) -> true;
operation_supported(update, dns_check_configuration, private) -> true;

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(create, emergency_passphrase, private) -> [];
required_availability(create, invite_token, private) -> [];

required_availability(get, configuration, private) -> [];
required_availability(get, cookie, private) -> [];
required_availability(get, test_image, private) -> [];
required_availability(get, progress, private) -> [];
required_availability(get, web_cert, private) -> [];
required_availability(get, dns_check, private) -> [];
required_availability(get, dns_check_configuration, private) -> [];
required_availability(get, emergency_passphrase, private) -> [];
required_availability(get, {task, _Id}, private) -> [];

required_availability(update, progress, private) -> [];
required_availability(update, web_cert, private) -> [all_healthy];
required_availability(update, dns_check_configuration, private) -> [].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{operation = create,
    client = #client{role = guest}, gri = #gri{aspect = emergency_passphrase}
}, _) ->
    not emergency_passphrase:is_set();

authorize(#onp_req{operation = create,
    client = #client{role = member}, gri = #gri{aspect = invite_token}
}, _) ->
    true;

authorize(#onp_req{operation = get,
    client = #client{role = guest}, gri = #gri{aspect = Aspect}
}, _) when
    Aspect == configuration;
    Aspect == test_image;
    Aspect == emergency_passphrase
->
    true;

authorize(#onp_req{
    operation = get, client = #client{role = _Any}, gri = #gri{aspect = {task, _}}
}, _) ->
    true;

authorize(#onp_req{
    operation = get, client = #client{role = Role}, gri = #gri{aspect = cookie}
}, _) when
    Role == member;
    Role == peer
->
    true;

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == progress;
    As == web_cert;
    As == dns_check;
    As == dns_check_configuration
->
    true;

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = As}
}, _) when
    As == progress;
    As == web_cert;
    As == dns_check_configuration
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{
    operation = create, gri = #gri{aspect = emergency_passphrase}
}, _) ->
    % validation is part of the passphrase-changing function
    ok;

validate(#onp_req{
    operation = create, gri = #gri{aspect = invite_token}
}, _) ->
    % validation is part of the passphrase-changing function
    ok;

validate(#onp_req{operation = get, gri = #gri{aspect = web_cert}}, _) ->
    case service:exists(?SERVICE_LE) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = dns_check}}, _) ->
    case onepanel_deployment:is_set(?PROGRESS_CLUSTER) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = {task, Id}}}, _) ->
    case service:exists_task(Id) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = Aspect}}, _) when
    Aspect == configuration;
    Aspect == cookie;
    Aspect == test_image;
    Aspect == progress;
    Aspect == dns_check_configuration;
    Aspect == emergency_passphrase
->
    ok;

validate(#onp_req{operation = update, gri = #gri{aspect = Aspect}}, _) when
    Aspect == progress;
    Aspect == web_cert;
    Aspect == dns_check_configuration
->
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = emergency_passphrase}, data = Data}) ->
    #{newPassphrase := NewPassphrase} = Data,
    CurrentPassphrase = maps:get(currentPassphrase, Data, undefined),
    emergency_passphrase:change(CurrentPassphrase, NewPassphrase);

create(#onp_req{gri = #gri{aspect = invite_token}}) ->
    {ok, value, invite_tokens:create()}.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = configuration}}, _) ->
    {ok, value, middleware_utils:format_onepanel_configuration()};

get(#onp_req{gri = #gri{aspect = cookie}}, _) ->
    {ok, value, erlang:get_cookie()};

get(#onp_req{gri = #gri{aspect = test_image}}, _) ->
    % Dummy image in png format. Used by gui to check connectivity.
    {ok, value, <<
        137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0,
        0, 1, 0, 0, 0, 1, 1, 3, 0, 0, 0, 37, 219, 86, 202, 0, 0, 0, 6, 80,
        76, 84, 69, 0, 0, 0, 255, 255, 255, 165, 217, 159, 221, 0, 0, 0, 9,
        112, 72, 89, 115, 0, 0, 14, 196, 0, 0, 14, 196, 1, 149, 43, 14, 27,
        0, 0, 0, 10, 73, 68, 65, 84, 8, 153, 99, 96, 0, 0, 0, 2, 0, 1, 244,
        113, 100, 166, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130
    >>};

get(#onp_req{gri = #gri{aspect = progress}}, _) ->
    {ok, value, format_deployment_progress()};

get(#onp_req{gri = #gri{aspect = web_cert}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_LE, get_details)};

get(#onp_req{gri = #gri{aspect = dns_check}, data = Data}, _) ->
    Ctx = #{force_check => maps:get(forceCheck, Data, false)},
    {ok, value, middleware_utils:result_from_service_action(
        cluster_worker_name(), dns_check, Ctx,
        dns_check, get
    )};

get(#onp_req{gri = #gri{aspect = dns_check_configuration}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        cluster_worker_name(), get_dns_check_configuration, #{},
        dns_check, get_configuration
    )};

get(#onp_req{gri = #gri{aspect = emergency_passphrase}}, _) ->
    {ok, value, emergency_passphrase:is_set()};

get(#onp_req{gri = #gri{aspect = {task, Id}}}, _) ->
    Response = format_service_task_results(service:get_results(Id)),
    {ok, value, Response}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = progress}, data = Data}) ->
    Mapping = rest_to_marker_mapping(),
    MarksToSet = lists:filtermap(fun({Field, Bool}) ->
        case lists:keyfind(Field, 1, Mapping) of
            {Field, ProgressMark} -> {true, {ProgressMark, Bool}};
            false -> false
        end
    end, maps:to_list(Data)),

    lists:foreach(fun
        ({Marker, true}) -> onepanel_deployment:set_marker(Marker);
        ({Marker, false}) -> onepanel_deployment:unset_marker(Marker)
    end, MarksToSet);

update(#onp_req{gri = #gri{aspect = web_cert}, data = Data}) ->
    Ctx = #{letsencrypt_enabled => maps:get(letsEncrypt, Data)},
    middleware_utils:execute_service_action(?SERVICE_LE, update, Ctx);

update(#onp_req{
    gri = #gri{aspect = dns_check_configuration}, data = Data
}) ->
    Ctx = kv_utils:copy_found([
        {dnsServers, dns_servers},
        {builtInDnsServer, built_in_dns_server},
        {dnsCheckAcknowledged, dns_check_acknowledged}
    ], Data),
    middleware_utils:execute_service_action(
        cluster_worker_name(), configure_dns_check, Ctx
    ).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% internal functions
%%%===================================================================

-spec format_deployment_progress() ->
    JsonMap :: #{atom() => boolean()}.
format_deployment_progress() ->
    Fields = rest_to_marker_mapping(),
    Fields2 = case onepanel_env:get_cluster_type() of
        oneprovider ->
            [{isRegistered, fun service_oneprovider:is_registered/0} | Fields];
        onezone ->
            Fields
    end,

    lists:foldl(fun
        ({Key, Fun}, Acc) when is_function(Fun) -> Acc#{Key => Fun()};
        ({Key, Mark}, Acc) -> Acc#{Key => onepanel_deployment:is_set(Mark)}
    end, #{}, Fields2).


%%--------------------------------------------------------------------
%% @doc Maps between rest 'progress' endpoint fields and atoms
%% used in onepanel_deployment model.
%% @end
%%--------------------------------------------------------------------
-spec rest_to_marker_mapping() -> [{atom(), onepanel_deployment:marker()}].
rest_to_marker_mapping() ->
    rest_to_marker_mapping(onepanel_env:get_cluster_type()).


%% @private
-spec rest_to_marker_mapping(onedata:cluster_type() | common) ->
    [{RestField :: atom(), ProgressMark :: onepanel_deployment:marker()}].
rest_to_marker_mapping(onezone) ->
    rest_to_marker_mapping(common);

rest_to_marker_mapping(oneprovider) ->
    [
        {storagesSetup, ?PROGRESS_STORAGE_SETUP}
        | rest_to_marker_mapping(common)
    ];

rest_to_marker_mapping(common) -> [
    {clusterNodes, ?PROGRESS_CLUSTER},
    {clusterIps, ?PROGRESS_CLUSTER_IPS},
    {webCertificate, ?PROGRESS_LETSENCRYPT_CONFIG},
    {dnsCheck, ?DNS_CHECK_ACKNOWLEDGED}
].


%%--------------------------------------------------------------------
%% @private @doc Returns name of cluster worker depending on the release type.
%% @end
%%--------------------------------------------------------------------
-spec cluster_worker_name() -> service:name().
cluster_worker_name() ->
    case onepanel_env:get_cluster_type() of
        oneprovider -> ?SERVICE_OPW;
        onezone -> ?SERVICE_OZW
    end.



%%--------------------------------------------------------------------
%% @doc Returns a formatted service asynchronous task result.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_results(Results :: {service_executor:results(), Total}) ->
    json_utils:json_map()
    when Total :: non_neg_integer() | {error, _}.
format_service_task_results({{error, _} = Error, _}) ->
    throw(Error);

format_service_task_results({Results, TotalSteps}) ->
    Base = case TotalSteps of
        _ when is_integer(TotalSteps) -> #{totalSteps => TotalSteps};
        _Error -> #{}
    end,

    case service_utils:results_contain_error(Results) of
        {true, Error} ->
            Base2 = maps:merge(Base#{
                <<"status">> => <<"error">>
            }, format_error(Error)),

            case format_service_task_steps(Results) of
                [] -> Base2;
                StepNames -> Base2#{steps => StepNames}
            end;
        false ->
            case lists:reverse(Results) of
                [{task_finished, {_, _, ok}} | _] ->
                    Base#{
                        <<"status">> => <<"ok">>,
                        <<"steps">> => format_service_task_steps(Results)
                    };
                _ ->
                    Base#{
                        <<"status">> => <<"running">>,
                        <<"steps">> => format_service_task_steps(Results)
                    }
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns a formatted error description.
%% @end
%%--------------------------------------------------------------------
-spec format_error(Reason :: term()) -> json_utils:json_map().
format_error({error, #exception{}}) ->
    #{<<"error">> => errors:to_json(?ERROR_INTERNAL_SERVER_ERROR)};
format_error(Reason) ->
    #{<<"error">> => errors:to_json(Reason)}.


%%--------------------------------------------------------------------
%% @doc Returns a list of formatted service steps.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_steps(Steps :: [service_executor:step_result()]) ->
    [StepName :: binary()].
format_service_task_steps(Steps) ->
    lists:filtermap(fun
        ({task_finished, _}) ->
            false;
        ({Module, Function}) ->
            {true, onepanel_utils:join([Module, Function], <<":">>)};
        (_) -> false
    end, Steps).

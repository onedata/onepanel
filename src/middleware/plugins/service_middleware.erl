%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_service type.
%%% Handles: deployment on new nodes, stopping and starting, checking status.
%%% In requests concerning a single host, the #gri.id is used to select it.
%%% @end
%%%-------------------------------------------------------------------
-module(service_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("modules/models.hrl").
-include("names.hrl").
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
% Deployment of new service nodes.
% Plural names, since multiple nodes are deployed in a single request.
operation_supported(create, couchbase_instances, private) -> true;
operation_supported(create, cluster_manager_instances, private) -> true;
operation_supported(create, op_worker_instances, private) -> onepanel:is_op_panel();
operation_supported(create, oz_worker_instances, private) -> onepanel:is_oz_panel();

operation_supported(get, {all_hosts_status, <<"cluster_manager">>}, private) -> true;
operation_supported(get, {all_hosts_status, <<"couchbase">>}, private) -> true;
operation_supported(get, {all_hosts_status, <<"op_worker">>}, private) -> onepanel:is_op_panel();
operation_supported(get, {all_hosts_status, <<"oz_worker">>}, private) -> onepanel:is_oz_panel();

operation_supported(get, {host_status, <<"cluster_manager">>}, private) -> true;
operation_supported(get, {host_status, <<"couchbase">>}, private) -> true;
operation_supported(get, {host_status, <<"op_worker">>}, private) -> onepanel:is_op_panel();
operation_supported(get, {host_status, <<"oz_worker">>}, private) -> onepanel:is_oz_panel();

% Nagios proxy.
operation_supported(get, {nagios, <<"op_worker">>}, private) -> onepanel:is_op_panel();
operation_supported(get, {nagios, <<"oz_worker">>}, private) -> onepanel:is_oz_panel();

% Start/stop all hosts
operation_supported(update, {start_stop_all, <<"couchbase">>}, private) -> true;
operation_supported(update, {start_stop_all, <<"cluster_manager">>}, private) -> true;
operation_supported(update, {start_stop_all, <<"op_worker">>}, private) -> onepanel:is_op_panel();
operation_supported(update, {start_stop_all, <<"oz_worker">>}, private) -> onepanel:is_oz_panel();

% Start/stop a single host
operation_supported(update, {start_stop, <<"couchbase">>}, private) -> true;
operation_supported(update, {start_stop, <<"cluster_manager">>}, private) -> true;
operation_supported(update, {start_stop, <<"op_worker">>}, private) -> onepanel:is_op_panel();
operation_supported(update, {start_stop, <<"oz_worker">>}, private) -> onepanel:is_oz_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(_, _, _) -> [].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = As}
}, _) when
    As == couchbase_instances;
    As == cluster_manager_instances;
    As == op_worker_instances;
    As == oz_worker_instances
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = {As, _}}
}, _) when
    As == all_hosts_status;
    As == host_status;
    As == nagios
->
    true;

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = {Aspect, _}}
}, _) when
    Aspect == start_stop;
    Aspect == start_stop_all
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{
    operation = create, gri = #gri{aspect = As}
}, _) when
    As == couchbase_instances;
    As == cluster_manager_instances;
    As == op_worker_instances;
    As == oz_worker_instances
->
    ok;

validate(#onp_req{operation = get, gri = #gri{aspect = {all_hosts_status, _}}}, _) ->
    ok;

validate(#onp_req{
    operation = get, gri = #gri{aspect = {host_status, ServiceBin}, id = <<HostBin/binary>>}
}, _) ->
    Service = binary_to_atom(ServiceBin, utf8),
    Host = binary_to_list(HostBin),
    ensure_has_host(Service, Host);

validate(#onp_req{operation = get, gri = #gri{aspect = {nagios, <<"op_worker">>}}}, _) ->
    case service_op_worker:get_hosts() /= [] of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = {nagios, <<"oz_worker">>}}}, _) ->
    case service_oz_worker:get_hosts() /= [] of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{
    operation = update, gri = #gri{aspect = {start_stop, ServiceBin}, id = <<HostBin/binary>>}
}, _) ->
    Service = binary_to_atom(ServiceBin, utf8),
    Host = binary_to_list(HostBin),
    ensure_has_host(Service, Host);

validate(#onp_req{
    operation = update, gri = #gri{aspect = {start_stop_all, _Services}}
}, _) ->
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = couchbase_instances}, data = Data}) ->
    Hosts = onepanel_utils:get_converted(hosts, Data, {seq, list}),
    Ctx = kv_utils:copy_found([
        {serverQuota, couchbase_server_quota},
        {bucketQuota, couchbase_bucket_quota}
    ], Data, #{hosts => Hosts}),
    {ok, value, _TaskId = service:apply_async(?SERVICE_CB, deploy, Ctx)};

create(#onp_req{gri = #gri{aspect = cluster_manager_instances}, data = Data}) ->
    Hosts = onepanel_utils:get_converted(hosts, Data, {seq, list}),
    MainHost = onepanel_utils:get_converted(mainHost, Data, list),
    Ctx = #{main_host => MainHost, hosts => Hosts},
    {ok, value, _TaskId = service:apply_async(?SERVICE_CM, deploy, Ctx)};

create(#onp_req{gri = #gri{aspect = Aspect}, data = Data}) when
    Aspect == oz_worker_instances;
    Aspect == op_worker_instances
->
    Service = case Aspect of
        oz_worker_instances -> ?SERVICE_OZW;
        op_worker_instances -> ?SERVICE_OPW
    end,
    Hosts = onepanel_utils:get_converted(hosts, Data, {seq, list}),
    {ok, #service{hosts = DbHosts}} = service:get(service_couchbase:name()),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(?SERVICE_CM),
    {ok, value, _TaskId = service:apply_async(Service, deploy, #{
        hosts => Hosts, db_hosts => DbHosts, cm_hosts => CmHosts,
        main_cm_host => MainCmHost
    })}.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = {host_status, ServiceBin}, id = HostBin}}, _) ->
    Service = binary_to_atom(ServiceBin, utf8),
    Module = service:get_module(Service),
    HostList = str_utils:binary_to_unicode_list(HostBin),
    {ok, value, middleware_utils:result_from_service_action(
        Service, status, #{hosts => [HostList]},
        Module, status
    )};

get(#onp_req{gri = #gri{aspect = {all_hosts_status, ServiceBin}}}, _) ->
    Service = binary_to_atom(ServiceBin, utf8),
    Module = service:get_module(Service),
    Results = service:apply_sync(Service, status, #{}),
    {HostsResults, []} = service_utils:select_service_step(Module, status, Results),
    {ok, value, lists:foldl(fun({Node, NodeStatus}, Acc) ->
        Host = onepanel_utils:convert(hosts:from_node(Node), binary),
        Acc#{Host => NodeStatus}
    end, #{}, HostsResults)};

get(#onp_req{gri = #gri{aspect = {nagios, WorkerBin}}}, _) ->
    Worker = binary_to_atom(WorkerBin, utf8),
    try
        {ok, Code, Headers, Body} = middleware_utils:result_from_service_action(
            Worker, get_nagios_response
        ),
        {ok, value, #{code => Code, headers => Headers, body => Body}}
    catch _:_ ->
        throw(?ERROR_SERVICE_UNAVAILABLE)
    end.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{
    gri = #gri{aspect = {start_stop_all, ServiceBin}}, data = Data
}) ->
    Service = binary_to_atom(ServiceBin, utf8),
    case Data of
        #{started := true} ->
            middleware_utils:execute_service_action(Service, start, #{});
        #{started := false} ->
            middleware_utils:execute_service_action(Service, stop, #{})
    end;

update(#onp_req{
    gri = #gri{aspect = {start_stop, ServiceBin}, id = <<HostBin/binary>>}, data = Data
}) ->
    Service = binary_to_atom(ServiceBin, utf8),
    Host = binary_to_list(HostBin),
    case Data of
        #{started := true} ->
            middleware_utils:execute_service_action(
                Service, start, #{hosts => [Host]});
        #{started := false} ->
            middleware_utils:execute_service_action(
                Service, stop, #{hosts => [Host]})
    end.


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_has_host(service:name(), Host :: service:host()) -> ok | no_return().
ensure_has_host(Service, Host) ->
    case service:has_host(Service, Host) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.


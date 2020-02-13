%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_host type.
%%% Handles addition and removal of nodes from the Onepanel cluster,
%%% as well as configuring the external IPs of each node.
%%% @end
%%%-------------------------------------------------------------------
-module(host_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
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
%% invite a remote host to the cluster
operation_supported(create, instance, private) -> true;
%% executed on a solitary node to join a cluster
operation_supported(create, join_cluster, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, list, private) -> true;
operation_supported(get, external_ips, private) -> true;

operation_supported(update, external_ips, private) -> true;

operation_supported(delete, instance, private) -> true;

operation_supported(_, _, _) -> false.



-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(update, external_ips, private) -> [all_healthy];
required_availability(_, _, _) -> [].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | errors:error().
fetch_entity(#onp_req{gri = #gri{id = Id}}) ->
    Host = binary_to_list(Id),
    case lists:member(Host, service_onepanel:get_hosts()) of
        true -> {ok, {undefined, 1}};
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = create, gri = #gri{aspect = join_cluster},
    client = #client{role = Role}
}, _) ->
    Role == guest andalso service_onepanel:available_for_clustering();

authorize(#onp_req{
    operation = get, gri = #gri{aspect = instance}, client = #client{role = _Any}
}, _) ->
    % node information is public
    true;

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == list;
    As == external_ips
->
    true;

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = external_ips}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = delete, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    ok;

validate(#onp_req{operation = create, gri = #gri{aspect = join_cluster}}, _) ->
    case service_onepanel:available_for_clustering() of
        true -> ok;
        false -> throw(?ERROR_NODE_ALREADY_IN_CLUSTER(hosts:self()))
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = As}}, _) when
    As == instance;
    As == list;
    As == external_ips
->
    ok;

validate(#onp_req{operation = update, gri = #gri{aspect = external_ips}}, _) ->
    ok;

validate(#onp_req{
    operation = delete, gri = #gri{aspect = instance, id = HostBin}
}, _) ->
    Host = binary_to_list(HostBin),
    lists:member(Host, service_onepanel:get_hosts())
        orelse throw(?ERROR_NOT_FOUND),
    service_onepanel:is_host_used(Host)
        andalso throw(?ERROR_NOT_SUPPORTED),
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = instance}, data = Data}) ->
    Address = maps:get(address, Data),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_PANEL, extend_cluster, #{address => Address}
    )};

create(#onp_req{gri = #gri{aspect = join_cluster}, data = Data}) ->
    Host = onepanel_utils:get_converted(clusterHost, Data, list),
    Ctx = maps:with([cluster_host, cookie], Data#{cluster_host => Host}),
    middleware_utils:execute_service_action(?SERVICE_PANEL, join_cluster, Ctx).


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance}}, _) ->
    Hostname = onepanel_utils:convert(hosts:self(), binary),
    ClusterType = onepanel_env:get_cluster_type(),
    {ok, value, #{<<"hostname">> => Hostname, <<"clusterType">> => ClusterType}};

get(#onp_req{gri = #gri{aspect = list}}, _) ->
    {ok, value, lists:sort(service_onepanel:get_hosts())};

get(#onp_req{gri = #gri{aspect = external_ips}}, _) ->
    Service = case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> ?SERVICE_OP;
        ?ONEZONE -> ?SERVICE_OZ
    end,
    {ok, value, middleware_utils:result_from_service_action(
        Service, format_cluster_ips
    )}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = external_ips}, data = Data}) ->
    ClusterIps = maps:get(hosts, Data),
    Service = case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> ?SERVICE_OP;
        ?ONEZONE -> ?SERVICE_OZ
    end,
    Ctx = #{cluster_ips => onepanel_utils:convert(ClusterIps, {keys, list})},

    middleware_utils:execute_service_action(Service, set_cluster_ips, Ctx).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{gri = #gri{aspect = instance, id = Id}}) ->
    Host = binary_to_list(Id),
    middleware_utils:execute_service_action(
        ?SERVICE_PANEL, leave_cluster, #{hosts => [Host]}).

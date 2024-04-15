%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains oneS3 service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ones3).
-author("Bartosz Walkowicz").

-behaviour(service_behaviour).

-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([
    exists/0,

    get_domain/0, get_port/0,

    create_service/1, add_service_host/1,

    set_node_ip/1,
    format_hosts_ips/0,
    get_hosts_ips/0
]).


%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> service:name().
name() ->
    ?SERVICE_ONES3.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    nodes:all(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(create, #{hosts := Hosts}) ->
    NewHosts = lists_utils:subtract(Hosts, get_hosts()),

    [
        #step{function = create_service, selection = first},
        #step{hosts = NewHosts, function = add_service_host}
    ];

get_steps(set_cluster_ips, #{hosts := Hosts} = _Ctx) ->
    [#step{function = set_node_ip, hosts = Hosts}];
get_steps(set_cluster_ips, #{cluster_ips := HostsToIps} = Ctx) ->
    % execute only on nodes where ip is explicitly provided
    get_steps(set_cluster_ips, Ctx#{hosts => lists_utils:intersect(get_hosts(), maps:keys(HostsToIps))});
get_steps(set_cluster_ips, Ctx) ->
    % execute on all service hosts, "guessing" IP if necessary
    get_steps(set_cluster_ips, Ctx#{hosts => get_hosts()}).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec exists() -> boolean().
exists() ->
    service:exists(name()).


-spec get_domain() -> binary().
get_domain() ->
    OpDomain = service_op_worker:get_domain(),
    <<"s3.", OpDomain/binary>>.


-spec get_port() -> undefined | inet:port_number().
get_port() ->
    onepanel_env:get(http_port, ?SERVICE_ONES3).


-spec create_service(service:step_ctx()) -> ok.
create_service(_Ctx) ->
    case service:create(#service{name = name()}) of
        {ok, _} -> ok;
        ?ERR_ALREADY_EXISTS -> ok
    end.


-spec add_service_host(service:step_ctx()) -> ok.
add_service_host(_Ctx) ->
    Host = hosts:self(),
    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc
%% Writes node Ip to app.config on the current node's panel.
%% If Ip is not given explicitly in cluster_ips map
%% and panel has none in its app config onepanel tries to determine it.
%% @end
%%--------------------------------------------------------------------
-spec set_node_ip(Ctx :: service:step_ctx()) -> ok | no_return().
set_node_ip(Ctx) ->
    Host = hosts:self(),

    {ok, Ip} = case kv_utils:find([cluster_ips, Host], Ctx) of
        {ok, null} ->
            {ok, undefined};
        {ok, NewIp} ->
            onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS),
            ip_utils:to_ip4_address(NewIp);
        _ ->
            {ok, infer_ip()}
    end,

    onepanel_env:write([name(), external_ip], Ip, ?SERVICE_PANEL),
    onepanel_env:set([external_ip], Ip, name()),

    ok.
%%    %% TODO??
%%    dns_check:invalidate_cache(?SERVICE_ONES3).


-spec format_hosts_ips() -> #{Host :: binary() => Ip :: binary()}.
format_hosts_ips() ->
    maps:from_list(lists:map(fun
        ({Host, undefined}) ->
            {onepanel_utils:convert(Host, binary), null};
        ({Host, IP}) ->
            {onepanel_utils:convert(Host, binary), onepanel_ip:ip4_to_binary(IP)}
    end, get_hosts_ips())).


-spec get_hosts_ips() -> [{service:host(), inet:ip4_address()}].
get_hosts_ips() ->
    Args = [[name(), external_ip], ?SERVICE_PANEL],

    lists:map(fun(Host) ->
        Node = nodes:service_to_node(?SERVICE_PANEL, Host),
        {ok, Ip} = rpc:call(Node, onepanel_env, read_effective, Args),
        {Host, Ip}
    end, hosts:all(name())).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec infer_ip() -> inet:ip4_address().
infer_ip() ->
    case onepanel_env:read_effective([name(), external_ip], ?SERVICE_PANEL) of
        {ok, {_, _, _, _} = Ip} ->
            Ip;
        {ok, IpList} when is_list(IpList) ->
            {ok, Ip} = ip_utils:to_ip4_address(IpList),
            Ip;
        _ ->
            onepanel_ip:determine_ip(name())
    end.

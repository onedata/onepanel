%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains onezone services management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onezone).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

-export([reconcile_dns/1]).

-define(SERVICE_OPA, service_onepanel:name()).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_CW, service_cluster_worker:name()).
-define(SERVICE_OZW, service_oz_worker:name()).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    onezone.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    lists:usort(lists:append([
        service:get_hosts(?SERVICE_CB),
        service:get_hosts(?SERVICE_CM),
        service:get_hosts(?SERVICE_OZW)
    ])).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    lists:usort(lists:append([
        service:get_nodes(?SERVICE_CB),
        service:get_nodes(?SERVICE_CM),
        service:get_nodes(?SERVICE_OZW)
    ])).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, Ctx) ->
    {ok, OpaCtx} = onepanel_maps:get([cluster, ?SERVICE_OPA], Ctx),
    {ok, CbCtx} = onepanel_maps:get([cluster, ?SERVICE_CB], Ctx),
    {ok, CmCtx} = onepanel_maps:get([cluster, ?SERVICE_CM], Ctx),
    {ok, OzwCtx} = onepanel_maps:get([cluster, ?SERVICE_OZW], Ctx),
    OzCtx = onepanel_maps:get(name(), Ctx, #{}),
    S = #step{verify_hosts = false},
    Ss = #steps{verify_hosts = false},
    [
        Ss#steps{service = ?SERVICE_OPA, action = deploy, ctx = OpaCtx},
        Ss#steps{service = ?SERVICE_CB, action = deploy, ctx = CbCtx},
        S#step{service = ?SERVICE_CB, function = status, ctx = CbCtx},
        Ss#steps{service = ?SERVICE_CM, action = deploy, ctx = CmCtx},
        S#step{service = ?SERVICE_CM, function = status, ctx = CmCtx},
        Ss#steps{service = ?SERVICE_OZW, action = deploy, ctx = OzwCtx},
        S#step{service = ?SERVICE_OZW, function = status, ctx = OzwCtx},
        S#step{module = service, function = save, ctx = OpaCtx,
            args = [#service{name = name(), ctx = OzCtx}],
            selection = first
        },
        Ss#steps{service = ?SERVICE_OPA, action = add_users, ctx = OpaCtx}
    ];

get_steps(start, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = start},
        #steps{service = ?SERVICE_CM, action = start},
        #steps{service = ?SERVICE_OZW, action = start}
    ];

get_steps(stop, _Ctx) ->
    [
        #steps{service = ?SERVICE_OZW, action = stop},
        #steps{service = ?SERVICE_CM, action = stop},
        #steps{service = ?SERVICE_CB, action = stop}
    ];

get_steps(restart, _Ctx) ->
    [
        #steps{action = stop},
        #steps{action = start}
    ];

get_steps(status, _Ctx) ->
    [
        #steps{service = ?SERVICE_CB, action = status},
        #steps{service = ?SERVICE_CM, action = status},
        #steps{service = ?SERVICE_OZW, action = status}
    ];

get_steps(modify_ips, Ctx) ->
    AppConfigFile = service_ctx:get(oz_worker_app_config_file, Ctx),
    Ctx2 = Ctx#{
        app_config_file => AppConfigFile,
        name => ?SERVICE_OZW
    },
    [#steps{action = modify_ips, ctx = Ctx2, service = ?SERVICE_CW},
       #step{function = reconcile_dns, selection = any}];

get_steps(get_cluster_ips, Ctx) ->
    [#step{function = get_cluster_ips, service = ?SERVICE_OZW, selection = any}].

-spec reconcile_dns(service:ctx()) -> ok.
reconcile_dns(#{node := Node} = Ctx) ->
    ok = rpc:call(Node, node_manager_plugin, reconcile_dns, []);
reconcile_dns(Ctx) ->
    [Node | _] = service_op_worker:get_nodes(),
    reconcile_dns(Ctx#{node => Node}).


%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains onepanel service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_onepanel).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("names.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([set_cookie/1, check_connection/1, ensure_all_hosts_available/1,
    init_cluster/1, extend_cluster/1, join_cluster/1, reset_node/1,
    ensure_node_ready/1, reload_webcert/1, add_users/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?APP_NAME.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    onepanel_cluster:nodes_to_hosts(get_nodes()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    onepanel_db:get_nodes().


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    Host = onepanel_cluster:node_to_host(),
    ClusterHosts = get_hosts(),
    NewHosts = onepanel_lists:subtract(Hosts, ClusterHosts),
    S = #step{hosts = [Host], verify_hosts = false},
    [
        #steps{action = init_cluster,
            condition = fun(_) -> ClusterHosts =:= [] end
        },
        S#step{function = extend_cluster,
            ctx = Ctx#{hosts => NewHosts},
            condition = fun(_) ->
                ClusterHosts /= [] andalso erlang:length(NewHosts) >= 1
            end
        },
        S#step{function = extend_cluster,
            ctx = Ctx#{hosts => lists:delete(Host, NewHosts)},
            condition = fun(_) ->
                ClusterHosts == [] andalso erlang:length(NewHosts) >= 2
            end
        }
    ];

get_steps(init_cluster, _Ctx) ->
    S = #step{hosts = [onepanel_cluster:node_to_host()], verify_hosts = false},
    [
        S#step{function = set_cookie},
        S#step{function = reset_node},
        S#step{function = init_cluster}
    ];

get_steps(join_cluster, #{cluster_host := ClusterHost}) ->
    Host = onepanel_cluster:node_to_host(),
    case ClusterHost of
        Host -> [];
        _ ->
            S = #step{hosts = [Host], verify_hosts = false},
            [
                S#step{function = set_cookie},
                S#step{function = check_connection},
                S#step{function = reset_node},
                S#step{function = join_cluster}
            ]
    end;

%% Removes given nodes from the current cluster, clears database on each
%% and initalizes separate one-node clusters.
get_steps(leave_cluster, #{hosts := Hosts}) ->
    [#step{function = reset_node, hosts = Hosts}];
get_steps(leave_cluster, Ctx) ->
    get_steps(leave_cluster, Ctx#{hosts => [onepanel_cluster:node_to_host()]});

%% Utility for managing cluster restart, waiting for all onepanel
%% nodes in the cluster to start.
get_steps(wait_for_cluster, _Ctx) ->
    SelfHost = onepanel_cluster:node_to_host(),
    Attempts = application:get_env(?APP_NAME, wait_for_cluster_attempts, 20),
    [
        #step{service = name(), function = ensure_all_hosts_available,
            attempts = Attempts, hosts = [SelfHost]},
        #step{service = name(), function = ensure_node_ready,
            attempts = Attempts, hosts = get_hosts()}
    ];

get_steps(add_users, #{users := _}) ->
    [#step{function = add_users, selection = any}];

get_steps(add_users, _Ctx) ->
    [];

get_steps(reload_webcert, _Ctx) ->
    [#step{function = reload_webcert}].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sets the node cookie.
%% @end
%%--------------------------------------------------------------------
-spec set_cookie(Ctx :: service:ctx()) -> ok | no_return().
set_cookie(#{cookie := Cookie} = Ctx) ->
    VmArgsFile = service_ctx:get(vm_args_file, Ctx),
    erlang:set_cookie(node(), Cookie),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile);

set_cookie(Ctx) ->
    set_cookie(Ctx#{cookie => erlang:get_cookie()}).


%%--------------------------------------------------------------------
%% @doc Checks whether this erlang node can be connected to the remote cluster
%% node.
%% @end
%%--------------------------------------------------------------------
-spec check_connection(Ctx :: service:ctx()) -> ok.
check_connection(#{cluster_host := ClusterHost}) ->
    ClusterNode = onepanel_cluster:host_to_node(ClusterHost),
    pong = net_adm:ping(ClusterNode),
    ok.


%%--------------------------------------------------------------------
%% @doc Initializes onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec init_cluster(Ctx :: service:ctx()) -> ok | no_return().
init_cluster(_Ctx) ->
    onepanel_db:init(),
    onepanel_db:create_tables().


%%--------------------------------------------------------------------
%% @doc Extends onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec extend_cluster(Ctx :: service:ctx()) -> ok.
extend_cluster(#{hosts := Hosts, auth := Auth, api_version := ApiVersion} = Ctx) ->
    ClusterHost = onepanel_cluster:node_to_host(),
    Port = rest_listener:port(),
    Prefix = rest_listener:get_prefix(ApiVersion),
    Suffix = onepanel_utils:join(["/hosts?clusterHost=", ClusterHost]),
    Body = json_utils:encode(#{cookie => erlang:get_cookie()}),
    Timeout = service_ctx:get(extend_cluster_timeout, Ctx, integer),
    CaCerts = rest_listener:get_cert_chain_pems(),
    Opts = [
        {ssl_options, [{secure, only_verify_peercert}, {cacerts, CaCerts}]},
        {connect_timeout, Timeout},
        {recv_timeout, Timeout}
    ],
    lists:foreach(fun(Host) ->
        Url = onepanel_utils:join(["https://", Host, ":", Port, Prefix, Suffix]),
        {ok, 204, _, _} = http_client:post(
            Url, #{
                <<"authorization">> => Auth,
                <<"Content-Type">> => <<"application/json">>
            }, Body,
            Opts
        )
    end, Hosts).


%%--------------------------------------------------------------------
%% @doc Adds this node to the remote onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Ctx :: service:ctx()) -> ok.
join_cluster(#{cluster_host := ClusterHost}) ->
    Node = node(),
    ClusterNode = onepanel_cluster:host_to_node(ClusterHost),
    ok = rpc:call(ClusterNode, onepanel_db, add_node, [Node]),
    onepanel_db:copy_tables().


%%--------------------------------------------------------------------
%% @doc Removes all the user and configuration data from this host.
%% Removes host from the database cluster.
%% @end
%%--------------------------------------------------------------------
-spec reset_node(Ctx :: service:ctx()) -> ok | no_return().
reset_node(_Ctx) ->
    Node = node(),
    ClusterNodes = lists:delete(node(), get_nodes()),
    onepanel_db:delete_tables(),
    onepanel_db:destroy(),
    onepanel_rpc:call_all(ClusterNodes, onepanel_db, remove_node, [Node]),
    ok = mnesia:start().


%%--------------------------------------------------------------------
%% @doc Adds users.
%% @end
%%--------------------------------------------------------------------
-spec add_users(Ctx :: service:ctx()) -> ok.
add_users(#{users := Users}) ->
    maps:fold(fun(Username, #{password := Password, userRole := Role}, _) ->
        case onepanel_user:create_noexcept(Username, Password, Role) of
            {ok, _} -> ok;
            #error{reason = ?ERR_ALREADY_EXISTS} ->
                onepanel_user:change_password(Username, Password),
                onepanel_user:update(Username, #{role => Role})
        end
    end, ok, Users).


%%--------------------------------------------------------------------
%% @doc
%% Ensures all cluster hosts are up.
%% @end
%%--------------------------------------------------------------------
-spec ensure_all_hosts_available(service:ctx()) -> ok | no_return().
ensure_all_hosts_available(_Ctx) ->
    Hosts = get_hosts(),
    lists:foreach(fun(Host) ->
        ok = check_connection(#{cluster_host => Host})
    end, Hosts),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Fails if some children of the main supervisor are not running.
%% @end
%%--------------------------------------------------------------------
ensure_node_ready(_Ctx) ->
    Counts = supervisor:count_children(onepanel_sup),
    true = (proplists:get_value(specs, Counts) == proplists:get_value(active, Counts)).


%%--------------------------------------------------------------------
%% @doc
%% Ensures certificates changed on disk are updated in listeners.
%% @end
%%--------------------------------------------------------------------
-spec reload_webcert(service:ctx()) -> ok.
reload_webcert(_Ctx) ->
    ssl:clear_pem_cache().

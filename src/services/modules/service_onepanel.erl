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
-include("deployment_progress.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([set_cookie/1, check_connection/1, ensure_all_hosts_available/1,
    init_cluster/1, extend_cluster/1, join_cluster/1, reset_node/1,
    ensure_node_ready/1, reload_webcert/1, add_users/1, available_for_clustering/0]).

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
    SelfHost = onepanel_cluster:node_to_host(),
    ClusterHosts = get_hosts(),
    NewHosts = onepanel_lists:subtract(Hosts, ClusterHosts),
    Attempts = application:get_env(?APP_NAME, extend_cluster_attempts, 20),
    [#step{
        function = extend_cluster, hosts = [SelfHost],
        ctx = Ctx#{hostname => NewHost, attempts => Attempts}
    } || NewHost <- NewHosts];

get_steps(extend_cluster, Ctx) ->
    [
        #step{function = extend_cluster, hosts = [onepanel_cluster:node_to_host()],
            ctx = Ctx#{attempts => 1}}
    ];

get_steps(init_cluster, _Ctx) ->
    S = #step{hosts = [onepanel_cluster:node_to_host()], verify_hosts = false},
    [
        S#step{function = set_cookie},
        S#step{function = reset_node},
        S#step{function = init_cluster}
    ];

get_steps(join_cluster, #{cluster_host := ClusterHost}) ->
    SelfHost = onepanel_cluster:node_to_host(),
    case {available_for_clustering(), ClusterHost} of
        {_, SelfHost} -> [];
        {false, _} -> ?throw_error(?ERR_NODE_NOT_EMPTY(SelfHost));
        {true, _} ->
            S = #step{hosts = [SelfHost], verify_hosts = false},
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
    lists:foreach(fun(Host) ->
        case is_used(Host) of
            true -> ?throw_error(?ERR_NODE_NOT_EMPTY(Host));
            false -> ok
        end
    end, Hosts),
    [
        #step{function = reset_node, hosts = Hosts},
        #step{function = init_cluster, hosts = Hosts}
    ];
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
%% @doc Adds given node to the current onepanel cluster.
%% Node can be identified by its exact erlang Hostname or
%% by any other Address. In the second case preliminary request
%% is made to learn proper node hostname and ensure it's built for
%% the same application.
%% Returns a map containing the node's Hostname.
%% Fails if given node is already part of a cluster, including
%% the current one.
%% @end
%%--------------------------------------------------------------------
-spec extend_cluster(service:ctx()) -> #{hostname := binary()} | no_return().
extend_cluster(#{attempts := Attempts}) when Attempts =< 0 ->
    ?throw_error(?ERR_BAD_NODE);

extend_cluster(#{hostname := Hostname, api_version := ApiVersion,
    attempts := Attempts} = Ctx) ->
    SelfHost = onepanel_cluster:node_to_host(),
    Body = json_utils:encode(#{
        cookie => erlang:get_cookie(),
        clusterHost => onepanel_utils:convert(SelfHost, binary)
    }),
    Headers = #{<<"Content-Type">> => <<"application/json">>},
    Suffix = "/join_cluster",
    Timeout = service_ctx:get(extend_cluster_timeout, Ctx, integer),
    Opts = https_opts(Timeout),
    Url = build_url(Hostname, ApiVersion, Suffix),

    case http_client:post(Url, Headers, Body, Opts) of
        {ok, 403, _, _} ->
            ?throw_error(?ERR_NODE_NOT_EMPTY(Hostname));
        {ok, 204, _, _} ->
            ?info("Host '~s' added to the cluster", [Hostname]),
            #{hostname => Hostname};
        {error, _} ->
            ?warning("Failed to connect with '~s' to extend cluster", [Hostname]),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end;

extend_cluster(#{address := Address, api_version := _ApiVersion,
    attempts := Attempts} = Ctx) ->
    ClusterType = onepanel_env:get(release_type),
    case get_remote_node_info(Ctx) of
        {ok, Hostname, ClusterType} ->
            extend_cluster(Ctx#{hostname => Hostname});
        {ok, Hostname, OtherType} ->
            ?throw_error(?ERR_INCOMPATIBLE_NODE(Address, OtherType));
        #error{reason = ?ERR_BAD_NODE} ->
            ?warning("Failed to connect with '~s' to extend cluster", [Address]),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end.


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
%% If a user already exists its password and role are updated.
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
    end, Hosts).


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


%%--------------------------------------------------------------------
%% @doc
%% Checks if current node is in a fresh state which allows it to join
%% other cluster.
%% @end
%%--------------------------------------------------------------------
available_for_clustering() ->
    onepanel_user:get_by_role(admin) == [] andalso length(get_hosts()) =< 1
    andalso not onepanel_deployment:is_completed(?PROGRESS_CLUSTER).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns information about remote node. The node must be ready to join
%% a cluster, ie. contain no configured users.
%% @end
%%--------------------------------------------------------------------
-spec get_remote_node_info(service:ctx()) ->
    {ok, Hostname :: binary(), Application :: atom()} | #error{} | no_return().
get_remote_node_info(#{address := Address, api_version := ApiVersion} = Ctx) ->
    Timeout = service_ctx:get(extend_cluster_timeout, Ctx, integer),
    Opts = https_opts(Timeout),
    Suffix = <<"/node">>,
    Url = build_url(Address, ApiVersion, Suffix),

    Headers = #{<<"Content-Type">> => <<"application/json">>},

    case http_client:get(Url, Headers, <<>>, Opts) of
        {ok, 200, _, Body} ->
            #{<<"hostname">> := Hostname,
                <<"componentType">> := ReleaseType} = json_utils:decode(Body),
            {ok, Hostname, onepanel_utils:convert(ReleaseType, atom)};
        {error, _} -> ?make_error(?ERR_BAD_NODE)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds url for given onepanel REST endpoint
%% @end
%%--------------------------------------------------------------------
-spec build_url(Host :: binary() | string(), ApiVersion :: rest_handler:version(),
    Suffix :: binary() | string()) -> binary().
build_url(Host, ApiVersion, Suffix) ->
    Port = rest_listener:port(),
    Prefix = rest_listener:get_prefix(ApiVersion),
    onepanel_utils:join(["https://", Host, ":", Port, Prefix, Suffix]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns connection opts for clustering requests.
%% @end
%%--------------------------------------------------------------------
-spec https_opts(Timeout :: non_neg_integer()) -> http_client:opts().
https_opts(Timeout) ->
    CaCerts = rest_listener:get_cert_chain_pems(),
    [
        {ssl_options, [{secure, only_verify_peercert}, {cacerts, CaCerts}]},
        {connect_timeout, Timeout},
        {recv_timeout, Timeout}
    ].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given host is used to run oneprovider or onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_used(service:host()) -> boolean().
is_used(Host) ->
    ClusterType = onepanel_env:get(release_type),
    SModule = service:get_module(ClusterType),
    try
        lists:member(Host, SModule:get_hosts())
    catch
        _:_ ->
            % incorrect during deployment when some services are
            % already deployed but other are not
            false
    end.
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
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

-define(WAIT_FOR_CLUSTER_DELAY,
    onepanel_env:get(wait_for_cluster_retry_delay, ?APP_NAME, timer:seconds(5))).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([set_cookie/1, configure/1, check_connection/1,
    ensure_all_hosts_available/1, init_cluster/1, extend_cluster/1,
    join_cluster/1, reset_node/1, ensure_node_ready/1, reload_webcert/1,
    available_for_clustering/0]).

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
    hosts:from_nodes(get_nodes()).


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
    SelfHost = hosts:self(),
    ClusterHosts = get_hosts(),
    NewHosts = onepanel_lists:subtract(Hosts, ClusterHosts),
    Attempts = application:get_env(?APP_NAME, extend_cluster_attempts, 20),
    [#step{
        function = extend_cluster, hosts = [SelfHost],
        ctx = Ctx#{hostname => NewHost, attempts => Attempts}
    } || NewHost <- NewHosts];

get_steps(extend_cluster, Ctx) ->
    [
        #step{function = extend_cluster, hosts = [hosts:self()],
            % when the reason of extend_cluster is an explicit request, do not retry
            ctx = Ctx#{attempts => 1}}
    ];

get_steps(init_cluster, _Ctx) ->
    S = #step{hosts = [hosts:self()], verify_hosts = false},
    [
        S#step{function = set_cookie},
        S#step{function = reset_node},
        S#step{function = init_cluster}
    ];

get_steps(join_cluster, #{cluster_host := ClusterHost}) ->
    SelfHost = hosts:self(),
    case {available_for_clustering(), ClusterHost} of
        {_, SelfHost} -> [];
        {false, _} -> ?throw_error(?ERR_NODE_NOT_EMPTY(SelfHost));
        {true, _} ->
            S = #step{hosts = [SelfHost], verify_hosts = false},
            [
                S#step{function = set_cookie},
                S#step{function = check_connection,
                    attempts = onepanel_env:get(node_connection_attempts, ?APP_NAME, 90),
                    retry_delay = onepanel_env:get(node_connection_retry_delay, ?APP_NAME, 1000)},
                S#step{function = reset_node},
                S#step{function = join_cluster}
            ]
    end;

%% Removes given nodes from the current cluster, clears database on each
%% and initializes separate one-node clusters.
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
    get_steps(leave_cluster, Ctx#{hosts => [hosts:self()]});

%% Utility for managing cluster restart, waiting for all onepanel
%% nodes in the cluster to start.
get_steps(wait_for_cluster, _Ctx) ->
    SelfHost = hosts:self(),
    Attempts = application:get_env(?APP_NAME, wait_for_cluster_attempts, 120),
    Delay = application:get_env(?APP_NAME, wait_for_cluster_delay, 5000),
    [
        % this step should pass quickly since presence of nodes
        % is already ensured in onepanel_sup:init/1
        #step{service = name(), function = ensure_all_hosts_available,
            attempts = Attempts, retry_delay = Delay, hosts = [SelfHost]},
        #step{service = name(), function = ensure_node_ready,
            attempts = Attempts, retry_delay = Delay, hosts = get_hosts()}
    ];

get_steps(clear_users, _Ctx) ->
    [#step{module = onepanel_user, function = delete_all, args = [],
        selection = any}];

get_steps(migrate_emergency_passphrase, _Ctx) ->
    [#step{module = emergency_passphrase, function = migrate_from_users, args = [],
        hosts = get_hosts(), selection = any,
        condition = fun(_) -> not emergency_passphrase:is_set() end}];

get_steps(Function, _Ctx) when
    Function == reload_webcert;
    Function == configure ->
    [#step{function = Function}].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sets the node cookie.
%% @end
%%--------------------------------------------------------------------
-spec set_cookie(Ctx :: service:ctx()) -> ok | no_return().
set_cookie(#{cookie := Cookie} = Ctx) ->
    VmArgsFile = service_ctx:get(onepanel_vm_args_file, Ctx),
    erlang:set_cookie(node(), Cookie),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile);

set_cookie(Ctx) ->
    set_cookie(Ctx#{cookie => erlang:get_cookie()}).


%%--------------------------------------------------------------------
%% @doc Stores onepanel configuration.
%% @end
%%--------------------------------------------------------------------
-spec configure(service:ctx()) -> ok.
configure(#{gui_debug_mode := GuiDebugMode} = Ctx) ->
    onepanel_env:set(gui_debug_mode, GuiDebugMode, ?APP_NAME),
    onepanel_env:write([?APP_NAME, gui_debug_mode], GuiDebugMode),
    configure(maps:remove(gui_debug_mode, Ctx));

configure(_Ctx) ->
    ok.


%%--------------------------------------------------------------------
%% @doc Checks whether this erlang node can be connected to the remote cluster
%% node.
%% @end
%%--------------------------------------------------------------------
-spec check_connection(Ctx :: service:ctx()) -> ok.
check_connection(#{cluster_host := ClusterHost}) ->
    ClusterNode = nodes:service_to_node(name(), ClusterHost),
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
    SelfHost = hosts:self(),
    Body = json_utils:encode(#{
        cookie => erlang:get_cookie(),
        clusterHost => onepanel_utils:convert(SelfHost, binary)
    }),
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
    Suffix = "/join_cluster",
    Timeout = service_ctx:get(extend_cluster_timeout, Ctx, integer),
    Opts = https_opts(Timeout),
    Url = build_url(Hostname, ApiVersion, Suffix),

    case http_client:post(Url, Headers, Body, Opts) of
        {ok, ?HTTP_204_NO_CONTENT, _, _} ->
            ?info("Host '~ts' added to the cluster", [Hostname]),
            #{hostname => Hostname};
        {ok, ?HTTP_403_FORBIDDEN, _, _} ->
            ?throw_error(?ERR_NODE_NOT_EMPTY(Hostname));
        {ok, Code, _, RespBody} ->
            ?error("Unexpected response when trying to add node: ~tp ~tp", [Code, RespBody]),
            timer:sleep(?WAIT_FOR_CLUSTER_DELAY),
            extend_cluster(Ctx#{attempts => Attempts - 1});
        {error, _} ->
            ?warning("Failed to connect with '~ts' to extend cluster", [Hostname]),
            timer:sleep(?WAIT_FOR_CLUSTER_DELAY),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end;

extend_cluster(#{address := Address, api_version := _ApiVersion,
    attempts := Attempts} = Ctx) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case get_remote_node_info(Ctx) of
        {ok, Hostname, ClusterType} ->
            extend_cluster(Ctx#{hostname => Hostname});
        {ok, _Hostname, OtherType} ->
            ?throw_error(?ERR_INCOMPATIBLE_NODE(Address, OtherType));
        #error{reason = ?ERR_BAD_NODE} ->
            ?warning("Failed to connect with '~ts' to extend cluster", [Address]),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end.


%%--------------------------------------------------------------------
%% @doc Adds this node to the remote onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Ctx :: service:ctx()) -> ok.
join_cluster(#{cluster_host := ClusterHost}) ->
    Node = node(),
    ClusterNode = nodes:service_to_node(name(), ClusterHost),
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
-spec ensure_node_ready(service:ctx()) -> true | no_return().
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
-spec available_for_clustering() -> boolean().
available_for_clustering() ->
    not emergency_passphrase:is_set() andalso
        length(get_hosts()) =< 1 andalso
        not onepanel_deployment:is_set(?PROGRESS_CLUSTER).


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

    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},

    case http_client:get(Url, Headers, <<>>, Opts) of
        {ok, ?HTTP_200_OK, _, Body} ->
            #{<<"hostname">> := Hostname,
                <<"clusterType">> := ClusterType} = json_utils:decode(Body),
            {ok, Hostname, onepanel_utils:convert(ClusterType, atom)};
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
    Port = https_listener:port(),
    Prefix = https_listener:get_prefix(ApiVersion),
    onepanel_utils:join(["https://", Host, ":", Port, Prefix, Suffix]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns connection opts for clustering requests.
%% @end
%%--------------------------------------------------------------------
-spec https_opts(Timeout :: non_neg_integer()) -> http_client:opts().
https_opts(Timeout) ->
    CaCerts = https_listener:get_cert_chain_pems(),
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
    ClusterType = onepanel_env:get_cluster_type(),
    SModule = service:get_module(ClusterType),
    try
        lists:member(Host, SModule:get_hosts())
    catch
        _:_ ->
            % incorrect during deployment when some services are
            % already deployed but other are not
            false
    end.
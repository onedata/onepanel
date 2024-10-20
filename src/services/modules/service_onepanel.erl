%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains onepanel service management functions.
%%%
%%% In particular, this module handles changes in the cluster's hosts set.
%%% Host (onepanel node) belongs to the cluster if it shares mnesia tables.
%%%
%%% Cluster extension flow is as follows
%%% (assuming oldNode is already configured and newNode is to be added):
%%% 1a. admin --> oldNode: POST /hosts {"address": "newNode"}
%%% 2a. oldNode --> newNode: GET /node # determine full erlang hostname
%%%     newNode --> oldNode: 200 OK {"hostname": "newNode.tld", "clusterType": onezone"}
%%% 3a. oldNode --> newNode: POST /join_cluster {"inviteToken": "TOKEN"}
%%% 4a. newNode --> oldNode: GET /cookie
%%%     oldNode --> newNode: 200 OK COOKIE_BINARY
%%%
%%% Alternatively cluster member can create invite token, which can be used by
%%% other nodes to join cluster. In such a case, the flow is as follows:
%%% 1b. admin --> oldNode: GET /invite_token
%%%     oldNode --> admin: 200 OK {"inviteToken": "TOKEN"}
%%% 2b. admin --> newNode: POST /join_cluster {"inviteToken": "TOKEN"}
%%% 3b. newNode --> oldNode: GET /cookie
%%%     oldNode --> newNode: 200 OK COOKIE_BINARY
%%%
%%% Only a node without emergency passphrase set and services deployed
%%% can join another cluster (see {@link available_for_clustering/0}),
%%% otherwise request (3a or 2b) will be rejected with code 401.
%%% Request (3a or 2b) triggers service_onepanel:join_cluster action at newNode.
%%% newNode changes its cookie to the one fetched from oldNode and discards its
%%% mnesia tables in favor of synchronizing from oldNode.
%%%
%%% In case of batch config (service_onepanel:deploy) the flow is similar,
%%% but the configuration request contains full erlang hostnames
%%% causing the discovery request (2) to be skipped.
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
-export([
    run_on_master_node/1,
    set_cookie/1, fetch_and_set_cookie/1, configure/1, check_connection/1,
    ensure_all_hosts_available/1, init_cluster/1, extend_cluster/1,
    join_cluster/1, reset_node/1, ensure_node_ready/1, reload_webcert/1,
    available_for_clustering/0, is_host_used/1
]).

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
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    SelfHost = hosts:self(),
    ClusterHosts = get_hosts(),
    NewHosts = lists_utils:subtract(Hosts, ClusterHosts),
    Attempts = application:get_env(?APP_NAME, extend_cluster_attempts, 20),
    {ok, InviteToken} = invite_tokens:create(),
    [#step{
        function = extend_cluster, hosts = [SelfHost],
        ctx = Ctx#{
            hostname => NewHost,
            attempts => Attempts,
            invite_token => InviteToken
        }
    } || NewHost <- NewHosts];

get_steps(extend_cluster, Ctx) ->
    {ok, InviteToken} = invite_tokens:create(),
    [
        #step{function = extend_cluster, hosts = [hosts:self()],
            % when the reason of extend_cluster is an explicit request
            % (as opposed to "batch config" action 'deploy'), do not retry
            % - the requesting client should ensure the node is already online.
            ctx = Ctx#{attempts => 1, invite_token => InviteToken}}
    ];

get_steps(init_cluster, _Ctx) ->
    S = #step{hosts = [hosts:self()], verify_hosts = false},
    [
        S#step{function = set_cookie, verify_hosts = false},
        S#step{function = reset_node, verify_hosts = false},
        S#step{function = init_cluster, verify_hosts = false}
    ];

get_steps(join_cluster, #{cluster_host := ClusterHost}) ->
    SelfHost = hosts:self(),
    case {available_for_clustering(), ClusterHost} of
        {_, SelfHost} -> [];
        {false, _} -> throw(?ERROR_NODE_ALREADY_IN_CLUSTER(SelfHost));
        {true, _} ->
            S = #step{hosts = [SelfHost], verify_hosts = false},
            [
                S#step{function = fetch_and_set_cookie},
                S#step{function = check_connection,
                    attempts = onepanel_env:get(node_connection_attempts, ?APP_NAME, 90),
                    retry_delay = onepanel_env:get(node_connection_retry_delay, ?APP_NAME, 1000)},
                S#step{function = reset_node},
                S#step{function = join_cluster},
                #steps{action = import_configuration, ctx = #{
                    reference_host => ClusterHost, hosts => [SelfHost]
                }, verify_hosts = false}
            ]
    end;

%% Removes given nodes from the current cluster, clears database on each
%% and initializes separate one-node clusters.
get_steps(leave_cluster, #{hosts := Hosts}) ->
    [
        #step{function = reset_node, hosts = Hosts, verify_hosts = false},
        #step{function = init_cluster, hosts = Hosts, verify_hosts = false}
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

get_steps(import_configuration, #{reference_host := Host} = Ctx) ->
    ReferenceNode = nodes:service_to_node(?SERVICE_PANEL, Host),
    [
        #step{module = onepanel_env, function = import_generated_from_node,
            args = [?SERVICE_PANEL, ReferenceNode, _SetInRuntime = true]},
        #steps{service = ?SERVICE_LE, action = import_files, ctx = Ctx}
    ];

get_steps(Function, _Ctx) when
    Function == reload_webcert;
    Function == configure
->
    [#step{function = Function}].


%%%===================================================================
%%% API functions
%%%===================================================================

-spec run_on_master_node(fun(() -> boolean())) -> boolean() | no_return().
run_on_master_node(Fun) ->
    % @TODO VFS-6085 rework the master choice when onepanel cluster is resizeable
    case hd(lists:sort(get_nodes())) of
        Self when node() =:= Self ->
            try
                Fun()
            catch Class:Reason:Stacktrace ->
                ?error_exception(Class, Reason, Stacktrace),
                false
            end;
        MasterNode ->
            case rpc:call(MasterNode, ?MODULE, ?FUNCTION_NAME, [Fun]) of
                Result when is_boolean(Result) -> Result
            end
    end.

%%--------------------------------------------------------------------
%% @doc Sets the node cookie.
%% @end
%%--------------------------------------------------------------------
-spec set_cookie(Ctx :: service:step_ctx()) -> ok | no_return().
set_cookie(#{cookie := Cookie}) ->
    VmArgsFile = onepanel_env:get(onepanel_vm_args_file),
    erlang:set_cookie(node(), Cookie),
    onepanel_vm:write("setcookie", Cookie, VmArgsFile);

set_cookie(Ctx) ->
    set_cookie(Ctx#{cookie => erlang:get_cookie()}).


%%--------------------------------------------------------------------
%% @doc Fetches and sets the node cookie.
%% @end
%%--------------------------------------------------------------------
-spec fetch_and_set_cookie(Ctx :: service:step_ctx()) -> ok | no_return().
fetch_and_set_cookie(#{invite_token := InviteToken, cluster_host := ClusterHost}) ->
    Headers = #{?HDR_X_AUTH_TOKEN => InviteToken},
    Suffix = "/cookie",
    Timeout = onepanel_env:get(extend_cluster_timeout),
    Opts = https_opts(Timeout),
    Url = build_url(ClusterHost, Suffix),

    case http_client:get(Url, Headers, <<>>, Opts) of
        {ok, ?HTTP_200_OK, _, Response} ->
            Cookie = binary_to_atom(json_utils:decode(Response), utf8),
            set_cookie(#{cookie => Cookie});
        {ok, ?HTTP_401_UNAUTHORIZED, _, _} ->
            throw(?ERROR_UNAUTHORIZED);
        {ok, ?HTTP_403_FORBIDDEN, _, _} ->
            throw(?ERROR_FORBIDDEN);
        {error, _} = Error ->
            ?warning("Failed to connect with '~ts' to fetch cookie due to: ~tp", [
                ClusterHost, Error
            ]),
            throw(?ERROR_NO_CONNECTION_TO_NEW_NODE(ClusterHost))
    end.


%%--------------------------------------------------------------------
%% @doc Stores onepanel configuration.
%% @end
%%--------------------------------------------------------------------
-spec configure(service:step_ctx()) -> ok.
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
-spec check_connection(Ctx :: service:step_ctx()) -> ok.
check_connection(#{cluster_host := ClusterHost}) ->
    ClusterNode = nodes:service_to_node(name(), ClusterHost),
    pong = net_adm:ping(ClusterNode),
    ok.


%%--------------------------------------------------------------------
%% @doc Initializes onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec init_cluster(Ctx :: service:step_ctx()) -> ok | no_return().
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
-spec extend_cluster(service:step_ctx()) -> #{hostname := binary()} | no_return().
extend_cluster(#{attempts := Attempts} = Ctx) when Attempts =< 0 ->
    NewNode = case Ctx of
        #{hostname := Hostname} -> Hostname;
        #{address := Address} -> Address
    end,
    throw(?ERROR_NO_CONNECTION_TO_NEW_NODE(NewNode));

extend_cluster(#{hostname := Hostname, attempts := Attempts, invite_token := InviteToken} = Ctx) ->
    Body = json_utils:encode(#{inviteToken => InviteToken}),
    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
    Suffix = "/join_cluster",
    Timeout = onepanel_env:get(extend_cluster_timeout),
    Opts = https_opts(Timeout),
    Url = build_url(Hostname, Suffix),

    case http_client:post(Url, Headers, Body, Opts) of
        {ok, ?HTTP_204_NO_CONTENT, _, _} ->
            ?info("Host '~ts' added to the cluster", [Hostname]),
            #{hostname => Hostname};
        {ok, ?HTTP_401_UNAUTHORIZED, _, _} ->
            throw(?ERROR_NODE_ALREADY_IN_CLUSTER(Hostname));
        {ok, ?HTTP_403_FORBIDDEN, _, _} ->
            throw(?ERROR_NODE_ALREADY_IN_CLUSTER(Hostname));
        {ok, Code, _, RespBody} ->
            ?error("Unexpected response when trying to add node: ~tp ~tp", [Code, RespBody]),
            timer:sleep(?WAIT_FOR_CLUSTER_DELAY),
            extend_cluster(Ctx#{attempts => Attempts - 1});
        {error, _} ->
            ?warning("Failed to connect with '~ts' to extend cluster", [Hostname]),
            timer:sleep(?WAIT_FOR_CLUSTER_DELAY),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end;

extend_cluster(#{address := Address, attempts := Attempts} = Ctx) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case get_remote_node_info(Ctx) of
        {ok, Hostname, ClusterType} ->
            extend_cluster(Ctx#{hostname => Hostname});
        {ok, _Hostname, OtherType} ->
            throw(?ERROR_NODE_NOT_COMPATIBLE(Address, OtherType));
        ?ERROR_NO_CONNECTION_TO_NEW_NODE(_) ->
            ?warning("Failed to connect with '~ts' to extend cluster", [Address]),
            extend_cluster(Ctx#{attempts => Attempts - 1})
    end.


%%--------------------------------------------------------------------
%% @doc Adds this node to the remote onepanel cluster.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Ctx :: service:step_ctx()) -> ok.
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
-spec reset_node(Ctx :: service:step_ctx()) -> ok | no_return().
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
-spec ensure_all_hosts_available(service:step_ctx()) -> ok | no_return().
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
-spec ensure_node_ready(service:step_ctx()) -> true | no_return().
ensure_node_ready(_Ctx) ->
    Counts = supervisor:count_children(onepanel_sup),
    true = (proplists:get_value(specs, Counts) == proplists:get_value(active, Counts)).


%%--------------------------------------------------------------------
%% @doc
%% Ensures certificates changed on disk are updated in listeners.
%% @end
%%--------------------------------------------------------------------
-spec reload_webcert(service:step_ctx()) -> ok.
reload_webcert(_Ctx) ->
    https_listener:reload_web_certs().


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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if given host is used to run oneprovider or onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_host_used(service:host()) -> boolean().
is_host_used(Host) ->
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
-spec get_remote_node_info(service:step_ctx()) ->
    {ok, Hostname :: binary(), Application :: atom()} | {error, _} | no_return().
get_remote_node_info(#{address := Address}) ->
    Timeout = onepanel_env:get(extend_cluster_timeout),
    Opts = https_opts(Timeout),
    Suffix = <<"/node">>,
    Url = build_url(Address, Suffix),

    Headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},

    case http_client:get(Url, Headers, <<>>, Opts) of
        {ok, ?HTTP_200_OK, _, Body} ->
            #{<<"hostname">> := Hostname,
                <<"clusterType">> := ClusterType} = json_utils:decode(Body),
            {ok, Hostname, onepanel_utils:convert(ClusterType, atom)};
        {error, _} -> ?ERROR_NO_CONNECTION_TO_NEW_NODE(Address)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds url for given onepanel REST endpoint
%% @end
%%--------------------------------------------------------------------
-spec build_url(Host :: binary() | string(),
    Suffix :: binary() | string()) -> binary().
build_url(Host, Suffix) ->
    Port = https_listener:port(),
    Prefix = https_listener:get_prefix(),
    onepanel_utils:join(["https://", Host, ":", Port, Prefix, Suffix]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns connection opts for clustering requests.
%% @end
%%--------------------------------------------------------------------
-spec https_opts(Timeout :: time:millis()) -> http_client:opts().
https_opts(Timeout) ->
    CaCerts = cert_utils:load_ders_in_dir(onepanel_env:get(cacerts_dir)),
    [
        {ssl_options, [{secure, false}, {cacerts, CaCerts}]},
        {connect_timeout, Timeout},
        {recv_timeout, Timeout}
    ].

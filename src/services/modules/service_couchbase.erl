%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains couchbase service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_couchbase).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").
-include("service.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, start/1, wait_for_init/1, stop/1, status/1,
    init_cluster/1, join_cluster/1, rebalance_cluster/1]).

-define(INIT_SCRIPT, "couchbase-server").
-define(CLI, "LC_ALL=en_US.UTF-8 /opt/couchbase/bin/couchbase-cli").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    couchbase.


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
    service:get_nodes(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    service:create(#service{name = name()}),
    ClusterHosts = get_hosts(),
    NewHosts = onepanel_lists:subtract(Hosts, ClusterHosts),
    AllHosts = onepanel_lists:union(ClusterHosts, NewHosts),
    [
        #step{hosts = NewHosts, function = configure},
        #step{hosts = AllHosts, function = start},
        #step{hosts = AllHosts, function = wait_for_init},
        #step{hosts = NewHosts, function = init_cluster, selection = first,
            condition = fun(_) -> ClusterHosts == [] end
        },
        #step{hosts = NewHosts, function = join_cluster, selection = rest,
            ctx = Ctx#{cluster_host => onepanel_lists:hd(NewHosts)},
            condition = fun(_) -> ClusterHosts == [] end
        },
        #step{hosts = NewHosts, function = join_cluster,
            ctx = Ctx#{cluster_host => onepanel_lists:hd(ClusterHosts)},
            condition = fun(_) -> ClusterHosts /= [] end
        },
        #step{hosts = AllHosts, function = rebalance_cluster, selection = first}
    ];

get_steps(resume, _Ctx) ->
    [
        #step{function = start},
        #step{function = wait_for_init}
    ];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(_Ctx) ->
    onepanel_shell:sed("-community", "", "/etc/init.d/" ++ ?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    Limits = #{
        open_files => service_ctx:get(couchbase_open_files_limit, Ctx)
    },
    service_cli:start(name(), Limits),
    service_watcher:register_service(name()),
    % update status cache
    status(Ctx),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_watcher:unregister_service(name()),
    service_cli:stop(name()),
    % update status cache
    status(Ctx),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> service:status().
status(Ctx) ->
    service:update_status(name(),
        case service_cli:status(name(), status) of
            running -> health(Ctx);
            stopped -> stopped;
            missing -> missing
        end).


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
health(Ctx) ->
    ConnectTimeout = service_ctx:get(couchbase_connect_timeout, Ctx, integer),
    Host = onepanel_cluster:node_to_host(),
    Port = service_ctx:get(couchbase_admin_port, Ctx, integer),

    case gen_tcp:connect(Host, Port, [], ConnectTimeout) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            healthy;
        {error, Reason} ->
            ?warning("Cannot connect to couchbase server (~s:~p) due to: "
            "~p", [Host, Port, Reason]),
            unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc Waits for the service initialization.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    StartAttempts = service_ctx:get(couchbase_wait_for_init_attempts, Ctx, integer),
    onepanel_utils:wait_until(?MODULE, status, [Ctx],
        {equal, healthy}, StartAttempts),

    % Couchbase reports healthy status before it's ready to serve requests.
    % This delay provides additional margin of error before starting workers.
    Delay = application:get_env(onepanel, couchbase_init_delay, 0),
    timer:sleep(Delay).


%%--------------------------------------------------------------------
%% @doc Initializes the service cluster.
%% @end
%%--------------------------------------------------------------------
-spec init_cluster(Ctx :: service:ctx()) -> ok | no_return().
init_cluster(Ctx) ->
    User = service_ctx:get(couchbase_user, Ctx),
    Password = service_ctx:get(couchbase_password, Ctx),
    ServerQuota = service_ctx:get(couchbase_server_quota, Ctx),
    BucketQuota = service_ctx:get(couchbase_bucket_quota, Ctx, integer),
    Host = onepanel_cluster:node_to_host(),
    Port = service_ctx:get(couchbase_admin_port, Ctx),
    Url = onepanel_utils:join(["http://", Host, ":", Port, "/pools/default"]),
    Timeout = service_ctx:get(couchbase_init_timeout, Ctx, integer),

    {ok, 200, _, _} = http_client:post(
        Url, maps:from_list([
            onepanel_utils:get_basic_auth_header(User, Password),
            {"content-type", "application/x-www-form-urlencoded"}
        ]), "memoryQuota=" ++ ServerQuota,
        [{connect_timeout, Timeout}, {recv_timeout, Timeout}]
    ),

    Cmd = [?CLI, "cluster-init", "-c", Host ++ ":" ++ Port,
            "--cluster-init-username=" ++ User,
            "--cluster-init-ramsize=" ++ ServerQuota],
    onepanel_shell:ensure_success(
        Cmd ++ ["--cluster-init-password=" ++ Password],
        Cmd ++ ["--cluster-init-password=*****"]),

    Release = onepanel_env:get(release_type),
    {ok, Buckets} = onepanel_lists:get(Release, onepanel_env:get(couchbase_buckets)),
    lists:foreach(fun
        ({Bucket, Quota}) ->
            create_bucket(Host, Port, User, Password, Bucket, Quota);
        (Bucket) ->
            create_bucket(Host, Port, User, Password, Bucket, BucketQuota)
    end, Buckets),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc Adds this host to the service cluster.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Ctx :: service:ctx()) -> ok | no_return().
join_cluster(#{cluster_host := ClusterHost} = Ctx) ->
    User = service_ctx:get(couchbase_user, Ctx),
    Password = service_ctx:get(couchbase_password, Ctx),
    Host = onepanel_cluster:node_to_host(),
    Port = service_ctx:get(couchbase_admin_port, Ctx),

    Cmd = [?CLI, "server-add", "-c",
            ClusterHost ++ ":" ++ Port, "-u", User, "-p", Password,
            "--server-add=" ++ Host ++ ":" ++ Port,
            "--server-add-username=" ++ User
    ],
    onepanel_shell:ensure_success(
        Cmd ++ ["--server-add-password=" ++ Password],
        Cmd ++ ["--server-add-password=*****"]
    ),

    service:add_host(name(), Host).


%%--------------------------------------------------------------------
%% @doc Rebalances the service cluster.
%% @end
%%--------------------------------------------------------------------
-spec rebalance_cluster(Ctx :: service:ctx()) -> ok | no_return().
rebalance_cluster(Ctx) ->
    User = service_ctx:get(couchbase_user, Ctx),
    Password = service_ctx:get(couchbase_password, Ctx),
    Host = onepanel_cluster:node_to_host(),
    Port = service_ctx:get(couchbase_admin_port, Ctx),

    Cmd = [?CLI, "rebalance", "-c", Host ++ ":" ++ Port, "-u", User],
    onepanel_shell:ensure_success(
        Cmd ++ ["-p", Password],
        Cmd ++ ["-p", "*****"]
    ),

    % Couchbase reports healthy status before it's ready to serve requests.
    % This delay provides additional margin of error before starting workers.
    Delay = application:get_env(onepanel, couchbase_init_delay, 0),
    timer:sleep(Delay).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc
%% Creates Couchbase bucket.
%% @end
%%--------------------------------------------------------------------
-spec create_bucket(Host :: string(), Port :: string(), User :: string(),
    Password :: string(), Bucket :: string(), BucketQuota :: integer()) ->
    ok | no_return().
create_bucket(Host, Port, User, Password, Bucket, BucketQuota) ->
    Cmd = [?CLI, "bucket-create", "-c", Host ++ ":" ++ Port,
        "-u", User, "--bucket=" ++ Bucket,
        "--bucket-ramsize=" ++ onepanel_utils:convert(BucketQuota, list),
        "--bucket-eviction-policy=fullEviction", "--wait"],
    onepanel_shell:ensure_success(
        Cmd ++ ["-p", Password],
        Cmd ++ ["-p", "****"]).

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
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include("modules/models.hrl").
-include("service.hrl").

% @formatter:off
-type model_ctx() :: #{
    %% Caches (i.e. not the primary source of truth):
    % service status cache
    status => #{service:host() => service:status()}
}.
% @formatter:on

-export_type([model_ctx/0]).


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
    nodes:all(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    service:create(#service{name = name()}),
    ClusterHosts = get_hosts(),
    NewHosts = lists_utils:subtract(Hosts, ClusterHosts),
    AllHosts = lists_utils:union(ClusterHosts, NewHosts),
    [
        #step{hosts = NewHosts, function = configure},
        #step{hosts = AllHosts, function = start},
        #step{hosts = AllHosts, function = wait_for_init},
        #step{hosts = NewHosts, function = init_cluster, selection = first,
            condition = fun(_) -> ClusterHosts == [] end
        },
        #step{hosts = NewHosts, function = join_cluster, selection = rest,
            ctx = Ctx#{cluster_host => lists_utils:hd(NewHosts)},
            condition = fun(_) -> ClusterHosts == [] end
        },
        #step{hosts = NewHosts, function = join_cluster,
            ctx = Ctx#{cluster_host => lists_utils:hd(ClusterHosts)},
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
-spec configure(Ctx :: service:step_ctx()) -> ok | no_return().
configure(_Ctx) ->
    onepanel_shell:sed("-community", "", "/etc/init.d/" ++ ?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:step_ctx()) -> ok | no_return().
start(Ctx) ->
    Limits = #{
        open_files => onepanel_env:get(couchbase_open_files_limit)
    },
    service_cli:start(name(), Limits),
    % update status cache
    status(Ctx),
    service:register_healthcheck(name(), #{hosts => [hosts:self()]}),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:step_ctx()) -> ok | no_return().
stop(Ctx) ->
    service:deregister_healthcheck(name(), Ctx),
    service_cli:stop(name()),
    % update status cache
    status(Ctx),
    ok.


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:step_ctx()) -> service:status().
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
-spec health(service:step_ctx()) -> service:status().
health(_Ctx) ->
    ConnectTimeout = onepanel_env:get(couchbase_connect_timeout),
    Host = hosts:self(),
    Port = onepanel_env:get(couchbase_admin_port),

    case gen_tcp:connect(Host, Port, [], ConnectTimeout) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            healthy;
        {error, Reason} ->
            ?warning("Cannot connect to couchbase server (~ts:~tp) due to: "
            "~tp", [Host, Port, Reason]),
            unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc Waits for the service initialization.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:step_ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    StartAttempts = onepanel_env:get(couchbase_wait_for_init_attempts),
    try
        onepanel_utils:wait_until(?MODULE, status, [Ctx],
            {equal, healthy}, StartAttempts)
    catch throw:attempts_limit_exceeded ->
        % Couchbase sometimes dies silently. Restart it once in such case
        % to reduce impact of this issue.
        ?warning("Wait for couchbase to come up timed out. "
            "Attempting restart of couchbase server."),
        service_cli:restart(name()),

        onepanel_utils:wait_until(?MODULE, status, [Ctx],
            {equal, healthy}, StartAttempts)
    end,

    service:register_healthcheck(name(), #{hosts => [hosts:self()]}),
    % Couchbase reports healthy status before it's ready to serve requests.
    % This delay provides additional margin of error before starting workers.
    Delay = application:get_env(onepanel, couchbase_after_init_delay, 0),
    timer:sleep(Delay).


%%--------------------------------------------------------------------
%% @doc Initializes the service cluster.
%% @end
%%--------------------------------------------------------------------
-spec init_cluster(Ctx :: service:step_ctx()) -> ok | no_return().
init_cluster(Ctx) ->
    User = onepanel_env:typed_get(couchbase_user, list),
    Password = onepanel_env:typed_get(couchbase_password, list),
    DefaultServerQuota = onepanel_env:typed_get(couchbase_server_quota, integer),
    ServerQuota = onepanel_utils:get_converted(
        couchbase_server_quota, Ctx, integer, DefaultServerQuota
    ),
    DefaultBucketQuota = onepanel_env:typed_get(couchbase_bucket_quota, integer),
    BucketQuota = onepanel_utils:get_converted(
        couchbase_bucket_quota, Ctx, integer, DefaultBucketQuota
    ),
    Host = hosts:self(),
    Port = onepanel_env:typed_get(couchbase_admin_port, list),
    Url = onepanel_utils:join(["http://", Host, ":", Port, "/pools/default"]),
    Timeout = onepanel_env:get(couchbase_init_timeout),
    Headers = maps:merge(
        onepanel_utils:get_basic_auth_header(User, Password),
        #{?HDR_CONTENT_TYPE => "application/x-www-form-urlencoded"}
    ),
    Body = str_utils:format("memoryQuota=~B", [ServerQuota]),

    {ok, ?HTTP_200_OK, _, _} = http_client:post(
        Url, Headers, Body,
        [{connect_timeout, Timeout}, {recv_timeout, Timeout}]
    ),

    Cmd = [?CLI, "cluster-init", "-c", Host ++ ":" ++ Port,
        str_utils:format("--cluster-init-username=~ts", [User]),
        str_utils:format("--cluster-init-ramsize=~B", [ServerQuota])
    ],
    onepanel_shell:ensure_success(
        Cmd ++ ["--cluster-init-password=" ++ Password],
        Cmd ++ ["--cluster-init-password=*****"]),

    ClusterType = onepanel_env:get_cluster_type(),
    Buckets = kv_utils:get(ClusterType, onepanel_env:get(couchbase_buckets)),
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
-spec join_cluster(Ctx :: service:step_ctx()) -> ok | no_return().
join_cluster(#{cluster_host := ClusterHost}) ->
    User = onepanel_env:get(couchbase_user),
    Password = onepanel_env:get(couchbase_password),
    Host = hosts:self(),
    Port = onepanel_env:typed_get(couchbase_admin_port, list),

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
-spec rebalance_cluster(Ctx :: service:step_ctx()) -> ok | no_return().
rebalance_cluster(_Ctx) ->
    User = onepanel_env:get(couchbase_user),
    Password = onepanel_env:get(couchbase_password),
    Host = hosts:self(),
    Port = onepanel_env:typed_get(couchbase_admin_port, list),

    Cmd = [?CLI, "rebalance", "-c", Host ++ ":" ++ Port, "-u", User],
    onepanel_shell:ensure_success(
        Cmd ++ ["-p", Password],
        Cmd ++ ["-p", "*****"]
    ),

    % Couchbase reports healthy status before it's ready to serve requests.
    % This delay provides additional margin of error before starting workers.
    Delay = application:get_env(onepanel, couchbase_after_init_delay, 0),
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
        str_utils:format("--bucket-ramsize=~B", [BucketQuota]),
        "--bucket-eviction-policy=fullEviction", "--wait"],
    onepanel_shell:ensure_success(
        Cmd ++ ["-p", Password],
        Cmd ++ ["-p", "****"]
    ).

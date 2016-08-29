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
-include("modules/logger.hrl").
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
    [
        #step{module = service, function = save,
            args = [#service{name = name()}], selection = first},
        #step{function = configure},
        #step{function = start},
        #step{function = wait_for_init},
        #step{function = init_cluster, selection = first},
        #step{function = join_cluster, selection = rest,
            ctx = Ctx#{cluster_host => hd(Hosts)}},
        #step{function = rebalance_cluster, selection = first}
    ];

get_steps(start, _Ctx) ->
    [#step{function = start}];

get_steps(stop, _Ctx) ->
    [#step{function = stop}];

get_steps(restart, _Ctx) ->
    [#step{function = stop}, #step{function = start}];

get_steps(status, _Ctx) ->
    [#step{function = status}];

get_steps(Action, _Ctx) ->
    ?throw({action_not_supported, Action}).

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
%% @doc {@link service:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    service:start(?INIT_SCRIPT, #{
        open_files => service_ctx:get(couchbase_open_files_limit, Ctx)
    }).


%%--------------------------------------------------------------------
%% @doc Waits for the service initialization.
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    StartAttempts = service_ctx:get(couchbase_wait_for_init_attempts, Ctx, integer),
    onepanel_utils:wait_until(?MODULE, status, [Ctx], {equal, running},
        StartAttempts),

    ConnectAttempts = service_ctx:get(couchbase_connect_attempts, Ctx, integer),
    ConnectTimeout = service_ctx:get(couchbase_connect_timeout, Ctx, integer),
    Host = onepanel_cluster:node_to_host(),
    Port = service_ctx:get(couchbase_admin_port, Ctx, integer),
    Validator = fun
        ({ok, Socket}) -> gen_tcp:close(Socket);
        ({error, Reason}) ->
            ?log_warning("Cannot connect to couchbase server (~s:~p) due to: "
            "~p", [Host, Port, Reason]),
            ?throw(Reason)
    end,

    onepanel_utils:wait_until(gen_tcp, connect, [Host, Port, [],
        ConnectTimeout], {validator, Validator}, ConnectAttempts).


%%--------------------------------------------------------------------
%% @doc {@link service:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(_Ctx) ->
    service:stop(?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(_Ctx) ->
    service:status(?INIT_SCRIPT).


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

    {ok, 200, _, _} = http_client:post(
        Url, [
            onepanel_utils:get_basic_auth_header(User, Password),
            {"Content-Type", "application/x-www-form-urlencoded"}
        ], "memoryQuota=" ++ ServerQuota
    ),

    onepanel_shell:check_call([?CLI, "cluster-init", "-c", Host ++ ":" ++ Port,
            "--cluster-init-username=" ++ User,
            "--cluster-init-password=" ++ Password,
            "--cluster-init-ramsize=" ++ ServerQuota,
            "--services=data,index,query"]),

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

    onepanel_shell:check_call([?CLI, "server-add", "-c",
            ClusterHost ++ ":" ++ Port, "-u", User, "-p", Password,
            "--server-add=" ++ Host ++ ":" ++ Port,
            "--server-add-username=" ++ User,
            "--server-add-password=" ++ Password,
            "--services=data,index,query"]),

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

    onepanel_shell:check_call([?CLI, "rebalance", "-c", Host ++ ":" ++ Port,
        "-u", User, "-p", Password]).

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
    onepanel_shell:check_call([?CLI, "bucket-create", "-c", Host ++ ":" ++ Port,
        "-u", User, "-p", Password, "--bucket=" ++ Bucket,
        "--bucket-ramsize=" ++ onepanel_utils:convert(BucketQuota, list),
        "--wait"]).
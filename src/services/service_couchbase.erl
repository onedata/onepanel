%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_couchbase).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("db/models.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([get_steps/2]).

%% API
-export([name/0, configure/1, start/1, wait_for_start/1, stop/1, status/1,
    init_cluster/1, join_cluster/1, rebalance_cluster/1]).

-define(NAME, couchbase).
-define(INIT_SCRIPT, "couchbase-server").
-define(CLI, "LC_ALL=en_US.UTF-8 /opt/couchbase/bin/couchbase-cli").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy, #{hosts := Hosts} = Ctx) ->
    [
        #step{function = configure},
        #step{function = start},
        #step{function = wait_for_start},
        #step{hosts = [hd(Hosts)], function = init_cluster},
        #step{hosts = tl(Hosts), function = join_cluster,
            ctx = Ctx#{cluster_host => hd(Hosts)}},
        #step{hosts = [hd(Hosts)], function = rebalance_cluster}
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
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?NAME.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(_Ctx) ->
    service:create(#service{name = ?NAME}),
    onepanel_shell:sed("-community", "", "/etc/init.d/" ++ ?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc @see service:start/1
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    service:start(?INIT_SCRIPT, #{
        open_files => service:param(couchbase_open_files_limit, Ctx),
        processes => service:param(couchbase_processes_limit, Ctx)
    }).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec wait_for_start(Ctx :: service:ctx()) -> ok | no_return().
wait_for_start(Ctx) ->
    StartAttempts = service:param(couchbase_start_attempts, Ctx),
    onepanel_utils:wait_until(?MODULE, status, [Ctx], {equal, ok},
        StartAttempts),

    ConnectAttempts = service:param(couchbase_connect_attempts, Ctx),
    ConnectTimeout = service:param(couchbase_connect_timeout, Ctx),
    Host = onepanel_utils:node_to_host(),
    Port = service:param(couchbase_admin_port, Ctx),
    Validator = fun
        ({ok, Socket}) -> gen_tcp:close(Socket);
        ({error, Reason}) ->
            ?warning("Cannot connect to couchbase server (~s:~p) due to: ~p",
                [Host, Port, Reason]),
            throw(Reason)
    end,

    onepanel_utils:wait_until(gen_tcp, connect, [Host, Port, [],
        ConnectTimeout], {validator, Validator}, ConnectAttempts).


%%--------------------------------------------------------------------
%% @doc @see service:stop/1
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(_Ctx) ->
    service:stop(?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc @see service:status/1
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> ok | no_return().
status(_Ctx) ->
    service:status(?INIT_SCRIPT).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec init_cluster(Ctx :: service:ctx()) -> ok | no_return().
init_cluster(Ctx) ->
    User = service:param(couchbase_user, Ctx),
    Password = service:param(couchbase_password, Ctx),
    Authorization = basic_authorization(User, Password),
    Quota = erlang:integer_to_list(service:param(couchbase_memory_quota, Ctx)),
    Host = onepanel_utils:node_to_host(),
    Port = erlang:integer_to_list(service:param(couchbase_admin_port, Ctx)),
    HostAndPort = Host ++ ":" ++ Port,

    {ok, 200, _, _} = http_client:post(
        "http://" ++ HostAndPort ++ "/pools/default", [
            {"Authorization", Authorization},
            {"Content-Type", "application/x-www-form-urlencoded"}
        ], "memoryQuota=" ++ Quota
    ),

    onepanel_shell:check_call([?CLI, "cluster-init", "-c", HostAndPort,
            "--cluster-init-username=" ++ User,
            "--cluster-init-password=" ++ Password,
            "--cluster-init-ramsize=" ++ Quota, "--services=data,index,query"]),

    onepanel_shell:check_call([?CLI, "bucket-create", "-c", HostAndPort,
            "-u", User, "-p", Password, "--bucket=default",
            "--bucket-ramsize=" ++ Quota, "--wait"]),

    service:add_host(?NAME, Host).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec join_cluster(Ctx :: service:ctx()) -> ok | no_return().
join_cluster(#{cluster_host := ClusterHost} = Ctx) ->
    User = service:param(couchbase_user, Ctx),
    Password = service:param(couchbase_password, Ctx),
    Host = onepanel_utils:node_to_host(),
    Port = erlang:integer_to_list(service:param(couchbase_admin_port, Ctx)),

    onepanel_shell:check_call([?CLI, "server-add", "-c",
            ClusterHost ++ ":" ++ Port, "-u", User, "-p", Password,
            "--server-add=" ++ Host ++ ":" ++ Port,
            "--server-add-username=" ++ User,
            "--server-add-password=" ++ Password,
            "--services=data,index,query"]),

    service:add_host(?NAME, Host).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec rebalance_cluster(Ctx :: service:ctx()) -> ok | no_return().
rebalance_cluster(Ctx) ->
    User = service:param(couchbase_user, Ctx),
    Password = service:param(couchbase_password, Ctx),
    Host = onepanel_utils:node_to_host(),
    Port = erlang:integer_to_list(service:param(couchbase_admin_port, Ctx)),

    onepanel_shell:check_call([?CLI, "rebalance", "-c", Host ++ ":" ++ Port,
        "-u", User, "-p", Password]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec basic_authorization(User :: string(), Password :: string()) ->
    Authorization :: string().
basic_authorization(User, Password) ->
    Hash = base64:encode(string:join([User, Password], ":")),
    "Basic " ++ erlang:binary_to_list(Hash).

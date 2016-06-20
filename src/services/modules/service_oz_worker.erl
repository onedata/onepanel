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
-module(service_oz_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/logger.hrl").
-include("modules/models.hrl").

%% Service behaviour callbacks
-export([name/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1,
    nagios_report/1]).

-define(INIT_SCRIPT, "oz_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:name/0
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    oz_worker.


%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx#{name => name()}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(#{main_cm_host := MainCmHost, cm_hosts := CmHosts,
    db_hosts := DbHosts} = Ctx) ->

    AppConfigPath = service_ctx:get(oz_worker_app_config_path, Ctx),
    VmArgsPath = service_ctx:get(oz_worker_vm_args_path, Ctx),
    OzName = service_ctx:get(onezone_name, Ctx),
    OzDomain = service_ctx:get_domain(onezone_domain, Ctx),
    CmNodes = onepanel_cluster:hosts_to_nodes(
        service_cluster_manager:name(),
        [MainCmHost | lists:delete(MainCmHost, CmHosts)]
    ),
    DbPort = erlang:integer_to_list(service_ctx:get(couchbase_port, Ctx)),
    DbNodes = lists:map(fun(Host) ->
        erlang:binary_to_atom(onepanel_utils:join([Host, DbPort], <<":">>), utf8)
    end, DbHosts),

    service_cluster_worker:configure(Ctx#{
        name => name(),
        cm_nodes => CmNodes,
        db_nodes => DbNodes,
        app_config => #{
            oz_name => OzName,
            http_domain => OzDomain
        },
        app_config_path => AppConfigPath,
        vm_args_path => VmArgsPath
    }).


%%--------------------------------------------------------------------
%% @doc @see service:start/1
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(oz_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc @see service:stop/1
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc @see service:status/1
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> ok | no_return().
status(Ctx) ->
    service_cluster_worker:status(Ctx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc @see service_cluster_worker:wait_for_init/1
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => service_ctx:get(
            oz_worker_wait_for_init_attempts, Ctx),
        wait_for_init_delay => service_ctx:get(op_worker_wait_for_init_delay, Ctx)
    }).


%%--------------------------------------------------------------------
%% @doc @see service_cluster_worker:nagios_report/1
%%--------------------------------------------------------------------
-spec nagios_report(Ctx :: service:ctx()) -> ok | no_return().
nagios_report(Ctx) ->
    service_cluster_worker:nagios_report(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx)
    }).
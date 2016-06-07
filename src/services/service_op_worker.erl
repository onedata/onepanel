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
-module(service_op_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("db/models.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([get_steps/2]).

%% API
-export([name/0, configure/1, start/1, stop/1, status/1, wait_for_init/1,
    nagios_report/1]).

-define(NAME, op_worker).
-define(INIT_SCRIPT, "op_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see service_behaviour:get_steps/2
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx).

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
configure(#{main_cm_host := MainCmHost, cm_hosts := CmHosts,
    db_hosts := DbHosts} = Ctx) ->

    AppConfigPath = service:param(op_worker_app_config_path, Ctx),
    VmArgsPath = service:param(op_worker_vm_args_path, Ctx),
    OpDomain = service:domain(oneprovider_domain, Ctx),
    CmNodes = onepanel_utils:hosts_to_nodes(
        service_cluster_manager:name(),
        [MainCmHost | lists:delete(MainCmHost, CmHosts)]
    ),
    DbPort = erlang:integer_to_list(service:param(couchbase_port, Ctx)),
    DbNodes = [erlang:list_to_atom(Host ++ ":" ++ DbPort) || Host <- DbHosts],

    service_cluster_worker:configure(Ctx#{
        name => ?NAME,
        cm_nodes => CmNodes,
        db_nodes => DbNodes,
        app_config => #{
            provider_domain => OpDomain
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
        open_files => service:param(op_worker_open_files_limit, Ctx)
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
        name => ?NAME,
        wait_for_init_attempts => service:param(
            op_worker_wait_for_init_attempts, Ctx),
        wait_for_init_delay => service:param(op_worker_wait_for_init_delay, Ctx)
    }).


%%--------------------------------------------------------------------
%% @doc @see service_cluster_worker:nagios_report/1
%%--------------------------------------------------------------------
-spec nagios_report(Ctx :: service:ctx()) -> Status :: atom().
nagios_report(Ctx) ->
    service_cluster_worker:nagios_report(Ctx#{
        nagios_protocol => service:param(op_worker_nagios_protocol, Ctx),
        nagios_port => service:param(op_worker_nagios_port, Ctx)
    }).

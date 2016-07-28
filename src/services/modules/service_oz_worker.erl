%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains oz_worker service management functions.
%%%--------------------------------------------------------------------
-module(service_oz_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("modules/logger.hrl").
-include("modules/models.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1,
    nagios_report/1]).

-define(INIT_SCRIPT, "oz_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    oz_worker.


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_hosts/0}
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%%--------------------------------------------------------------------
-spec get_nodes() -> Nodes :: [node()].
get_nodes() ->
    service:get_nodes(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx#{name => name()}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(Ctx) ->
    AppConfigPath = service_ctx:get(oz_worker_app_config_path, Ctx),
    VmArgsPath = service_ctx:get(oz_worker_vm_args_path, Ctx),
    OzName = service_ctx:get(onezone_name, Ctx),
    OzDomain = service_ctx:get_domain(onezone_domain, Ctx),

    service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => #{
            oz_name => OzName,
            http_domain => OzDomain
        },
        app_config_path => AppConfigPath,
        vm_args_path => VmArgsPath
    }).


%%--------------------------------------------------------------------
%% @doc {@link service:start/1}
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(oz_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc {@link service:stop/1}
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(Ctx) ->
    service_cluster_worker:status(Ctx#{init_script => ?INIT_SCRIPT}).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:wait_for_init/1}
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => service_ctx:get(
            oz_worker_wait_for_init_attempts, Ctx, integer),
        wait_for_init_delay => service_ctx:get(
            op_worker_wait_for_init_delay, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:nagios_report/1}
%%--------------------------------------------------------------------
-spec nagios_report(Ctx :: service:ctx()) -> ok | no_return().
nagios_report(Ctx) ->
    service_cluster_worker:nagios_report(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx, integer)
    }).
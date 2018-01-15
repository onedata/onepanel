%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains op_worker service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_op_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).

-include("service.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([configure/1, setup_certs/1, start/1, stop/1, status/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1, add_storages/1, get_storages/1,
    update_storage/1]).

-define(INIT_SCRIPT, "op_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    op_worker.


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
get_steps(add_storages, #{hosts := Hosts, storages := _} = Ctx) ->
    [#step{hosts = Hosts, function = add_storages, selection = first, ctx = Ctx}];

get_steps(add_storages, #{storages := _} = Ctx) ->
    get_steps(add_storages, Ctx#{hosts => get_hosts()});

get_steps(add_storages, _Ctx) ->
    [];

get_steps(get_storages, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = get_storages, selection = any}];

get_steps(get_storages, Ctx) ->
    get_steps(get_storages, Ctx#{hosts => get_hosts()});

get_steps(update_storage, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = update_storage, selection = any}];

get_steps(update_storage, Ctx) ->
    get_steps(update_storage, Ctx#{hosts => get_hosts()});

get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx#{name => name()}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(Ctx) ->
    AppConfigFile = service_ctx:get(op_worker_app_config_file, Ctx),
    VmArgsFile = service_ctx:get(op_worker_vm_args_file, Ctx),

    service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => #{},
        app_config_file => AppConfigFile,
        vm_args_file => VmArgsFile
    }).


%%--------------------------------------------------------------------
%% @doc Setup service certificates using onepanel certificates.
%% @end
%%--------------------------------------------------------------------
-spec setup_certs(Ctx :: service:ctx()) -> ok | no_return().
setup_certs(Ctx) ->
    lists:foreach(fun({Src, Dst}) ->
        {ok, _} = file:copy(service_ctx:get(Src, Ctx), service_ctx:get(Dst, Ctx))
    end, [
        {key_file, op_worker_web_key_file},
        {cert_file, op_worker_web_cert_file}
    ]).


%%--------------------------------------------------------------------
%% @doc {@link service:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(op_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{
        init_script => ?INIT_SCRIPT,
        custom_cmd_env => op_worker_start_cmd
    }),
    service_watcher:register_service(name()).


%%--------------------------------------------------------------------
%% @doc {@link service:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_watcher:unregister_service(name()),
    service_cluster_worker:stop(Ctx#{
        init_script => ?INIT_SCRIPT,
        custom_cmd_env => op_worker_stop_cmd
    }).


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(Ctx) ->
    service_cluster_worker:status(Ctx#{
        init_script => ?INIT_SCRIPT,
        custom_cmd_env => op_worker_status_cmd
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:wait_for_init/1}
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => service_ctx:get(
            op_worker_wait_for_init_attempts, Ctx, integer),
        wait_for_init_delay => service_ctx:get(
            op_worker_wait_for_init_delay, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_response/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:ctx()) ->
    Response :: http_client:response().
get_nagios_response(Ctx) ->
    service_cluster_worker:get_nagios_response(Ctx#{
        nagios_protocol => service_ctx:get(op_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(op_worker_nagios_port, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_status/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    service_cluster_worker:get_nagios_status(Ctx#{
        nagios_protocol => service_ctx:get(op_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(op_worker_nagios_port, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc Configures the service storages.
%% @end
%%--------------------------------------------------------------------
-spec add_storages(Ctx :: service:ctx()) -> ok | no_return().
add_storages(#{storages := Storages, ignore_exists := IgnoreExists}) ->
    op_worker_storage:add(Storages, IgnoreExists);

add_storages(Ctx) ->
    add_storages(Ctx#{ignore_exists => false}).


%%--------------------------------------------------------------------
%% @doc Returns a list of the configured service storages.
%% @end
%%--------------------------------------------------------------------
-spec get_storages(Ctx :: service:ctx()) -> list().
get_storages(#{id := Id}) ->
    op_worker_storage:get(Id);

get_storages(_Ctx) ->
    op_worker_storage:get().


%%--------------------------------------------------------------------
%% @doc Configuration details of the service storage.
%% @end
%%--------------------------------------------------------------------
-spec update_storage(Ctx :: service:ctx()) -> ok | no_return().
update_storage(#{id := Id, args := Args}) ->
    op_worker_storage:update(Id, Args).

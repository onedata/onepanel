%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains oz_worker service management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_oz_worker).
-author("Krzysztof Trzepla").
-behaviour(service_behaviour).
-behaviour(letsencrypt_plugin_behaviour).

-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).
%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, reload_webcert/1, get_domain/1,
get_admin_email/1, is_letsencrypt_supported/1]).

%% API
-export([configure/1, start/1, stop/1, status/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1]).
-export([reconcile_dns/1]).

-define(INIT_SCRIPT, "oz_worker").

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    oz_worker.


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
    AppConfigFile = service_ctx:get(oz_worker_app_config_file, Ctx),
    VmArgsFile = service_ctx:get(oz_worker_vm_args_file, Ctx),
    OzName = service_ctx:get(onezone_name, Ctx),
    OzDomain = service_ctx:get_domain(onezone_domain, Ctx),

    % TODO VFS-4140 Mark IPs configured only in batch mode
    service_cluster_worker:mark_cluster_ips_configured(name()),

    service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => #{
            oz_name => OzName,
            http_domain => OzDomain
        },
        app_config_file => AppConfigFile,
        vm_args_file => VmArgsFile,
        initialize_ip => true
    }).


%%--------------------------------------------------------------------
%% @doc {@link service:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(oz_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{
        init_script => ?INIT_SCRIPT,
        custom_cmd_env => oz_worker_start_cmd
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
        custom_cmd_env => oz_worker_stop_cmd
    }).


%%--------------------------------------------------------------------
%% @doc {@link service:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> running | stopped | not_found.
status(Ctx) ->
    service_cluster_worker:status(Ctx#{
        init_script => ?INIT_SCRIPT,
        custom_cmd_env => oz_worker_status_cmd
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
            oz_worker_wait_for_init_attempts, Ctx, integer),
        wait_for_init_delay => service_ctx:get(
            op_worker_wait_for_init_delay, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:nagios_report/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:ctx()) ->
    Response :: http_client:response().
get_nagios_response(Ctx) ->
    service_cluster_worker:get_nagios_response(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx, integer)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_status/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    service_cluster_worker:get_nagios_status(Ctx#{
        nagios_protocol => service_ctx:get(oz_worker_nagios_protocol, Ctx),
        nagios_port => service_ctx:get(oz_worker_nagios_port, Ctx, integer)
    }).

%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_txt_record/1}
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(service:ctx()) -> ok | no_return().
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := _TTL,
    nodes := [Node | _] = Nodes} = Ctx) ->
    % oz_worker does not support custom TTL times

    CurrentRecords = onepanel_env:get_remote(Node, [dns, static_txt_records], name()),

    ok = onepanel_env:set_remote(Nodes, [dns, static_txt_records],
        [{Name, Value} | CurrentRecords], name()),
    reconcile_dns(Ctx);
set_txt_record(#{txt_name := _, txt_value := _} = Ctx) ->
    set_txt_record(Ctx#{nodes => get_nodes()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:remove_txt_record/1}
%% @end
%%--------------------------------------------------------------------
remove_txt_record(#{txt_name := Name, nodes := Nodes} = Ctx) ->
    CurrentRecords = onepanel_env:get_remote(hd(Nodes),
        [dns, static_txt_records], name()),

    ok = onepanel_env:set_remote(Nodes, [dns, static_txt_records],
        proplists:delete(Name, CurrentRecords), name()),
    reconcile_dns(Ctx);
remove_txt_record(#{txt_name := _} = Ctx) ->
    remove_txt_record(Ctx#{nodes => get_nodes()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:reload_webcert/0}
%% @end
%%--------------------------------------------------------------------
reload_webcert(Ctx) ->
    service_cluster_worker:reload_webcert(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:get_domain/0}
%% @end
%%--------------------------------------------------------------------
-spec get_domain(service:ctx()) -> binary().
get_domain(#{node := Node} = Ctx) ->
    list_to_binary(onepanel_env:get_remote(Node, [http_domain], name()));
get_domain(Ctx) ->
    get_domain(Ctx#{node => hd(get_nodes())}).


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:get_admin_email/0}
%% @end
%%--------------------------------------------------------------------
-spec get_admin_email(service:ctx()) -> binary() | undefined.
get_admin_email(_Ctx) ->
    undefined.


-spec reconcile_dns(service:ctx()) -> ok.
reconcile_dns(#{node := Node} = Ctx) ->
    ok = rpc:call(Node, node_manager_plugin, reconcile_dns_config, []);
reconcile_dns(#{nodes := [Node | _]} = Ctx) ->
    reconcile_dns(Ctx#{node => Node});
reconcile_dns(Ctx) ->
    [Node | _] = get_nodes(),
    reconcile_dns(Ctx#{node => Node}).


is_letsencrypt_supported(Ctx) ->
    status(Ctx) == running.

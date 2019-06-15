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
-behaviour(letsencrypt_plugin_behaviour).

-include("names.hrl").
-include("service.hrl").
-include("modules/models.hrl").
-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).
%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, get_dns_server/0,
    get_domain/0, get_admin_email/0, set_http_record/2,
    supports_letsencrypt_challenge/1]).

%% API
-export([configure/1, start/1, stop/1, status/1, health/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1, add_storages/1, get_storages/1,
    update_storage/1, invalidate_luma_cache/1, reload_webcert/1,
    get_compatible_onezones/0, is_connected_to_oz/0]).
-export([migrate_generated_config/1]).

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
    nodes:all(name()).


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

get_steps(invalidate_luma_cache, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = invalidate_luma_cache, selection = any}];

get_steps(invalidate_luma_cache, Ctx) ->
    get_steps(invalidate_luma_cache, Ctx#{hosts => get_hosts()});

get_steps(Action, Ctx) ->
    service_cluster_worker:get_steps(Action, Ctx#{name => name()}).

%%%===================================================================
%%% Public API
%%% Functions which can be called on any node.
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns list of Onezone versions compatible with this worker.
%% @end
%%--------------------------------------------------------------------
-spec get_compatible_onezones() -> [binary()].
get_compatible_onezones() ->
    {_, Node} = nodes:onepanel_with(name()),
    {ok, Versions} = rpc:call(Node, onepanel_env, read_effective,
        [[op_worker, compatible_oz_versions], ?SERVICE_OPW]),
    onepanel_utils:convert(Versions, {seq, binary}).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the op_worker has GraphSync connection to Onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_connected_to_oz() -> boolean().
is_connected_to_oz() ->
    case nodes:any(?SERVICE_OPW) of
        {ok, Node} -> true == rpc:call(Node, oneprovider, is_connected_to_oz, []);
        _ -> false
    end.

%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:ctx()) -> ok | no_return().
configure(Ctx) ->
    GeneratedConfigFile = onepanel_env:get_config_path(name(), generated),
    VmArgsFile = service_ctx:get(op_worker_vm_args_file, Ctx),

    case maps:get(mark_cluster_ips_configured, Ctx, false)
        orelse pop_legacy_ips_configured() of
        true -> onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS);
        _ -> ok
    end,

    ok = service_cluster_worker:configure(Ctx#{
        name => name(),
        app_config => #{},
        generated_config_file => GeneratedConfigFile,
        vm_args_file => VmArgsFile,
        initialize_ip => false % do not set IP until onezone is connected
    }),

    {ok, RecvBuffer} = onepanel_env:read_effective(
        [rtransfer_link, transfer, recv_buffer_size], name()),
    {ok, SendBuffer} = onepanel_env:read_effective(
        [rtransfer_link, transfer, send_buffer_size], name()),
    ?info("Remember to set following kernel parameters "
    "for optimal op_worker performance:~n"
    "sysctl -w ~s=~b~n"
    "sysctl -w ~s=~b", [
        "net.core.wmem_max", 2 * RecvBuffer,
        "net.core.rmem_max", 2 * SendBuffer
    ]).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => service_ctx:get(op_worker_open_files_limit, Ctx)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:ctx()) -> service:status().
status(Ctx) ->
    % Since this function is invoked periodically by onepanel_cron
    % use it to schedule DNS check refresh on a single node
    catch maybe_check_dns(),
    service_cluster_worker:status(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
-spec health(service:ctx()) -> service:status().
health(Ctx) ->
    case (catch get_nagios_status(Ctx)) of
        ok -> healthy;
        _ -> unhealthy
    end.


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
add_storages(#{storages := Storages, ignore_exists := IgnoreExists})
    when map_size(Storages) > 0 ->
    op_worker_storage:add(Storages, IgnoreExists),
    onepanel_deployment:set_marker(?PROGRESS_STORAGE_SETUP);

add_storages(#{storages := _, ignore_exists := _}) ->
    ok;

add_storages(Ctx) ->
    add_storages(Ctx#{ignore_exists => false}).


%%--------------------------------------------------------------------
%% @doc Returns a list of the configured service storages.
%% @end
%%--------------------------------------------------------------------
-spec get_storages(Ctx :: service:ctx()) -> map().
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


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for invalidating luma cache on given
%% provider for given storage.
%% @end
%%-------------------------------------------------------------------
-spec invalidate_luma_cache(Ctx :: service:ctx()) -> ok.
invalidate_luma_cache(#{id := StorageId}) ->
    op_worker_storage:invalidate_luma_cache(StorageId).


%%--------------------------------------------------------------------
%% @doc
%% Ensures certificates changed on disk are updated in worker listeners.
%% @end
%%--------------------------------------------------------------------
-spec reload_webcert(service:ctx()) -> ok.
reload_webcert(Ctx) ->
    service_cluster_worker:reload_webcert(Ctx#{name => name()}),

    Node = nodes:local(name()),
    ok = rpc:call(Node, rtransfer_config, restart_link, []).


%%--------------------------------------------------------------------
%% @doc
%% Checks if the provider has subdomain delegation enabled
%% needed for Let's Encrypt to be available.
%% @end
%%--------------------------------------------------------------------
-spec supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean().
supports_letsencrypt_challenge(http) ->
    service:healthy(name()) andalso
        service_oneprovider:is_registered(); % unregistered provider does not have a domain
supports_letsencrypt_challenge(dns) ->
    try
        service:healthy(name()) andalso
            service_oneprovider:is_registered() andalso
            maps:get(subdomainDelegation, service_oneprovider:get_details(), false)
    catch
        _:_ -> false
    end;
supports_letsencrypt_challenge(_) -> false.


%%--------------------------------------------------------------------
%% @doc {@link letsencrypt_plugin_behaviour:set_http_record/2}
%% @end
%%--------------------------------------------------------------------
-spec set_http_record(Name :: binary(), Value :: binary()) -> ok.
set_http_record(Name, Value) ->
    Nodes = get_nodes(),
    {Results, []} = rpc:multicall(Nodes, http_listener,
        set_response_to_letsencrypt_challenge, [Name, Value]),
    lists:foreach(fun(R) -> ok = R end, Results),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Sets txt record in onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec set_txt_record(Ctx :: service:ctx()) -> ok.
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := TTL}) ->
    {ok, Node} = nodes:any(name()),
    ok = rpc:call(Node, provider_logic, set_txt_record, [Name, Value, TTL]).


%%--------------------------------------------------------------------
%% @doc
%% Removes txt record from onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_record(Ctx :: service:ctx()) -> ok.
remove_txt_record(#{txt_name := Name}) ->
    {ok, Node} = nodes:any(name()),
    ok = rpc:call(Node, provider_logic, remove_txt_record, [Name]).


%%--------------------------------------------------------------------
%% @doc
%% Returns hostname of the dns server responsible for setting txt record.
%% @end
%%--------------------------------------------------------------------
-spec get_dns_server() -> string().
get_dns_server() ->
    service_oneprovider:get_oz_domain().


%%--------------------------------------------------------------------
%% @doc Returns the domain of the provider.
%% @end
%%--------------------------------------------------------------------
-spec get_domain() -> binary().
get_domain() ->
    #{domain := Domain} = service_oneprovider:get_details(),
    Domain.


%%--------------------------------------------------------------------
%% @doc Returns the email address of the provider administrator.
%% @end
%%--------------------------------------------------------------------
-spec get_admin_email() -> binary().
get_admin_email() ->
    case service_oneprovider:get_details() of
        #{adminEmail := AdminEmail} -> AdminEmail;
        _ -> undefined
    end.


%%--------------------------------------------------------------------
%% @doc {@link onepanel_env:migrate_generated_config/2}
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:ctx()) -> ok | no_return().
migrate_generated_config(Ctx) ->
    service_cluster_worker:migrate_generated_config(Ctx#{
        name => name(),
        variables => [
            [name(), oz_domain]
        ]
    }).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%-------------------------------------------------------------------
%% @doc
%% Removes legacy (up to 18.02.0-rc1) way of marking cluster IPs as
%% configured and returns its value.
%% @end
%%-------------------------------------------------------------------
pop_legacy_ips_configured() ->
    case service:get(name()) of
        {ok, #service{ctx = #{cluster_ips_configured := Configured}}} ->
            service:update(name(), fun(#service{ctx = C} = S) ->
                S#service{ctx = maps:remove(cluster_ips_configured, C)}
            end),
            Configured;
        _ -> false
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% If this function is run on the first of service's nodes
%% and Oneprovider is registered, triggers DNS check cache refresh.
%% @end
%%-------------------------------------------------------------------
maybe_check_dns() ->
    case hosts:self() == hd(get_hosts())
        andalso service_oneprovider:is_registered()
        andalso dns_check:should_update_cache(name()) of

        true -> dns_check:async_update_cache(name());
        false -> ok
    end.

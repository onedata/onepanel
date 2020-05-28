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
-include("names.hrl").
-include("modules/models.hrl").
-include("deployment_progress.hrl").
-include("modules/errors.hrl").
-include_lib("hackney/include/hackney_lib.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").


% @formatter:off
-type model_ctx() :: #{
    %% Caches (i.e. not the primary source of truth):
    % service status cache
    status => #{service:host() => service:status()},

    %% Deprecated (removed from ctx after upgrading past 18.02.0-rc1)
    %% {@link pop_legacy_ips_configured/0}
    cluster_ips_configured => boolean()
}.
% @formatter:on

-export_type([model_ctx/0]).


%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% LE behaviour callbacks
-export([set_txt_record/1, remove_txt_record/1, get_dns_server/0,
    get_domain/0, get_admin_email/0, set_http_record/2,
    supports_letsencrypt_challenge/1]).

%% Public API
-export([get_compatible_onezones/0, is_connected_to_oz/0,
    is_transfers_mock_enabled/0]).

%% Step functions
-export([configure/1, configure_additional_node/1, import_provider_auth_file/1,
    start/1, stop/1, status/1, health/1, wait_for_init/1,
    get_nagios_response/1, get_nagios_status/1, add_storage/1, get_storages/1,
    update_storage/1, remove_storage/1, reload_webcert/1,
    set_transfers_mock/1]).

%% LUMA step functions
-export([
    get_luma_configuration/1,
    clear_luma_db/1,
    get_user_mapping/1,
    get_default_posix_credentials/1,
    get_display_credentials/1,
    get_uid_to_onedata_user_mapping/1,
    get_acl_user_to_onedata_user_mapping/1,
    get_acl_group_to_onedata_group_mapping/1,
    add_user_mapping/1,
    add_default_posix_credentials/1,
    add_display_credentials/1,
    add_uid_to_onedata_user_mapping/1,
    add_acl_user_to_onedata_user_mapping/1,
    add_acl_group_to_onedata_group_mapping/1,
    update_user_mapping/1,
    remove_user_mapping/1,
    remove_default_posix_credentials/1,
    remove_display_credentials/1,
    remove_uid_to_onedata_user_mapping/1,
    remove_acl_user_to_onedata_user_mapping/1,
    remove_acl_group_to_onedata_group_mapping/1
]).

-export([migrate_generated_config/1]).


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
-spec get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
get_steps(add_nodes, #{new_hosts := NewHosts} = Ctx) ->
    case get_hosts() of
        [] ->
            {ok, Ctx2} = kv_utils:rename_entry(new_hosts, hosts, Ctx),
            [#steps{action = deploy, ctx = Ctx2}];
        [ExistingHost | _] ->
            Ctx2 = Ctx#{name => name(), reference_host => ExistingHost},
            [
                #step{function = configure_additional_node, hosts = NewHosts, ctx = Ctx2},
                #step{function = import_provider_auth_file, hosts = NewHosts, ctx = Ctx2,
                    condition = fun(_) -> service_oneprovider:is_registered() end}
                | service_cluster_worker:add_nodes_steps(Ctx2)
            ]
    end;

get_steps(add_storages, #{storages := Storages} = Ctx) ->
    ?info("~b storage(s) will be added", [maps:size(Storages)]),
    StoragesList = maps:to_list(Storages),
    [
        #steps{action = add_storage, ctx = Ctx#{name => Name, params => Params}}
        || {Name, Params} <- StoragesList
    ] ++ [
        #step{module = onepanel_deployment, function = set_marker,
            args = [?PROGRESS_STORAGE_SETUP], hosts = [hosts:self()]}
    ];

get_steps(add_storages, _Ctx) ->
    [];

get_steps(add_storage, _Ctx) ->
    [#step{function = add_storage, selection = first}];

get_steps(get_storages, #{hosts := Hosts}) ->
    [#step{hosts = Hosts, function = get_storages, selection = any}];

get_steps(get_storages, Ctx) ->
    get_steps(get_storages, Ctx#{hosts => get_hosts()});

get_steps(update_storage, Ctx) ->
    #{id := Id, storage := Changes} = Ctx,
    Current = op_worker_storage:get(Id),
    case matches_local_ceph_pool(Current) of
        true ->
            [
                #steps{service = ?SERVICE_CEPH, action = modify_pool,
                    ctx = Changes#{name => maps:get(poolName, Current)}},
                #step{function = update_storage, selection = any}
            ];
        false ->
            [
                #step{function = update_storage, selection = any}
            ]
    end;

get_steps(Function, _Ctx) when 
    Function == remove_storage;
    Function == clear_luma_db;
    Function == get_luma_configuration;
    Function == get_user_mapping;
    Function == get_default_posix_credentials;
    Function == get_display_credentials;
    Function == get_uid_to_onedata_user_mapping;
    Function == get_acl_user_to_onedata_user_mapping;
    Function == get_acl_group_to_onedata_group_mapping;
    Function == add_user_mapping;
    Function == add_default_posix_credentials;
    Function == add_display_credentials;
    Function == add_uid_to_onedata_user_mapping;
    Function == add_acl_user_to_onedata_user_mapping;
    Function == add_acl_group_to_onedata_group_mapping;
    Function == update_user_mapping;
    Function == remove_user_mapping;
    Function == remove_default_posix_credentials;
    Function == remove_display_credentials;
    Function == remove_uid_to_onedata_user_mapping;
    Function == remove_acl_user_to_onedata_user_mapping;
    Function == remove_acl_group_to_onedata_group_mapping
->
    [#step{function = Function, selection = any}];

get_steps(Function, _Ctx) when
    Function == set_transfers_mock
->
    [#step{function = Function, selection = all}];

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
    % Try to retrieve oneprovider version from op worker.
    % When opw is not responding use onepanel version instead.
    {_, PanelVersion} = onepanel:get_build_and_version(),
    OpVersion = case nodes:any(?SERVICE_OPW) of
        {ok, Node} ->
            case op_worker_rpc:get_op_worker_version(Node) of
                {badrpc, _} ->
                    PanelVersion;
                V ->
                    V
            end;
        _ ->
            PanelVersion
    end,

    {ok, Versions} = compatibility:get_compatible_versions(
        ?ONEPROVIDER, OpVersion, ?ONEZONE),
    Versions.


%%--------------------------------------------------------------------
%% @doc
%% Checks if the op_worker has GraphSync connection to Onezone.
%% @end
%%--------------------------------------------------------------------
-spec is_connected_to_oz() -> boolean().
is_connected_to_oz() ->
    case nodes:any(?SERVICE_OPW) of
        {ok, OpwNode} -> true == op_worker_rpc:is_connected_to_oz(OpwNode);
        _ -> false
    end.


-spec is_transfers_mock_enabled() -> boolean().
is_transfers_mock_enabled() ->
    {ok, OpwNode} = nodes:any(name()),
    case onepanel_env:get_remote(OpwNode, rtransfer_mock, name()) of
        Boolean when is_boolean(Boolean) -> Boolean;
        _ -> false
    end.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Configures the service.
%% @end
%%--------------------------------------------------------------------
-spec configure(Ctx :: service:step_ctx()) -> ok | no_return().
configure(Ctx) ->
    VmArgsFile = onepanel_env:get(op_worker_vm_args_file),

    case maps:get(mark_cluster_ips_configured, Ctx, false)
        orelse pop_legacy_ips_configured() of
        true -> onepanel_deployment:set_marker(?PROGRESS_CLUSTER_IPS);
        _ -> ok
    end,

    ok = service_cluster_worker:configure(maps:merge(#{
        name => name(),
        app_config => #{},
        vm_args_file => VmArgsFile,
        initialize_ip => service_oneprovider:is_registered() % do not set IP until onezone is connected
    }, Ctx)),

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
%% @doc Configures new op_worker node, assuming there are already
%% some cluster_manager and op_worker instances.
%% @end
%%--------------------------------------------------------------------
-spec configure_additional_node(#{reference_host := service:host(), _ => _}) -> ok.
configure_additional_node(#{reference_host := _} = Ctx) ->
    {ok, MainCmHost} = service_cluster_manager:get_main_host(),
    CmHosts = service_cluster_manager:get_hosts(),
    DbHosts  = service_couchbase:get_hosts(),

    AppConfig = case service_oneprovider:is_registered() of
        true -> #{oz_domain => service_oneprovider:get_oz_domain()};
        false -> #{}
    end,
    NewCtx = maps_utils:merge([
        #{
            main_cm_host => MainCmHost, cm_hosts => CmHosts,
            db_hosts => DbHosts,
            app_config => AppConfig#{
                rtransfer_mock => is_transfers_mock_enabled()
            }
        },
        Ctx
    ]),
    configure(NewCtx).


%%--------------------------------------------------------------------
%% @doc Copies file storing provider's token to the current node.
%% @end
%%--------------------------------------------------------------------
-spec import_provider_auth_file(
    #{reference_host := service:host() | node(), _ => _}
) -> ok.
import_provider_auth_file(#{reference_host := RefHost}) ->
    RefNode = nodes:service_to_node(?SERVICE_PANEL, RefHost),
    Path = onepanel_env:get_remote(RefNode, op_worker_root_token_path, ?APP_NAME),
    ok = rpc:call(RefNode, onepanel_utils, distribute_file, [[hosts:self()], Path]).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:start/1}
%% @end
%%--------------------------------------------------------------------
-spec start(Ctx :: service:step_ctx()) -> ok | no_return().
start(Ctx) ->
    NewCtx = maps:merge(#{
        open_files => onepanel_env:get(op_worker_open_files_limit)
    }, Ctx),
    service_cluster_worker:start(NewCtx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:stop/1}
%% @end
%%--------------------------------------------------------------------
-spec stop(Ctx :: service:step_ctx()) -> ok | no_return().
stop(Ctx) ->
    service_cluster_worker:stop(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc {@link service_cli:status/1}
%% @end
%%--------------------------------------------------------------------
-spec status(Ctx :: service:step_ctx()) -> service:status().
status(Ctx) ->
    % Since this function is invoked periodically by onepanel_cron
    % use it to schedule DNS check refresh on a single node
    catch maybe_check_dns(),
    service_cluster_worker:status(Ctx#{name => name()}).


%%--------------------------------------------------------------------
%% @doc Checks if a running service is in a fully functional state.
%% @end
%%--------------------------------------------------------------------
-spec health(service:step_ctx()) -> service:status().
health(Ctx) ->
    case (catch get_nagios_status(Ctx)) of
        ok -> healthy;
        _ -> unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:wait_for_init/1}
%% @end
%%--------------------------------------------------------------------
-spec wait_for_init(Ctx :: service:step_ctx()) -> ok | no_return().
wait_for_init(Ctx) ->
    service_cluster_worker:wait_for_init(Ctx#{
        name => name(),
        wait_for_init_attempts => onepanel_env:get(op_worker_wait_for_init_attempts),
        wait_for_init_delay => onepanel_env:get(op_worker_wait_for_init_delay)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_response/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_response(Ctx :: service:step_ctx()) ->
    Response :: http_client:response().
get_nagios_response(Ctx) ->
    service_cluster_worker:get_nagios_response(Ctx#{
        nagios_protocol => onepanel_env:get(op_worker_nagios_protocol),
        nagios_port => onepanel_env:get(op_worker_nagios_port)
    }).


%%--------------------------------------------------------------------
%% @doc {@link service_cluster_worker:get_nagios_status/1}
%% @end
%%--------------------------------------------------------------------
-spec get_nagios_status(Ctx :: service:step_ctx()) -> Status :: atom().
get_nagios_status(Ctx) ->
    service_cluster_worker:get_nagios_status(Ctx#{
        nagios_protocol => onepanel_env:get(op_worker_nagios_protocol),
        nagios_port => onepanel_env:get(op_worker_nagios_port)
    }).


%%--------------------------------------------------------------------
%% @doc Configures the service storages.
%% @end
%%--------------------------------------------------------------------
-spec add_storage(Ctx :: service:step_ctx()) -> ok | no_return().
add_storage(#{params := #{type := Type} = Params, name := Name} = Ctx) when
    Type == ?LOCAL_CEPH_STORAGE_TYPE ->
    case hosts:all(?SERVICE_CEPH_OSD) of
        [] -> throw(?ERROR_NO_SERVICE_NODES(?SERVICE_CEPH_OSD));
        _ -> ok
    end,

    case ceph_pool:exists(Name) of
        true -> ok;
        false ->
            service_utils:throw_on_error(service:apply_sync(
                ?SERVICE_CEPH, create_pool, Params#{name => Name}
            ))
    end,
    FilledParams = maps:merge(Params, service_ceph:make_storage_params(Name)),
    add_storage(Ctx#{params => FilledParams});

add_storage(#{params := _, name := _} = Ctx) ->
    op_worker_storage:add(Ctx).


%%--------------------------------------------------------------------
%% @doc Returns a list of the configured service storages.
%% @end
%%--------------------------------------------------------------------
-spec get_storages(#{id => op_worker_storage:id()}) ->
    op_worker_storage:storage_details()
    | [op_worker_storage:id()].
get_storages(#{id := Id}) ->
    Details = op_worker_storage:get(Id),
    case matches_local_ceph_pool(Details) of
        true -> service_ceph:decorate_storage_details(Details);
        false -> Details
    end;

get_storages(_Ctx) ->
    op_worker_storage:list().


%%--------------------------------------------------------------------
%% @doc Modifies storage configuration.
%% @end
%%--------------------------------------------------------------------
-spec update_storage(Ctx :: service:step_ctx()) ->
    op_worker_storage:storage_params() | no_return().
update_storage(#{id := Id, storage := Params}) ->
    {ok, Node} = nodes:any(name()),
    op_worker_storage:update(Node, Id, Params).


%%--------------------------------------------------------------------
%% @doc Removes op_worker storage.
%% @end
%%--------------------------------------------------------------------
-spec remove_storage(Ctx :: #{id := op_worker_storage:id(), _ => _}) -> ok | no_return().
remove_storage(#{id := Id}) ->
    {ok, Node} = nodes:any(name()),
    #{name := Name} = Details = op_worker_storage:get(Id),
    op_worker_storage:remove(Node, Id),
    case matches_local_ceph_pool(Details) of
        true ->
            service_utils:throw_on_error(service:apply_sync(
                ?SERVICE_CEPH, delete_pool, #{name => Name}
            )),
            ok;
        false ->
            ok
    end.


-spec get_luma_configuration(Ctx :: service:step_ctx()) -> ok.
get_luma_configuration(#{storage := Storage}) ->
    op_worker_luma:get_luma_configuration(Storage).


-spec clear_luma_db(Ctx :: service:step_ctx()) -> ok.
clear_luma_db(#{id := StorageId}) ->
    op_worker_luma:clear_luma_db(StorageId).


-spec get_user_mapping(Ctx :: service:step_ctx()) -> ok.
get_user_mapping(#{id := StorageId, onedataUserId := OnedataUserId}) ->
    op_worker_luma:get_user_mapping(StorageId, OnedataUserId).


-spec get_default_posix_credentials(Ctx :: service:step_ctx()) -> ok.
get_default_posix_credentials(#{id := StorageId, spaceId := SpaceId}) ->
    op_worker_luma:get_default_posix_credentials(StorageId, SpaceId).


-spec get_display_credentials(Ctx :: service:step_ctx()) -> ok.
get_display_credentials(#{id := StorageId, spaceId := SpaceId}) ->
    op_worker_luma:get_display_credentials(StorageId, SpaceId).


-spec get_uid_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
get_uid_to_onedata_user_mapping(#{id := StorageId, uid := Uid}) ->
    op_worker_luma:get_uid_to_onedata_user_mapping(StorageId, Uid).


-spec get_acl_user_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
get_acl_user_to_onedata_user_mapping(#{id := StorageId, aclUser := AclUser}) ->
    op_worker_luma:get_acl_user_to_onedata_user_mapping(StorageId, AclUser).

-spec get_acl_group_to_onedata_group_mapping(Ctx :: service:step_ctx()) -> ok.
get_acl_group_to_onedata_group_mapping(#{id := StorageId, aclGroup := AclGroup}) ->
    op_worker_luma:get_acl_group_to_onedata_group_mapping(StorageId, AclGroup).

-spec add_user_mapping(Ctx :: service:step_ctx()) -> ok.
add_user_mapping(#{id := StorageId, mapping := Mapping}) ->
    op_worker_luma:add_user_mapping(StorageId, Mapping).

-spec add_default_posix_credentials(Ctx :: service:step_ctx()) -> ok.
add_default_posix_credentials(#{id := StorageId, spaceId := SpaceId, credentials := Credentials}) ->
    op_worker_luma:add_default_posix_credentials(StorageId, SpaceId, Credentials).

-spec add_display_credentials(Ctx :: service:step_ctx()) -> ok.
add_display_credentials(#{id := StorageId, spaceId := SpaceId, credentials := Credentials}) ->
    op_worker_luma:add_display_credentials(StorageId, SpaceId, Credentials).

-spec add_uid_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
add_uid_to_onedata_user_mapping(#{id := StorageId, uid := Uid, mapping := Mapping}) ->
    op_worker_luma:add_uid_to_onedata_user_mapping(StorageId, Uid, Mapping).

-spec add_acl_user_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
add_acl_user_to_onedata_user_mapping(#{id := StorageId, aclUser := AclUser, mapping := Mapping}) ->
    op_worker_luma:add_acl_user_to_onedata_user_mapping(StorageId, AclUser, Mapping).

-spec add_acl_group_to_onedata_group_mapping(Ctx :: service:step_ctx()) -> ok.
add_acl_group_to_onedata_group_mapping(#{id := StorageId, aclGroup := AclGroup, mapping := Mapping}) ->
    op_worker_luma:add_acl_group_to_onedata_group_mapping(StorageId, AclGroup, Mapping).

-spec update_user_mapping(Ctx :: service:step_ctx()) -> ok.
update_user_mapping(#{id := StorageId, onedataUserId := OnedataUserId, storageUser := StorageUser}) ->
    op_worker_luma:update_user_mapping(StorageId, OnedataUserId, StorageUser).

-spec remove_user_mapping(Ctx :: service:step_ctx()) -> ok.
remove_user_mapping(#{id := StorageId, onedataUserId := OnedataUserId}) ->
    op_worker_luma:remove_user_mapping(StorageId, OnedataUserId).

-spec remove_default_posix_credentials(Ctx :: service:step_ctx()) -> ok.
remove_default_posix_credentials(#{id := StorageId, spaceId := SpaceId}) ->
    op_worker_luma:remove_default_posix_credentials(StorageId, SpaceId).

-spec remove_display_credentials(Ctx :: service:step_ctx()) -> ok.
remove_display_credentials(#{id := StorageId, spaceId := SpaceId}) ->
    op_worker_luma:remove_display_credentials(StorageId, SpaceId).

-spec remove_uid_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
remove_uid_to_onedata_user_mapping(#{id := StorageId, uid := Uid}) ->
    op_worker_luma:remove_uid_to_onedata_user_mapping(StorageId, Uid).

-spec remove_acl_user_to_onedata_user_mapping(Ctx :: service:step_ctx()) -> ok.
remove_acl_user_to_onedata_user_mapping(#{id := StorageId, aclUser := AclUser}) ->
    op_worker_luma:remove_acl_user_to_onedata_user_mapping(StorageId, AclUser).

-spec remove_acl_group_to_onedata_group_mapping(Ctx :: service:step_ctx()) -> ok.
remove_acl_group_to_onedata_group_mapping(#{id := StorageId, aclGroup := AclGroup}) ->
    op_worker_luma:remove_acl_group_to_onedata_group_mapping(StorageId, AclGroup).

-spec set_transfers_mock(#{transfers_mock := boolean()}) -> ok.
set_transfers_mock(#{transfers_mock := Enabled}) ->
    env_write_and_set(rtransfer_mock, Enabled).


%%--------------------------------------------------------------------
%% @doc
%% Ensures certificates changed on disk are updated in op_worker
%% on the current node.
%% @end
%%--------------------------------------------------------------------
-spec reload_webcert(service:step_ctx()) -> ok.
reload_webcert(Ctx) ->
    service_cluster_worker:reload_webcert(Ctx#{name => name()}),

    Node = nodes:local(name()),
    ok = op_worker_rpc:restart_rtransfer_link(Node).


%%--------------------------------------------------------------------
%% @doc
%% Checks if given Let's Encrypt challenge can be fulfilled.
%% @end
%%--------------------------------------------------------------------
-spec supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean().
supports_letsencrypt_challenge(Challenge) when
    Challenge == http;
    Challenge == dns
->
    ?MODULE:get_hosts() /= [] orelse throw(?ERROR_NO_SERVICE_NODES(name())),
    service:is_healthy(name()) orelse throw(?ERROR_SERVICE_UNAVAILABLE),
    service_oneprovider:is_registered() orelse throw(?ERROR_UNREGISTERED_ONEPROVIDER),
    case Challenge of
        http -> true;
        dns ->
            case maps:get(subdomainDelegation, service_oneprovider:get_details(), false) of
                true -> true;
                false -> false
            end
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
-spec set_txt_record(Ctx :: service:step_ctx()) -> ok.
set_txt_record(#{txt_name := Name, txt_value := Value, txt_ttl := TTL}) ->
    ok = op_worker_rpc:set_txt_record(Name, Value, TTL).


%%--------------------------------------------------------------------
%% @doc
%% Removes txt record from onezone dns via oneprovider.
%% @end
%%--------------------------------------------------------------------
-spec remove_txt_record(Ctx :: service:step_ctx()) -> ok.
remove_txt_record(#{txt_name := Name}) ->
    ok = op_worker_rpc:remove_txt_record(Name).


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
%% @doc Copies given variables from old app.config file to the "generated"
%% app config file. Afterwards moves the legacy file to a backup location.
%% @end
%%--------------------------------------------------------------------
-spec migrate_generated_config(service:step_ctx()) -> ok | no_return().
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
-spec pop_legacy_ips_configured() -> boolean().
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
-spec maybe_check_dns() -> ok.
maybe_check_dns() ->
    case hosts:self() == hd(get_hosts())
        andalso service_oneprovider:is_registered()
        andalso dns_check:should_update_cache(name()) of

        true -> dns_check:async_update_cache(name());
        false -> ok
    end.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Writes op_worker app variable to autogenerated app config file
%% and sets it in the live config on local node.
%% @end
%%-------------------------------------------------------------------
-spec env_write_and_set(Variable :: atom(), Value :: term()) -> ok | no_return().
env_write_and_set(Variable, Value) ->
    Node = nodes:local(name()),
    onepanel_env:write([name(), Variable], Value, ?SERVICE_OPW),
    % if op-worker is offline failure can be ignored,
    % the variable will be read on next startup
    catch onepanel_env:set_remote(Node, Variable, Value, name()),
    ok.


%%-------------------------------------------------------------------
%% @private
%% @doc
%% Checks if a storage using cephrados helper has a corresponding pool
%% in the local Ceph cluster.
%% @end
%%-------------------------------------------------------------------
-spec matches_local_ceph_pool
    (op_worker_storage:storage_details() | op_worker_storage:id()) -> boolean().
matches_local_ceph_pool(#{type := Type, name := Name, clusterName := ClusterName}) when
    Type == ?LOCAL_CEPH_STORAGE_TYPE; % when checking against op_worker output
    Type == ?CEPH_STORAGE_HELPER_NAME % when checking user input
->
    service:exists(?SERVICE_CEPH)
        andalso ceph:get_cluster_name() == ClusterName
        andalso ceph_pool:exists(Name);

matches_local_ceph_pool(#{}) ->
    false.

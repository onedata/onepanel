%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Higher level service module for managing ceph cluster
%%% comprised of Monitors, OSDs and Managers.
%%%
%%% The Ceph services differ from Onedata services in that there can
%%% be more than one per host, especially OSDs. To support such setup
%%% each of the mgr, mon, osd models have 'instances' map in
%%% the #service.ctx.
%%%
%%% The local Ceph cluster is used to create storages baked by
%%% its pools. It is done by creating a storage with type "localceph",
%%% which triggers creation of Ceph pool with name equal to the storage
%%% and an op_worker storage of type "cephrados".
%%% When listing the storage, its name and clusterName arg
%%% are used to detect corresponding local pool and the storage&pool details
%%% are returned to the user as "localceph" type storage model.
%%% @end
%%%--------------------------------------------------------------------

-module(service_ceph).
-author("Wojciech Geisler").
-behaviour(service_behaviour).

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API
-export([status/1]).

%% Step functions
-export([initialize_config/1, distribute_config/1, write_config/1,
    create_admin_keyring/1, extend_cluster/1, register_host/1]).
-export([make_storage_params/1, decorate_storage_details/1]).
-export([get_details/0, get_usage/1, get_health_report/0]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:name/0}
%% @end
%%--------------------------------------------------------------------
-spec name() -> Name :: service:name().
name() ->
    ?SERVICE_CEPH.


%%--------------------------------------------------------------------
%% @doc Returns list of hosts guaranteed to have ceph.conf
%% and admin keyring files present.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts() -> Hosts :: [service:host()].
get_hosts() ->
    service:get_hosts(name()).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_nodes/0}
%% @end
%%--------------------------------------------------------------------
-spec get_nodes() -> no_return().
get_nodes() ->
    error(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
%% Deploys whole or part of the Ceph cluster.
%% Deployment of managers or monitors will be skipped on hosts
%% already having a fully deployed daemon.
%% OSDs with Ids used by a deployed daemon will be skipped as well.
get_steps(deploy, Ctx) ->
    InitialCtx = maps:with([cluster_name, fsid], Ctx),
    ensure_models(InitialCtx),

    Mons = maps:get(monitors, Ctx, []),
    Mgrs = maps:get(managers, Ctx, []),
    Osds = maps:get(osds, Ctx, []),
    MonHosts = ceph:extract_hosts(Mons),
    MgrHosts = ceph:extract_hosts(Mgrs),
    OsdHosts = ceph:extract_hosts(Osds),
    Hosts = lists_utils:union([MonHosts, MgrHosts, OsdHosts, get_hosts()]),
    [
        #steps{action = ensure_hosts, ctx = Ctx#{hosts => Hosts}},
        #steps{action = deploy_all, service = ceph_mon, ctx = Ctx#{hosts => MonHosts},
            condition = Mons /= []},
        #steps{action = deploy_all, service = ceph_mgr, ctx = Ctx#{hosts => MgrHosts},
            condition = Mgrs /= []},
        #steps{action = deploy_all, service = ceph_osd, ctx = Ctx#{hosts => OsdHosts},
            condition = Osds /= []}
    ];

%% Initialize cluster or copy existing auth
get_steps(ensure_hosts, #{hosts := []}) ->
    [];
get_steps(ensure_hosts, #{hosts := Hosts} = Ctx) ->
    {NewCluster, Master} = case service:get(name()) of
        {ok, #service{hosts = [First | _]}} -> {false, First};
        _ -> {true, hd(Hosts)}
    end,
    [
        #steps{action = init_cluster, ctx = Ctx#{hosts => [Master]},
            condition = NewCluster},
        #step{function = extend_cluster, hosts = [Master]},
        #step{function = register_host, hosts = Hosts}
    ];

get_steps(init_cluster, _Ctx) ->
    [
        #step{function = initialize_config, selection = first},
        #step{function = create_admin_keyring, selection = first},
        #step{function = register_host, selection = all}
    ];

get_steps(get_pool, #{name := Name}) ->
    [#step{module = ceph_pool, function = get, selection = any, args = [Name]}];

get_steps(get_all_pools, _Ctx) ->
    [#step{module = ceph_pool, function = get_all, selection = any, args = []}];

get_steps(create_pool, Ctx) ->
    % validate here so that the pool is not created if set_replication is to fail
    ok = ceph_pool:validate_copies_number(Ctx),
    Ctx2 = ceph_pool:insert_default_values(Ctx),
    [
        #step{module = ceph_pool, function = create, ctx = Ctx2, selection = first},
        #step{module = ceph_pool, function = set_replication, ctx = Ctx2, selection = first}
    ];

get_steps(delete_pool, _Ctx) ->
    [#step{module = ceph_pool, function = delete, selection = any}];

get_steps(modify_pool, Ctx) ->
    ok = ceph_pool:validate_copies_number(Ctx),
    [#step{module = ceph_pool, function = set_replication, selection = any}];

get_steps(resume, _Ctx) ->
    case service:exists(name()) of
        true ->
            [
                #steps{action = resume_all, service = ceph_mon},
                #steps{action = resume_all, service = ceph_mgr},
                #steps{action = resume_all, service = ceph_osd}
            ];
        false ->
            []
    end;

get_steps(NoArgsAction, _Ctx) when
    NoArgsAction == get_health_report;
    NoArgsAction == get_details
->
    [#step{function = NoArgsAction, args = [], selection = any}];

get_steps(SingleHostAction, _Ctx) when
    SingleHostAction == get_usage
->
    [#step{function = SingleHostAction, selection = any}];

get_steps(AllHostAction, _Ctx) when
    AllHostAction == write_config
->
    [#step{function = AllHostAction, selection = all}].


%%%===================================================================
%%% Healthcheck
%%%===================================================================


-spec status(service:ctx()) -> service:status().
status(_Ctx) ->
    health().


%% @private
-spec health() -> service:status().
health() ->
    try get_health_report() of
        #{level := ok} -> healthy;
        #{level := _} -> unhealthy
    catch
        Type:Error ->
            ?warning("Cannot check ceph health because of ~tp:~tp", [Type, Error]),
            unhealthy
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns Ceph cluster status report.
%% @end
%%--------------------------------------------------------------------
-spec get_health_report() -> #{level := ceph:status_level(), messages => [binary()]}.
get_health_report() ->
    case ceph_cli:health() of
        {ok, Map} ->
            Status = maps:get(<<"status">>, Map),
            Checks = maps:get(<<"checks">>, Map, #{}),
            #{
                level => ceph_status_level(Status),
                messages => format_checks(Checks)
            };
        Error ->
            ?warning("Could not obtain ceph health report: ~tp", [Error]),
            throw(?ERROR_INTERNAL_SERVER_ERROR)
    end.


%%%===================================================================
%%% Api functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Returns global Ceph cluster information.
%% @end
%%--------------------------------------------------------------------
-spec get_details() -> #{atom() := binary()}.
get_details() ->
    #{fsid := FSID, cluster_name := ClusterName} = get_ctx(),
    #{fsid => FSID, name => ClusterName}.


%%--------------------------------------------------------------------
%% @doc
%% Writes given Ceph configuration on all nodes.
%% @end
%%--------------------------------------------------------------------
-spec distribute_config(ceph_conf:config()) -> ok | {error, _}.
distribute_config(Config) ->
    Results = service:apply_sync(ceph, write_config, #{config => Config}),
    case service_utils:results_contain_error(Results) of
        false -> ok;
        {true, Error} -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns the part of cephrados storage params which can be derived
%% from ceph cluster params.
%% @end
%%--------------------------------------------------------------------
-spec make_storage_params(ceph_pool:name()) -> op_worker_storage:storage_params().
make_storage_params(PoolName) ->
    Self = node(),
    case nodes:onepanel_with(name()) of
        {ok, Self} ->
            Cluster = ceph:get_cluster_name(),
            MonitorHosts = onepanel_utils:join(hosts:all(?SERVICE_CEPH_MON), <<",">>),
            {Username, Key} = ceph:get_pool_user(),
            #{
                type => ?CEPH_STORAGE_HELPER_NAME,
                insecure => true,
                clusterName => Cluster,
                poolName => PoolName,
                username => Username,
                key => Key,
                monitorHostname => MonitorHosts
            };
        {ok, Other} ->
            rpc:call(Other, ?MODULE, ?FUNCTION_NAME, [PoolName])
    end.


%%--------------------------------------------------------------------
%% @doc
%% Decorates op_worker storage details with ceph pool information
%% if the storage name matches a pool name.
%% @end
%%--------------------------------------------------------------------
-spec decorate_storage_details(op_worker_storage:storage_details()) ->
    op_worker_storage:storage_details().
decorate_storage_details(#{type := ?CEPH_STORAGE_HELPER_NAME, name := Name} = Storage) ->
    case ceph_pool:get(Name) of
        {error, _} ->
            Storage;
        Pool ->
            maps:merge(Storage#{type => ?LOCAL_CEPH_STORAGE_TYPE}, Pool)
    end;

decorate_storage_details(Storage) ->
    Storage.


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Writes initial config file to default location.
%% @end
%%--------------------------------------------------------------------
-spec initialize_config(service:ctx()) -> ok.
initialize_config(_Ctx) ->
    #{fsid := UUID} = get_ctx(),
    ConfigBase = onepanel_env:get(ceph_default_config),
    Config = ceph_conf:put(global, <<"fsid">>, UUID, ConfigBase),
    ok = ceph_conf:write(Config, ceph:get_conf_path()).


%%--------------------------------------------------------------------
%% @doc
%% Prepare hosts for joining ceph cluster be copying ceph.conf
%% and admin keyring.
%% Must be executed on a host with existing configuration files.
%% @end
%%--------------------------------------------------------------------
-spec extend_cluster(service:ctx()) -> ok.
extend_cluster(#{hosts := Hosts}) ->
    NewHosts = lists_utils:subtract(Hosts, get_hosts()),

    ConfPath = ceph:get_conf_path(),
    KeyringPath = ceph:get_admin_keyring_path(),

    onepanel_utils:distribute_file(NewHosts, ConfPath),
    onepanel_utils:distribute_file(NewHosts, KeyringPath).


%%--------------------------------------------------------------------
%% @doc Stores current host in service model.
%% @end
%%--------------------------------------------------------------------
-spec register_host(service:ctx()) -> ok.
register_host(_Ctx) ->
    service:add_host(name(), hosts:self()).


%%--------------------------------------------------------------------
%% @doc
%% Generates a keyring file, with capabilities for managing all Ceph
%% daemons.
%% @end
%%--------------------------------------------------------------------
-spec create_admin_keyring(service:ctx()) -> ok.
create_admin_keyring(_Ctx) ->
    Path = ceph:get_admin_keyring_path(),
    ceph_cli:auth_create_keyring(Path, "client.admin", [
        {mon, <<"allow *">>},
        {osd, <<"allow *">>},
        {mds, <<"allow *">>},
        {mgr, <<"allow *">>}
    ]).


%%--------------------------------------------------------------------
%% @doc
%% Writes ceph_conf:config to the Ceph cluster configuration file..
%% @end
%%--------------------------------------------------------------------
-spec write_config(#{config := ceph_conf:config(), path => file:name_all()}) ->
    ok.
write_config(#{config := Config} = Ctx) ->
    Path = maps:get(path, Ctx, ceph:get_conf_path()),
    ceph_conf:write(Config, Path).


%%--------------------------------------------------------------------
%% @doc
%% Returns disk space usage data, either for the whole cluster
%% or for a pool with specified name.
%% @end
%%--------------------------------------------------------------------
-spec get_usage(service:ctx()) ->
    ceph_pool:usage() |
    #{total := map(), osds := map(), pools := #{ceph_pool:name() => ceph_pool:usage()}}.
get_usage(Ctx) ->
    #{<<"stats">> := Stats, <<"pools">> := Pools} = ceph_cli:df(),
    Total = kv_utils:copy_found([
        {<<"total_bytes">>, total},
        {<<"total_used_bytes">>, used},
        {<<"total_avail_bytes">>, available}
    ], Stats),
    PoolsMap = pools_df_list_to_map(Pools),

    case maps:find(pool, Ctx) of
        {ok, Name} ->
            maps:get(Name, PoolsMap);
        error ->
            OsdsUsage = service_ceph_osd:get_usage(),
            #{total => Total, osds => OsdsUsage, pools => PoolsMap}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures existence of service models for service_ceph
%% and Ceph daemons services.
%% When creating the service_ceph model inserts default ctx.
%% @end
%%--------------------------------------------------------------------
-spec ensure_models(GlobalParams :: map()) -> ok.
ensure_models(GlobalParams) ->
    DefaultName = onepanel_env:typed_get(ceph_default_cluster, binary),
    Defaults = #{cluster_name => DefaultName, fsid => ceph:gen_uuid()},
    ServiceCtx = maps:merge(Defaults, GlobalParams),

    service:create(#service{name = name(), ctx = ServiceCtx}),
    lists:foreach(fun(Module) ->
        service:create(#service{name = Module:name(), ctx = #{instances => #{}}})
    end, [service_ceph_osd, service_ceph_mon, service_ceph_mgr]).


%%--------------------------------------------------------------------
%% @private @doc Returns this service's ctx.
%% @end
%%--------------------------------------------------------------------
-spec get_ctx() -> service:ctx() | {error, _}.
get_ctx() ->
    service:get_ctx(name()).


%%--------------------------------------------------------------------
%% @private @doc Converts Ceph health level to atom.
%% @end
%%--------------------------------------------------------------------
-spec ceph_status_level(binary()) -> ceph:status_level().
ceph_status_level(<<"HEALTH_OK">>) -> ok;
ceph_status_level(<<"HEALTH_WARN">>) -> warning;
ceph_status_level(<<"HEALTH_ERR">>) -> error;
ceph_status_level(<<Level/binary>>) ->
    % should not happen, but crashing healthcheck should be avoided
    ?warning("Unrecorgnized Ceph status level: ~tp", [Level]),
    warning.


%%--------------------------------------------------------------------
%% @private @doc Formats Ceph health messages.
%% @end
%%--------------------------------------------------------------------
-spec format_checks(#{binary() => binary()}) -> [binary()].
format_checks(Map) ->
    lists:map(fun({Check, Description}) ->
        Summary = kv_utils:get([<<"summary">>, <<"message">>], Description),
        Details = maps:get(<<"detail">>, Description, []),
        DetailsIodata = case Details of
            [] -> "";
            _ ->
                Messages = [Msg || #{<<"message">> := Msg} <- Details],
                [" (", lists:join("; ", Messages), ")"]
        end,
        str_utils:format_bin("~ts: ~ts~ts", [Check, Summary, DetailsIodata])
    end, maps:to_list(Map)).


%% @private
-spec pools_df_list_to_map([map()]) -> #{ceph_pool:name() => ceph_pool:usage()}.
pools_df_list_to_map(Pools) ->
    lists:foldl(fun(Pool, Acc) ->
        #{<<"name">> := Name, <<"stats">> := PoolStats} = Pool,
        PoolUsage = ceph_pool:parse_data_usage(PoolStats),
        Acc#{Name => PoolUsage}
    end, #{}, Pools).

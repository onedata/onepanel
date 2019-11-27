%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains ceph monitor service management functions.
%%% There should never be more than one monitor on a node.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ceph_mon).
-author("Wojciech Geisler").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

-define(TMP_KEYRING_PATH, <<"/tmp/mon.keyring">>).

%% monitor Id, always equal to `list_to_binary(monitor's hostname)`
-type id() :: binary().
-export_type([id/0]).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API functions
-export([status/1, exists/1]).

%% Step functions
-export([add_monitors/1, create_monmap/2, create_keyring/1, setup_initial_member/1,
    add_mon_to_config/1, mkfs/1, start/1, wait_for_init/1, stop/1,
    register_host/1]).
-export([get_details/1, list/0, list_quorum/0]).

% private RPC
-export([get_mon_auth/0, get_mon_map/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

-spec name() -> Name :: service:name().
name() ->
    ?SERVICE_CEPH_MON.


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
-spec get_nodes() -> no_return().
get_nodes() ->
    ?throw_error(?ERR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link service_behaviour:get_steps/2}
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Action :: service:action(), Args :: service:ctx()) ->
    Steps :: [service:step()].
get_steps(deploy_all, #{monitors := []}) ->
    [];

get_steps(deploy_all, #{monitors := Mons} = AllCtx) ->
    WithIds = ceph:ensure_ids_from_hosts(Mons),
    Filtered = ceph:filter_out_existing_instances(name(), WithIds),
    AllCtx2 = AllCtx#{monitors => Filtered},

    {ReferenceHost, CurrentHosts} = case service:get(name()) of
        {ok, #service{hosts = [First | _] = Hosts}} ->
            {First, Hosts};
        {ok, #service{hosts = []}} ->
            {maps:get(host, hd(Filtered)), []}
    end,
    [
        #step{function = add_monitors, ctx = AllCtx2},
        #steps{action = initialize_config, ctx = AllCtx2, condition = CurrentHosts == []}
        |
        service_utils:for_each_ctx(Filtered, [
            #steps{action = add_node, ctx = #{reference_host => ReferenceHost}}
        ])
    ];

get_steps(initialize_config, _Ctx) ->
    [#step{function = setup_initial_member, selection = first}];


get_steps(resume_all, _Ctx) ->
    service_utils:for_each_ctx(list_instances(), [
        #step{function = start}]) ++
    % check quorum only after starting all monitors
    service_utils:for_each_ctx(list_instances(), [
        #step{function = wait_for_init}]);

get_steps(resume, #{id := _Id}) ->
        [#step{function = start}];

get_steps(add_node, #{reference_host := _, hosts := [_]}) ->
    [
        #step{function = mkfs},
        #step{function = add_mon_to_config},
        #step{function = start},
        #step{function = wait_for_init},
        #step{function = register_host}
    ];

get_steps(Action, _Ctx) when
    Action == get_details ->
    [#step{function = Action, selection = any}];

get_steps(NoArgsFunction, _Ctx) when
    NoArgsFunction == list ->
    [#step{function = NoArgsFunction, args = [], selection = any}];

get_steps(stop_all, _Cxt) ->
    service_utils:for_each_ctx(list_instances(), [
        #steps{action = stop}
    ]);

get_steps(Action, #{id := _} = Ctx) when
    Action == start;
    Action == stop ->
    OneHostCtx = case Ctx of
        #{id := _, hosts := [_]} -> Ctx;
        #{id := _, hosts := []} -> Ctx;
        #{id := _, hosts := Hosts} when is_list(Hosts) ->
            ?throw_error(?ERR_AMBIGUOUS_HOSTS);
        #{id := Id} ->
            Ctx#{hosts => [maps:get(host, get_instance(Id))]}
    end,
    [#step{function = Action, ctx = OneHostCtx}].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Status used by service healthcheck.
%% @end
%%--------------------------------------------------------------------
-spec status(#{id := id()}) -> service:status().
status(#{id := Id}) ->
    #{ip := Ip} = get_instance(Id),
    DataDir = ceph:get_data_dir(mon, Id),
    StartedBy = ceph_cli:mon_start_cmd(Id, DataDir, Ip),
    case onepanel_shell:process_exists(StartedBy) of
        true -> healthy;
        false -> stopped
    end.


-spec exists(id()) -> boolean().
exists(Id) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := _}} -> true;
        _ -> false
    end.


%%%===================================================================
%%% Node setup step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Stores monitor instance data in the service ctx.
%% @end
%%--------------------------------------------------------------------
-spec add_monitors(#{monitors := [#{host := service:host(), _ => _}]}) -> ok.
add_monitors(#{monitors := Mons}) ->
    lists:foreach(fun(#{id := <<Id/binary>>, host := Host} = Ctx) ->
        Addr = case Ctx of
            #{ip := Ip} -> Ip;
            _ -> get_local_ip(Host)
        end,
        service:store_in_ctx(name(), [instances, Id],
            #{id => Id, host => Host, ip => Addr, deployment_finished => false})
    end, Mons).


%%--------------------------------------------------------------------
%% @doc
%% Ensures at least one monitor is listed as "initial member"
%% in Ceph conf file.
%% @end
%%--------------------------------------------------------------------
-spec setup_initial_member(service:ctx()) -> ok.
setup_initial_member(#{monitors := Monitors}) ->
    [Id | _] = [Id || #{id := Id} <- Monitors],
    Config = ceph_conf:read(ceph:get_conf_path()),
    NewConfig = ceph_conf:put(global, <<"mon initial members">>, Id, Config),
    service_ceph:distribute_config(NewConfig).


%%--------------------------------------------------------------------
%% @doc
%% Adds monitor host and addr to the Ceph config file.
%% @end
%%--------------------------------------------------------------------
-spec add_mon_to_config(#{id := id()}) -> ok | #error{}.
add_mon_to_config(#{id := Id}) ->
    Ip = kv_utils:get([instances, Id, ip], service:get_ctx(name())),
    Config = ceph_conf:read(ceph:get_conf_path()),
    NewConfig1 = ceph_conf:append(global, <<"mon host">>, Ip, Config),
    NewConfig2 = ceph_conf:put(<<"mon.", Id/binary>>, <<"mon addr">>, Ip, NewConfig1),
    service_ceph:distribute_config(NewConfig2).


%%--------------------------------------------------------------------
%% @doc
%% Obtains monmap and keyring and creates mon data structures.
%% @end
%%--------------------------------------------------------------------
-spec mkfs(#{id := id(), action := Action, _ => _}) -> ok | no_return() when
    Action :: generate | {copy, From :: service:host()}.
mkfs(#{id := Id, action := Action}) ->
    utils:run_with_tempdir(fun(TempDir) ->
        KeyringPath = filename:join(TempDir, "keyring"),
        MonmapPath = filename:join(TempDir, "monmap"),

        case Action of
            generate ->
                create_monmap(MonmapPath, list_instances()),
                create_keyring(KeyringPath);
            {copy, FromHost} ->
                Node = nodes:service_to_node(?SERVICE_PANEL, FromHost),
                {ok, Monmap} = rpc:call(Node, ?MODULE, get_mon_map, [get_id_by_host(FromHost)]),
                file:write_file(MonmapPath, Monmap),

                {ok, Auth} = rpc:call(Node, ?MODULE, get_mon_auth, []),
                file:write_file(KeyringPath, Auth)
        end,

        ceph_cli:mon_mkfs(Id, MonmapPath, KeyringPath)
    end);

mkfs(#{id := _Id, reference_host := ReferenceHost} = Ctx) ->
    Self = hosts:self(),
    case ReferenceHost of
        Self -> mkfs(Ctx#{action => generate});
        _Other -> mkfs(Ctx#{action => {copy, ReferenceHost}})
    end.


-spec start(#{id := id()}) -> ok.
start(#{id := Id} = Ctx) ->
    Host = hosts:self(), % sanity check
    #{ip := Ip, host := Host} = get_instance(Id),
    DataDir = ceph:get_data_dir(mon, Id),
    ok = ceph_cli:mon_start(Id, DataDir, Ip),
    service:register_healthcheck(name(), Ctx),
    ?info("Service ceph_mon (id ~p) started", [Id]).


-spec wait_for_init(#{id := id()}) -> ok | no_return().
wait_for_init(#{id := Id}) ->
    StartAttempts = onepanel_env:get(ceph_mon_wait_for_init_attempts),
    Validator = fun(Running) -> true = lists:member(Id, Running) end,
    onepanel_utils:wait_until(?MODULE, list_quorum, [],
        {validator, Validator}, StartAttempts),
    ok.


-spec stop(#{id := id()}) -> ok.
stop(#{id := Id}) ->
    #{ip := Ip} = get_instance(Id),
    DataDir = ceph:get_data_dir(mon, Id),
    StartedBy = ceph_cli:mon_start_cmd(Id, DataDir, Ip),
    ceph_cli:stop_with_timeout(StartedBy).


%%--------------------------------------------------------------------
%% @doc Adds current host to the list of service hosts.
%% @end
%%--------------------------------------------------------------------
-spec register_host(service:ctx()) -> ok.
register_host(#{id := Id}) ->
    service:store_in_ctx(name(), [instances, Id, deployment_finished], true),
    service:add_host(name(), hosts:self()).


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Returns Ceph Monitors key in keyring format.
%% @end
%%--------------------------------------------------------------------
-spec get_mon_auth() -> {ok, binary()}.
get_mon_auth() ->
    {ok, str_utils:ensure_suffix(ceph_cli:auth_get('mon.'), "\n")}.


%%--------------------------------------------------------------------
%% @doc
%% Returns the monmap. If the cluster is down, extracts the monmap
%% from data directory of Monitor with given Id.
%% @end
%%--------------------------------------------------------------------
-spec get_mon_map(id()) -> {ok, binary()}.
get_mon_map(MonId) ->
    utils:run_with_tempdir(fun(TempDir) ->
        File = filename:join(TempDir, "monmap"),
        try
            ceph_cli:mon_export_monmap(File),
            {ok, _} = file:read_file(File)
        catch _:_ ->
            % fallback which does not require any mon to be online
            ceph_cli:mon_extract_monmap(MonId, File),
            {ok, _} = file:read_file(File)
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Describes Monitor with given Id.
%% @end
%%--------------------------------------------------------------------
-spec get_details(#{id := id()}) -> map().
get_details(#{id := <<Id/binary>>}) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := Details}} -> format_details(Details);
        _ -> ?throw_error(?ERR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Lists known instances of Monitors.
%% @end
%%--------------------------------------------------------------------
-spec list() -> #{monitors := [map()]}.
list() ->
    #{monitors => lists:map(fun format_details/1, list_instances())}.


%%--------------------------------------------------------------------
%% @doc
%% Lists Monitors currently participating in the quorum.
%% Must be executed on a service_ceph host.
%% @end
%%--------------------------------------------------------------------
-spec list_quorum() -> [id()].
list_quorum() ->
    #{<<"quorum_names">> := Ids} = ceph_cli:status(),
    Ids.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec list_instances() -> [ceph:instance()].
list_instances() ->
    case service:get_ctx(name()) of
        #{instances := Instances} -> maps:values(Instances);
        _ -> []
    end.


%% @private
-spec get_instance(id()) -> ceph:instance().
get_instance(Id) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := Instance}} -> Instance;
        _ -> ?throw_error(?ERR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get Id of a Ceph Monitor assigned to given host.
%% @end
%%--------------------------------------------------------------------
-spec get_id_by_host(service:host()) -> id().
get_id_by_host(Host) ->
    Id = list_to_binary(Host),
    case exists(Id) of
        true -> Id;
        false -> ?make_error(?ERR_NOT_FOUND)
    end.


%% @private
-spec get_local_ip(service:host() | node()) -> IP :: binary().
get_local_ip(HostOrNode) ->
    Node = nodes:service_to_node(?SERVICE_PANEL, HostOrNode),
    [IP | _] = rpc:call(Node, onepanel_ip, hostname_ips, []),
    onepanel_ip:ip4_to_binary(IP).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates monmap file to be imported by mon mkfs.
%% @end
%%--------------------------------------------------------------------
-spec create_monmap(file:filename_all(), Monitors :: [service:ctx()]) -> ok.
create_monmap(Path, Monitors) ->
    FSID = ceph:get_cluster_uuid(),
    IdToIp = [{<<"mon.", Id/binary>>, Ip}
        || #{id := Id, ip := Ip} <- Monitors],
    ceph_cli:monmap_create(Path, FSID, IdToIp, true).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates keyring file to be imported by mon mkfs.
%% @end
%%--------------------------------------------------------------------
-spec create_keyring(file:filename_all()) -> ok.
create_keyring(Path) ->
    AdminKeyring = ceph:get_admin_keyring_path(),
    ok = ceph_cli:auth_create_keyring(Path, <<"mon.">>, [{mon, <<"allow *">>}]),
    ok = ceph_cli:auth_import_keyring(Path, AdminKeyring).


%% @private
-spec format_details(map()) -> #{atom() => binary()}.
format_details(Instance) ->
    Details = maps:with([ip, id, host], Instance),
    onepanel_utils:convert(Details, {values, binary}).

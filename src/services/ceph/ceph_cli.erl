%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Wrappers for Ceph shell commands.
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_cli).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("names.hrl").
-include_lib("ctool/include/logging.hrl").

-type token() :: onepanel_shell:token().
-type capability() :: {Entity :: token(), Cap :: token()}.
-type capabilities() :: [capability()].

-export([stop_with_timeout/1]).
-export([status/0, health/0, df/0]).
-export([auth_add/3, auth_get/1, auth_get/2, auth_create_keyring/3,
    auth_import_keyring/2, auth_get_or_create/2, auth_get_or_create_key/2,
    auth_print_key/1]).
-export([osd_create/0, osd_create/2, osd_next_id/0, osd_mkfs_mkkey/1, osd_start/1,
    osd_start_cmd/1, osd_mark/2, osd_purge/1, osd_is_safe_to_destroy/1, osd_df/0]).
-export([pool_create/1, pool_create/2, pool_delete/1, pool_set_application/2,
    list_pools/0]).
-export([set_pool_param/3, get_pool_param/2]).
-export([volume_list/0, volume_prepare_bluestore/2, volume_activate/2]).
-export([mon_mkfs/3, mon_start/3, mon_start_cmd/3]).
-export([monmap_create/4, mon_export_monmap/1, mon_extract_monmap/2]).
-export([mgr_start/1, mgr_start_cmd/1]).

-define(TIMEOUT_MS, onepanel_env:get(ceph_cli_timeout)).
-define(TIMEOUT(Operation),
    ?TIMEOUT(Operation, ?TIMEOUT_MS)).
-define(TIMEOUT(Operation, Timeout),
    utils:timeout(fun() -> Operation end, Timeout)).
-define(TIMEOUT(M, F, A),
    ?TIMEOUT(M, F, A, ?TIMEOUT_MS)).
-define(TIMEOUT(M, F, A, Timeout),
    utils:timeout(M, F, A, Timeout)).

-define(CLUSTER, "--cluster", ceph:get_cluster_name()).

-define(QUOTE(Tokens),
    [onepanel_shell:quote(Token) || Token <- Tokens]).

-define(CEPH(Tokens),
    ?QUOTE(["ceph", ?CLUSTER, "--format", "json" | Tokens])).
-define(VOLUME_LVM(Tokens),
    ?QUOTE(["ceph-volume", ?CLUSTER, "lvm" | Tokens])).
-define(OSD(Tokens),
    ?QUOTE(["ceph-osd", ?CLUSTER | Tokens])).
-define(MON(Tokens),
    ?QUOTE(["ceph-mon", ?CLUSTER | Tokens])).
-define(MGR(Tokens),
    ?QUOTE(["ceph-mgr", ?CLUSTER | Tokens])).
-define(AUTH(Tokens),
    ?QUOTE(["ceph", "auth", ?CLUSTER | Tokens])).
-define(AUTHTOOL(Tokens),
    ?QUOTE(["ceph-authtool", ?CLUSTER | Tokens])).


-spec status() -> #{binary() => json_utils:json_term()}.
status() ->
    json_utils:decode(onepanel_shell:get_success_output(?CEPH(["status"]))).


-spec health() -> {ok, #{binary() => binary()}} | {error, timeout}.
health() ->
    % timeout may occur if monitors do not have quorum
    case ?TIMEOUT(onepanel_shell, get_success_output, [?CEPH(["health"])]) of
        {done, JSON} -> {ok, json_utils:decode(JSON)};
        {error, timeout} -> ?make_error(?ERR_TIMEOUT)
    end.


-spec df() -> map().
df() ->
    Output = onepanel_shell:get_success_output(?CEPH(["df"])),
    json_utils:decode(Output).


%%--------------------------------------------------------------------
%% @doc Takes command which started a process as argument
%% and tries to stop the process by sending a TERM signal.
%% If the process does not disappear before timeout, KILL signal is sent.
%% @end
%%--------------------------------------------------------------------
-spec stop_with_timeout([token()]) -> ok | no_match.
stop_with_timeout(StartCommand) ->
    TimeoutSeconds = onepanel_env:get(ceph_stop_timeout) div 1000,
    onepanel_shell:pkill(StartCommand, 'TERM'),
    try
        onepanel_utils:wait_until(onepanel_shell, process_exists, [StartCommand],
            {validator, fun(Exists) -> false = Exists end},
            TimeoutSeconds, 1000)
    catch
        throw:attempts_limit_exceeded ->
            ?warning("Process ~p did not stop in ~p seconds, sending KILL signal",
                [StartCommand, TimeoutSeconds]),
            onepanel_shell:pkill(StartCommand, 'KILL')
    end.


-spec auth_add(KeyringPath :: token(), User :: token(), capabilities()) -> ok.
auth_add(KeyringPath, User, Caps) ->
    CapsCmd = lists:append([
        [Entity, Cap] || {Entity, Cap} <- Caps
    ]),
    Cmd = ?AUTH(["add", "-i", KeyringPath, User | CapsCmd]),
    onepanel_shell:ensure_success(Cmd).


-spec auth_get_or_create(Username :: token(), capabilities()) -> binary().
auth_get_or_create(User, Caps) ->
    CapsCmd = lists:append([[Entity, Cap] || {Entity, Cap} <- Caps]),
    onepanel_shell:get_success_output(
        ?AUTH(["get-or-create", User | CapsCmd])
    ).


-spec auth_get_or_create_key(Username :: token(), capabilities()) -> binary().
auth_get_or_create_key(User, Caps) ->
    CapsCmd = lists:append([[Entity, Cap] || {Entity, Cap} <- Caps]),
    onepanel_shell:get_success_output(
        ?AUTH(["get-or-create-key", User | CapsCmd])
    ).


-spec auth_get(Entity :: token()) -> binary().
auth_get(Entity) ->
    onepanel_shell:get_success_output(?AUTH(["get", Entity])).


%%--------------------------------------------------------------------
%% @doc Writes given entity's keyring to given location.
%% @end
%%--------------------------------------------------------------------
-spec auth_get(Entity :: token(), OutputFile :: token()) -> ok.
auth_get(Entity, OutputFile) ->
    onepanel_shell:ensure_success(?AUTH(["get", Entity, "-o", OutputFile])).


-spec auth_print_key(Name :: token()) -> Key :: binary().
auth_print_key(Name) ->
    Output = onepanel_shell:get_success_output(?CEPH(["auth", "print-key", Name])),
    #{<<"key">> := Key} = json_utils:decode(Output),
    Key.


-spec auth_create_keyring(Path :: token(), Name :: token(),
    Capabilities :: [{Entity :: token(), Capability :: binary()}]) -> ok.
auth_create_keyring(Path, Name, Capabilities) ->
    BaseCmd = ["--create-keyring", Path, "--gen-key", "-n", Name],
    CapsCmds = [["--cap", Entity, Cap] || {Entity, Cap} <- Capabilities],

    Cmd = lists:append([BaseCmd | CapsCmds]),
    onepanel_shell:ensure_success(?AUTHTOOL(Cmd)).


-spec auth_import_keyring(ModifiedKeyring :: file:filename_all(),
    ImportedKeyring :: file:name_all()) -> ok.
auth_import_keyring(ModifiedKeyring, ImportedKeyring) ->
    onepanel_shell:ensure_success(
        ?AUTHTOOL([ModifiedKeyring, "--import-keyring", ImportedKeyring])
    ).


%%--------------------------------------------------------------------
%% @doc Registers OSD with autoassigned Id.
%% Returns Id of the created OSD.
%% @end
%%--------------------------------------------------------------------
-spec osd_create() -> Id :: binary().
osd_create() ->
    JSON = onepanel_shell:get_success_output(?CEPH(["osd", "create"])),
    #{<<"osdid">> := Id} = json_utils:decode(JSON),
    Id.


%%--------------------------------------------------------------------
%% @doc Registers OSD with given Id.
%% Throws if the Id of newly created OSD is different than expected.
%% @end
%%--------------------------------------------------------------------
-spec osd_create(UUID :: ceph:uuid(), Id :: service_ceph_osd:id()) ->
    ok.
osd_create(UUID, Id) ->
    JSON = onepanel_shell:get_success_output(?CEPH(["osd", "create", UUID, <<"osd.", Id/binary>>])),
    IdInt = binary_to_integer(Id),
    #{<<"osdid">> := IdInt} = json_utils:decode(JSON),
    ok.

%%--------------------------------------------------------------------
%% @doc Returns first available (i.e. higher than any used) Ceph OSD id.
%% @end
%%--------------------------------------------------------------------
-spec osd_next_id() -> service_ceph_osd:id().
osd_next_id() ->
    JSON = onepanel_shell:get_success_output(?CEPH(["osd", "getmaxosd"])),
    #{<<"max_osd">> := Id} = json_utils:decode(JSON),
    integer_to_binary(Id).


-spec osd_mkfs_mkkey(Id :: service_ceph_osd:id()) -> ok | no_return().
osd_mkfs_mkkey(Id) ->
    onepanel_shell:ensure_success(?OSD(["-i", Id, "--mkfs", "--mkkey", "--no-mon-config"])).


-spec osd_start(Id :: service_ceph_osd:id()) -> ok | no_return().
osd_start(Id) ->
    onepanel_shell:ensure_success(osd_start_cmd(Id)).


-spec osd_start_cmd(Id :: service_ceph_osd:id()) -> [token()].
osd_start_cmd(Id) ->
    ?OSD(["-i", Id]).


-spec osd_mark(Id :: service_ceph_osd:id(), State :: down | out) -> ok.
osd_mark(Id, State) ->
    onepanel_shell:ensure_success(?CEPH(["osd", State, Id])).


-spec osd_purge(Id :: service_ceph_osd:id()) -> ok | no_return().
osd_purge(Id) ->
    onepanel_shell:ensure_success(?CEPH([
        "osd", "purge", Id, "--yes-i-really-mean-it"])).


-spec osd_is_safe_to_destroy(Id :: service_ceph_osd:id()) -> boolean().
osd_is_safe_to_destroy(Id) ->
    {Code, _Output, _} = onepanel_shell:execute(?CEPH(["osd", "safe-to-destroy", Id])),
    Code == 0.


-spec osd_df() -> #{binary() := term()}.
osd_df() ->
    Output = onepanel_shell:get_success_output(?CEPH(["osd", "df", "plain"])),
    json_utils:decode(Output).


-spec pool_create(Name :: binary()) -> ok | no_return().
pool_create(Name) ->
    pool_create(Name, onepanel_env:get(ceph_default_pg, ?APP_NAME, 128)).

-spec pool_create(Name :: binary(), PgNum :: pos_integer()) -> ok | no_return().
pool_create(Name, PgNum) ->
    onepanel_shell:ensure_success(
        ?CEPH(["osd", "pool", "create", Name, PgNum, PgNum])
    ).


-spec pool_delete(Name :: binary()) -> ok.
pool_delete(Name) ->
    onepanel_shell:ensure_success(
        ?CEPH(["osd", "pool", "delete", Name, Name, "--yes-i-really-really-mean-it"])
    ).


-spec pool_set_application(Name :: ceph_pool:name(), App :: token()) ->
    ok | no_return().
pool_set_application(Pool, App) ->
    onepanel_shell:ensure_success(
        ?CEPH(["osd", "pool", "application", "enable", Pool, App])
    ).


-spec list_pools() -> [#{name => binary(), number => integer()}] | no_return().
list_pools() ->
    JSON = onepanel_shell:get_success_output(?CEPH(["osd", "lspools"])),
    Pools = json_utils:decode(JSON),
    [#{name => Name, number => Num}
        || #{<<"poolnum">> := Num, <<"poolname">> := Name} <- Pools].


-spec set_pool_param(ceph_pool:name(), Param :: token(), Value :: token()) -> ok.
set_pool_param(Pool, Param, Value) ->
    onepanel_shell:ensure_success(
        ?CEPH(["osd", "pool", "set", Pool, Param, Value])
    ).


-spec get_pool_param(Pool :: binary(), Param :: binary()) ->
    {ok, json_utils:json_term()} | {error, _}.
get_pool_param(Pool, Param) ->
    case onepanel_shell:execute(
        ?CEPH(["osd", "pool", "get", Pool, Param])
    ) of
        {0, Output, _} ->
            #{Param := Value} = json_utils:decode(Output),
            {ok, Value};
        {Code, Output, StdErr} ->
            ?make_error(?ERR_CMD_FAILURE(Code, Output, StdErr))
    end.


%%--------------------------------------------------------------------
%% @doc Returns data in format
%% #{osd-id => [#{matching-device-1}, #{matching-device-2}]}.
%% Multiple devices appear if all of them are tagged (in LVM metadata)
%% with equal OSD id.
%% @end
%%--------------------------------------------------------------------
-spec volume_list() -> json_utils:json_term().
volume_list() ->
    Output = onepanel_shell:get_success_output(?VOLUME_LVM(["list", "--format=json"])),
    json_utils:decode(Output).


%%--------------------------------------------------------------------
%% @doc Partitions a block device for usage by OSD.
%% A device may be a disk or partition path in /dev/ or
%% an LVM reference "vgroup/lvolume".
%% @end
%%--------------------------------------------------------------------
-spec volume_prepare_bluestore(UUID :: ceph:uuid(), Device :: binary()) -> ok.
volume_prepare_bluestore(UUID, Device) ->
    ?info("Formatting ~p for Ceph OSD", [Device]),
    Cmd = ?VOLUME_LVM(["prepare", "--no-systemd", "--bluestore", "--data", Device, "--osd-fsid", UUID]),
    onepanel_shell:ensure_success(Cmd).


%%--------------------------------------------------------------------
%% @doc Activates the OSD. This mounts tmpfs with the OSD metadata
%% at /var/lib/ceph/osd/{cluster}-{id}.
%% @end
%%--------------------------------------------------------------------
-spec volume_activate(service_ceph_osd:id(), binary()) -> ok.
volume_activate(OsdId, OsdUUID) ->
    % ceph-volume will not use provided Id or UUID if only one of them is given
    onepanel_shell:ensure_success(?VOLUME_LVM(
        ["activate", "--no-systemd", OsdId, OsdUUID])).


-spec monmap_create(Path :: binary(), FSID :: ceph:uuid(),
    Monitors, Clobber :: boolean()) -> ok | no_return()
    when Monitors :: [{service_ceph_mon:id(), IP :: binary()}].
monmap_create(Path, FSID, Monitors, Clobber) ->
    BaseCmd = ["monmaptool", "--create", "--fsid", FSID, Path],
    ClobberCmd = case Clobber of
        true -> ["--clobber"];
        _ -> []
    end,
    Adds = lists:append([
        ["--add", MonId, IP] || {MonId, IP} <- Monitors
    ]),

    Cmd = lists:append([BaseCmd, Adds, ClobberCmd]),
    onepanel_shell:ensure_success(Cmd).


-spec mon_mkfs(service_ceph_mon:id(), MonmapPath :: token(),
    KeyringPath :: token()) -> ok | no_return().
mon_mkfs(Name, Monmap, Keyring) ->
    onepanel_shell:ensure_success(
        ?MON(["--mkfs", "-i", Name, "--monmap", Monmap, "--keyring", Keyring])).


-spec mon_start(service_ceph_mon:id(), DataDir :: token(), IP :: token()) -> ok.
mon_start(Id, DataDir, IP) ->
    onepanel_shell:ensure_success(mon_start_cmd(Id, DataDir, IP)).


-spec mon_start_cmd(service_ceph_mon:id(), DataDir :: token(), IP :: token()) ->
    [token()].
mon_start_cmd(Id, DataDir, IP) ->
    ?MON(["-i", Id, "--mon-data", DataDir, "--public-addr", IP]).


%%--------------------------------------------------------------------
%% @doc Exports monmap. To succeed some monitor must be online
%% in the cluster, not necessarily on the current node.
%% @end
%%--------------------------------------------------------------------
-spec mon_export_monmap(OutputPath :: token()) -> ok.
mon_export_monmap(OutputPath) ->
    {done, _} = ?TIMEOUT(
        onepanel_shell:ensure_success(?CEPH(["mon", "getmap", "-o", OutputPath]))
    ),
    ok.


%%--------------------------------------------------------------------
%% @doc Exports monmap. The ceph-mon must be stopped to release lock
%% and allow the exprot.
%% @end
%%--------------------------------------------------------------------
-spec mon_extract_monmap(service_ceph_mon:id(), OutputPath :: token()) -> ok.
mon_extract_monmap(MonId, OutputPath) ->
    onepanel_shell:ensure_success(?MON(["-i", MonId, "--extract-monmap", OutputPath])).


-spec mgr_start(Name :: token()) -> ok.
mgr_start(Name) ->
    onepanel_shell:ensure_success(mgr_start_cmd(Name)).


-spec mgr_start_cmd(Name :: token()) -> [token()].
mgr_start_cmd(Name) ->
    ?MGR(["-i", Name]).

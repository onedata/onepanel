%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains Ceph OSD service management functions.
%%% Ceph OSD may have type 'blockdevice' or 'loopdevice'. Blockdevice
%%% means formatting a whole block device (disk or format).
%%% Loopdevice uses the same Ceph object store ("Bluestore") under the hood,
%%% but instead of using a physical block device, onepanel creates a file
%%% of given size and mounts it as a loopdevice.
%%%
%%% During deployment an OSD is identified by its UUID. This is because
%%% ids should be contiguous and are left to be assigned by Ceph.
%%% For other purposes (such as selecting the OSD in API requests) the integer
%%% id is used, as it is most commonly present in Ceph CLI responses.
%%% @end
%%%--------------------------------------------------------------------
-module(service_ceph_osd).
-author("Wojciech Geisler").
-behaviour(service_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("service.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").

% @formatter:off
-type id() :: binary().
-type blockdevice_instance() :: #{type := blockdevice, uuid := ceph:uuid(),
    id => id(), device := binary(), _ => _}.
-type loopdevice_instance() :: #{type := loopdevice, uuid := ceph:uuid(),
    id => id(), path := binary(), device := binary(), % device: vgroup/lvolume
    size := bytes(), _ => _}.
-type type() :: loopdevice | blockdevice.
-type bytes() :: integer().
-type usage() :: #{total := bytes(), used := bytes(), available := bytes()}.
% @formatter:on

-export_type([id/0, type/0, usage/0]).

%% Service behaviour callbacks
-export([name/0, get_hosts/0, get_nodes/0, get_steps/2]).

%% API functions
-export([list/0, exists/1, count/0]).

%% Step functions
-export([prepare_loopdevice/1, resume_loopdevice/1]).
-export([write_bootstrap_keyring/1, format_block_device/1,
    mark_deployed/1, start/1, stop/1, status/1]).
-export([get_details/1, get_disks/1, get_usage/0, get_usage_by_id/1]).

%%%===================================================================
%%% Service behaviour callbacks
%%%===================================================================

-spec name() -> Name :: service:name().
name() ->
    ?SERVICE_CEPH_OSD.


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
get_steps(deploy_all, #{osds := Ctxs}) ->
    AssignedPaths = assign_paths(Ctxs),
    UniqueUUIDs = ceph:filter_out_existing_instances(name(), AssignedPaths, uuid),
    UniquePaths = ceph:filter_out_existing_instances(name(), UniqueUUIDs, path),
    service_utils:for_each_ctx(UniquePaths, [#steps{action = deploy}]);

get_steps(deploy, #{hosts := [_]}) ->
    [
        #steps{action = prepare},
        #steps{action = start},
        #step{function = mark_deployed}
    ];

get_steps(prepare, #{hosts := [_]} = _Ctx) ->
    [
        #step{function = write_bootstrap_keyring},
        #steps{action = prepare_storage}
    ];

get_steps(prepare_storage, #{type := loopdevice}) ->
    [
        #step{function = prepare_loopdevice}
    ];

get_steps(prepare_storage, #{type := blockdevice}) ->
    [
        #step{function = format_block_device}
    ];

get_steps(resume_all, _Ctx) ->
    service_utils:for_each_ctx(list_instances(),
        [#steps{action = resume}]);

get_steps(resume, _Ctx) ->
    [#steps{action = start}];

get_steps(get_disks, #{hosts := [_]} = _Ctx) ->
    % host should be provided in rest request.
    [#step{function = get_disks}];

get_steps(get_usage_by_id = Action, #{id := Id}) ->
    [
        #step{function = Action, args = [Id], hosts = [ceph:id_to_host(name(), Id)]}
    ];

get_steps(start, #{uuid := _, type := Type, host := Host}) ->
    [
        #step{function = resume_loopdevice, condition = Type == loopdevice, hosts = [Host]},
        #step{function = start, hosts = [Host]}
    ];

get_steps(stop_all, _Ctx) ->
    service_utils:for_each_ctx(list_instances(), [
        #steps{action = stop}
    ]);

get_steps(stop, #{uuid := UUID}) ->
    #{host := Host} = get_instance({uuid, UUID}),
    [#step{function = stop, hosts = [Host]}];

get_steps(Action, _Ctx) when
    Action == get_details;
    Action == get_usage ->
    [#step{function = Action, selection = any}];

get_steps(NoArgsAction, _Ctx) when
    NoArgsAction == list ->
    [#step{function = NoArgsAction, selection = any, args = []}].


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Lists details of all OSDs.
%% @end
%%--------------------------------------------------------------------
-spec list() -> #{osds := [#{atom() := term()}]}.
list() ->
    Instances = maps:get(instances, get_ctx(), #{}),
    Ids = maps:keys(Instances),
    #{osds => [get_details(#{id => Id}) || Id <- Ids]}.


-spec exists(id()) -> boolean().
exists(Id) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := _}} -> true;
        _ -> false
    end.


%%--------------------------------------------------------------------
%% @doc Returns number of OSD instances.
%% @end
%%--------------------------------------------------------------------
-spec count() -> non_neg_integer().
count() ->
    maps:size(maps:get(instances, get_ctx(), #{})).


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Writes bootstrap-osd keyring in location expected by ceph-volume.
%% @end
%%--------------------------------------------------------------------
write_bootstrap_keyring(_Ctx) ->
    ClusterName = ceph:get_cluster_name(),
    Path = <<"/var/lib/ceph/bootstrap-osd/", ClusterName/binary, ".keyring">>,
    ceph_cli:auth_get(<<"client.bootstrap-osd">>, Path).


%%--------------------------------------------------------------------
%% @doc Allocates file for a loopdevice, sets up /dev/loopX path
%% and formats LVM volume on it.
%% @end
%%--------------------------------------------------------------------
prepare_loopdevice(#{uuid := UUID, size := Size} = Ctx) ->
    Path = maps:get(path, Ctx, gen_loopdevice_path(UUID)),
    Loop = loopdevice:ensure_loopdevice(Path, Size),
    GroupName = gen_vgroup_name(UUID),
    VolumeName = gen_lvolume_name(UUID),

    lvm:create_physical_volume(Loop),
    lvm:create_volume_group(GroupName, [Loop]),
    lvm:create_logical_volume(VolumeName, GroupName),

    Device = <<GroupName/binary, "/", VolumeName/binary>>,
    format_block_device(Ctx#{device => Device}),
    Id = uuid_to_id(UUID),

    service:store_in_ctx(name(), [instances, Id, size], Size),
    service:store_in_ctx(name(), [instances, Id, path], Path).


%%--------------------------------------------------------------------
%% @doc Formats block devices for use with ceph OSD.
%% DESTROYS ALL DATA existing on given block devices!
%% Device may be given as "/dev/xxxx" path or LVM's "vgroup/lvolume"
%% Given device will become LVM physical volume, which is recommended
%% to be a partition rather than a raw disk, although both
%% options will work.
%% @end
%%--------------------------------------------------------------------
-spec format_block_device(service:ctx()) -> ok .
format_block_device(#{type := Type, device := Device, uuid := UUID} = Ctx) ->
    ceph_cli:volume_prepare_bluestore(UUID, Device),
    {ok, Id} = obtain_id_by_uuid(UUID),

    service:store_in_ctx(name(), [uuid_to_id, UUID], Id),
    service:store_in_ctx(name(), [instances, Id],
        #{
            id => Id,
            uuid => UUID,
            host => hosts:self(),
            type => Type,
            deployment_finished => false,
            device => Device
        }).


%%--------------------------------------------------------------------
%% @private
%% @doc Queries ceph-volume to determine id assigned to OSD
%% created with given UUID.
%% @end
%%--------------------------------------------------------------------
-spec obtain_id_by_uuid(binary()) -> {ok, id()} | #error{}.
obtain_id_by_uuid(UUID) ->
    Output = ceph_cli:volume_list(),
    OsdsList = lists:append(maps:values(Output)),

    Matching = lists:filtermap(fun(#{<<"tags">> := Tags}) ->
        case Tags of
            #{<<"ceph.osd_fsid">> := UUID, <<"ceph.osd_id">> := OsdId} ->
                {true, OsdId};
            _ ->
                false
        end
    end, OsdsList),
    case Matching of
        [] -> ?make_error(?ERR_NOT_FOUND);
        [Id] -> {ok, Id};
        _Multiple -> ?make_error(?ERR_AMBIGUOUS_UUID)
    end.


%%--------------------------------------------------------------------
%% @doc Makes sure data file of loopdevice OSD is set up as a loopdevice
%% and triggers LVM rescan to restore lvolume configured on it.
%% @end
%%--------------------------------------------------------------------
-spec resume_loopdevice(service:ctx()) -> ok.
resume_loopdevice(#{uuid := UUID}) ->
    #{path := Path, size := Size, device := VgLv} = get_instance({uuid, UUID}),
    loopdevice:ensure_loopdevice(Path, Size),
    lvm:scan_volume_groups(),
    lvm:enable_logical_volume(VgLv).


%%--------------------------------------------------------------------
%% @doc Starts the OSD.
%% @end
%%--------------------------------------------------------------------
-spec start(#{id | uuid := binary()}) -> ok.
start(#{uuid := UUID, id := Id} = Ctx) ->
    ceph_cli:volume_activate(Id, UUID),
    ceph_cli:osd_start(Id),
    service:register_healthcheck(name(), Ctx),
    ?info("Service ceph_osd (id ~p) started", [Id]);

start(#{uuid := UUID} = Ctx) ->
    start(Ctx#{id => uuid_to_id(UUID)}).


-spec stop(#{id | uuid := binary()}) -> ok.
stop(#{id := Id}) ->
    ceph_cli:stop_with_timeout(ceph_cli:osd_start_cmd(Id));

stop(#{uuid := UUID} = Ctx) ->
    stop(Ctx#{id => uuid_to_id(UUID)}).


%%--------------------------------------------------------------------
%% @doc Status used by service healthcheck.
%% @end
%%--------------------------------------------------------------------
-spec status(#{id := id()}) -> service:status().
status(#{id := Id}) ->
    StartedBy = ceph_cli:osd_start_cmd(Id),
    case onepanel_shell:process_exists(StartedBy) of
        true -> healthy;
        false -> stopped
    end.


-spec mark_deployed(service:ctx()) -> ok.
mark_deployed(#{uuid := UUID}) ->
    Id = uuid_to_id(UUID),
    service:store_in_ctx(name(), [instances, Id, deployment_finished], true),
    service:add_host(name(), hosts:self()).


-spec get_details(#{id := id()}) -> #{atom() := binary()}.
get_details(#{id := Id}) ->
    Instances = maps:get(instances, get_ctx(), #{}),
    Details = onepanel_maps:get_store_multiple([
        {id, id}, {path, path}, {type, type}, {host, host},
        {device, device}, {uuid, uuid}
    ], maps:get(Id, Instances)),

    Details#{
        host => list_to_binary(maps:get(host, Details)),
        id => Id
    }.


-spec get_disks(service:ctx()) -> #{blockDevices := [#{atom() := term()}]}.
get_disks(_Ctx) ->
    Disks = onepanel_block_device:get_devices(),
    Host = hosts:self(),
    #{blockDevices => [
        Disk#{host => list_to_binary(Host)} || Disk <- Disks
    ]}.


-spec get_usage_by_id(id()) -> usage().
get_usage_by_id(Id) ->
    maps:get(Id, get_usage()).


%%--------------------------------------------------------------------
%% @doc Returns space usage for all OSDs.
%% @end
%%--------------------------------------------------------------------
-spec get_usage() -> #{id() => usage()}.
get_usage() ->
    Map = ceph_cli:osd_df(),
    Osds = maps:get(<<"nodes">>, Map),
    maps:from_list(lists:map(fun(#{<<"id">> := Id} = Osd) ->
        Total = maps:get(<<"kb">>, Osd) * 1024,
        Used = maps:get(<<"kb_used">>, Osd) * 1024,
        Available = maps:get(<<"kb_avail">>, Osd) * 1024,
        IdBin = integer_to_binary(Id),
        {IdBin, #{total => Total, used => Used, available => Available}}
    end, Osds)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_ctx() -> service:ctx() | #error{}.
get_ctx() ->
    service:get_ctx(name()).


%% @private
-spec list_instances() -> [ceph:instance()].
list_instances() ->
    case service:get_ctx(name()) of
        #{instances := Instances} -> maps:values(Instances);
        _ -> []
    end.


%% @private
-spec get_instance({id, id()} | {uuid, ceph:uuid()}) ->
    blockdevice_instance() | loopdevice_instance().
get_instance({id, Id}) ->
    case service:get_ctx(name()) of
        #{instances := #{Id := Instance}} -> Instance;
        _ -> ?throw_error(?ERR_NOT_FOUND)
    end;

get_instance({uuid, UUID}) ->
    get_instance({id, uuid_to_id(UUID)}).


%%--------------------------------------------------------------------
%% @private
%% @doc Generates loopdevice file paths where missing.
%% @end
%%--------------------------------------------------------------------
-spec assign_paths([#{type := type(), path => binary(), K => V}]) ->
    [#{type := type(), path := binary(), K => V}].
assign_paths(Ctxs) ->
    lists:map(fun
        (#{type := blockdevice} = Ctx) -> Ctx;
        (#{type := loopdevice, path := _} = Ctx) -> Ctx;
        (#{type := loopdevice, uuid := UUID} = Ctx) ->
            Ctx#{path => gen_loopdevice_path(UUID)}
    end, Ctxs).


%%--------------------------------------------------------------------
%% @private
%% @doc Find Id by UUID.
%% @end
%%--------------------------------------------------------------------
-spec uuid_to_id(binary()) -> id() | no_return().
uuid_to_id(UUID) ->
    #{uuid_to_id := #{UUID := Id}} = get_ctx(),
    Id.


%% @private
-spec gen_loopdevice_path(ceph:uuid()) -> binary().
gen_loopdevice_path(UUID) ->
    <<"/volumes/persistence/ceph-loopdevices/osd-", UUID/binary, ".loop">>.


%% @private
-spec gen_vgroup_name(ceph:uuid()) -> binary().
gen_vgroup_name(UUID) ->
    <<"osd-", UUID/binary>>.


%% @private
-spec gen_lvolume_name(ceph:uuid()) -> binary().
gen_lvolume_name(UUID) ->
    <<"osd-data-", UUID/binary>>.

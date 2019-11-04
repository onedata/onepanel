%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for mounting and formatting block devices.
%%% Assumes onepanel has root permissions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_block_device).
-author("Wojciech Geisler").

% @formatter:off
-type bytes() :: non_neg_integer().
-type device_path() :: binary().
-type device_desc() :: #{
    type := binary(), path := device_path(), size := bytes(), mounted := boolean()
}.
% formatter:on

-export([get_devices/0, mount/2, is_blockdevice/1]).


%%--------------------------------------------------------------------
%% @doc Lists block devices - disks and partitions - present in the system
%% for use by Ceph OSD.
%% @end
%%--------------------------------------------------------------------
-spec get_devices() -> [device_desc()].
get_devices() ->
    Output = onepanel_shell:get_success_output(["lsblk",
        "-J", % JSON output
        "-b", % size in bytes
        "-p"  % full paths to device files
    ]),
    Map = json_utils:decode(Output),
    Parsed = parse_devices_tree(maps:get(<<"blockdevices">>, Map, [])),
    Devices = filter_devices_supported_by_ceph_volume(Parsed),
    filter_unavailable(Devices).


%%--------------------------------------------------------------------
%% @doc Filters out disks returned by lsblk but not actually visible
%% in /dev/. This is usually caused by incorrectly set up docker
%% privileges/mounts.
%% @end
%%--------------------------------------------------------------------
-spec filter_unavailable([device_desc()]) -> [device_desc()].
filter_unavailable(Disks) ->
    [Disk || #{path := Path} = Disk <- Disks, is_blockdevice(Path)].


%%--------------------------------------------------------------------
%% @doc Calls the `mount` command.
%% @end
%%--------------------------------------------------------------------
-spec mount(Device :: binary(), Mountpoint :: binary()) -> ok | no_return().
mount(Device, Mountpoint) ->
    onepanel_shell:ensure_success(["mount", Device, Mountpoint]).


-spec is_blockdevice(Path :: binary()) -> boolean().
is_blockdevice(Path) ->
    PathToken = onepanel_shell:quote(Path),
    {Code, _, _} = onepanel_shell:execute(["test", "-b", PathToken]),
    Code == 0.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Processes lsblk output and returns a flat list of block devices.
%% Uses the tree hierarchy in heuristic determining if a device
%% is used (mounted or used for lvolume).
%% @end
%%--------------------------------------------------------------------
-spec parse_devices_tree([Blockdevice]) -> [device_desc()] when
    Blockdevice :: #{binary() => [Blockdevice] | term()}.
parse_devices_tree(Blockdevices) ->
    lists:flatmap(fun(Device) ->
        case parse_blockdevice(Device) of
            {true, Description} ->
                Children = maps:get(<<"children">>, Device, []),
                [Description | parse_devices_tree(Children)];
            false -> []
        end
    end, Blockdevices).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filters device list to only include types which are accepted by
%% ceph-volume tool for OSD creation.
%% @end
%%--------------------------------------------------------------------
-spec filter_devices_supported_by_ceph_volume([Device]) -> [Device] when
    Device :: #{type => binary(), _ => _}.
filter_devices_supported_by_ceph_volume(Devices) ->
    lists:filter(fun
        (#{type := Type}) -> lists:member(Type, [<<"disk">>, <<"part">>]);
        (_) -> false
    end, Devices).



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Translates blockdevice JSON description as returned by lsblk
%% into internal format. Returns 'false' if required fields were
%% not found in the description.
%% @end
%%--------------------------------------------------------------------
-spec parse_blockdevice(map()) -> {true, device_desc()} | false.
parse_blockdevice(#{<<"name">> := Name, <<"type">> := Type, <<"size">> := Size} = Map) ->
    {true, #{
        path => Name,
        size => binary_to_integer(Size),
        mounted => is_used(Map),
        type => Type
    }};

% silently skip devices in unexpected format
parse_blockdevice(_) -> false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to determine if given block device or mounted
%% or support a logical volume.
%% Warning: will not detect a device being part of LVM vgroup
%% when the group does not contain a lvolume.
%% @end
%%--------------------------------------------------------------------
-spec is_used(map()) -> boolean().
is_used(#{<<"mountpoint">> := Mount}) when is_binary(Mount) ->
    true;

is_used(#{<<"children">> := Children}) ->
    lists:any(fun(#{<<"type">> := Type} = Child) ->
        % check type: for example, type "lvm" indicates this device supports a logical volume
        Type /= <<"part">> orelse is_used(Child)
    end, Children);

is_used(_) ->
    false.

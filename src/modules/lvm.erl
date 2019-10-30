%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for LVM management.
%%% @end
%%%--------------------------------------------------------------------
-module(lvm).
-author("Wojciech Geisler").

-include("modules/errors.hrl").

-type device() :: binary(). % path in /dev/

-export([create_physical_volume/1, remove_physical_volume/1,
    create_volume_group/2, create_logical_volume/2]).
-export([enable_logical_volume/1, disable_volume_group/1, remove_volume_group/1,
    scan_volume_groups/0]).

-define(QUOTE(Tokens), lists:map(fun onepanel_shell:quote/1, Tokens)).

%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates physical volume, that is formats given disk or partition
%% for use by lvm.
%% @end
%%--------------------------------------------------------------------
-spec create_physical_volume(device()) -> ok.
create_physical_volume(Device) ->
    onepanel_shell:ensure_success(?QUOTE(["pvcreate", Device])).


%%--------------------------------------------------------------------
%% @doc Removs physical volume.
%% @end
%%--------------------------------------------------------------------
-spec remove_physical_volume(device()) -> ok.
remove_physical_volume(Device) ->
    onepanel_shell:ensure_success(?QUOTE(["pvremove", "-y", Device])).


%%--------------------------------------------------------------------
%% @doc Creates volume group baked by given physical volumes.
%% @end
%%--------------------------------------------------------------------
-spec create_volume_group(GroupName :: binary(), [device()]) -> ok.
create_volume_group(GroupName, PhysicalVolumes) ->
    onepanel_shell:ensure_success(?QUOTE(
        ["vgcreate", GroupName | PhysicalVolumes]
    )).


%%--------------------------------------------------------------------
%% @doc Changes volume group state to disabled.
%% @end
%%--------------------------------------------------------------------
-spec disable_volume_group(GroupName :: binary()) -> ok.
disable_volume_group(GroupName) ->
    onepanel_shell:ensure_success(?QUOTE(["vgchange", "-a", "n", GroupName])).


-spec remove_volume_group(GroupName :: binary()) -> ok.
remove_volume_group(GroupName) ->
    onepanel_shell:ensure_success(?QUOTE(["vgremove", "-f", GroupName])).


%%--------------------------------------------------------------------
%% @doc Creates logical volume spanning whole volume group.
%% @end
%%--------------------------------------------------------------------
-spec create_logical_volume(VolumeName :: binary(), GroupName :: binary()) -> ok.
create_logical_volume(VolumeName, GroupName) ->
    onepanel_shell:ensure_success(?QUOTE(
        ["lvcreate", "-l", "100%FREE", "-n", VolumeName, GroupName]
    )).


%%--------------------------------------------------------------------
%% @doc Changes logical volume state to enabled.
%% @end
%%--------------------------------------------------------------------
-spec enable_logical_volume(GroupVolume :: binary()) -> ok.
enable_logical_volume(GroupVolume) ->
    onepanel_shell:ensure_success(?QUOTE(["lvchange", "-a", "y", GroupVolume])).


%%--------------------------------------------------------------------
%% @doc Looks for existing volume groups. Triggers /dev/ entries creation.
%% @end
%%--------------------------------------------------------------------
scan_volume_groups() ->
    onepanel_shell:ensure_success(["vgscan", "--mknodes"]).



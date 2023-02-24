%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for creation and mounting of loopdevices. A loopdevice
%%% is a single file emulating a block device.
%%%
%%% Note that in docker environments losetup detects loopdevices
%%% backed by files from other containers, especially if file of the same
%%% name is not present in the current container.
%%% @end
%%%--------------------------------------------------------------------
-module(loopdevice).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("ctool/include/posix/errno.hrl").

-type bytes() :: integer().
-type device_path() :: binary(). % /dev/loopX path

-export([ensure_loopdevice/2, detach_loopdevice/1]).
-export([list_loopdevices/1]).

-define(QUOTE(Var), shell_utils:quote(Var)).
-define(LOCK_ID, <<"get_highest_loopdevice_number_lock_id">>).


%%%===================================================================
%%% Public API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Makes sure there is a file at given path and is set up
%% as a loopdevice. Returns /dev/loopX path of the loopdevice.
%% @end
%%--------------------------------------------------------------------
-spec ensure_loopdevice(Path :: binary(), Size :: bytes()) -> device_path().
ensure_loopdevice(Path, Size) ->
    case list_loopdevices(Path) of
        [Device | _] ->
            Device;
        _ ->
            ensure_file(Path, Size),
            losetup(Path)
    end.


-spec detach_loopdevice(device_path()) -> ok.
detach_loopdevice(Device) ->
    shell_utils:ensure_success(["losetup", "-d", ?QUOTE(Device)]).


%%--------------------------------------------------------------------
%% @doc Lists loopdevices baked by given file.
%% @end
%%--------------------------------------------------------------------
-spec list_loopdevices(Path :: binary()) -> [device_path()].
list_loopdevices(Path) ->
    % check if the file is visible locally, otherwise losetup
    % may incorrectly list loop devices set up in other containers
    case filelib:is_regular(Path) of
        true ->
            Output = shell_utils:get_success_output(["losetup",
                "--list", "-J", % JSON output
                "-j", % show devices associated with file
                ?QUOTE(Path)]),
            case string:is_empty(string:trim(Output)) of
                true ->
                    [];
                false ->
                    Map = json_utils:decode(Output),
                    List = maps:get(<<"loopdevices">>, Map, []),
                    [P || #{<<"name">> := P} <- List,
                        onepanel_block_device:is_blockdevice(P)]
            end;
        false ->
            []
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Ensures given file exists and has at least given size.
%% @end
%%--------------------------------------------------------------------
-spec ensure_file(Path :: binary(), Size :: bytes()) -> ok.
ensure_file(Path, Size) ->
    case {filelib:is_file(Path), filelib:is_regular(Path)} of
        {true, false} -> throw(?ERROR_FILE_ACCESS(Path, ?EEXIST));
        {true, true} -> fallocate(Path, Size);
        {false, _} -> fallocate(Path, Size)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Creates file of given size. If the file already exists,
%% it is ensured to be at least Size bytes.
%% @end
%%--------------------------------------------------------------------
-spec fallocate(Path :: binary(), Size :: bytes()) -> ok.
fallocate(Path, Size) ->
    ok = filelib:ensure_dir(Path),
    shell_utils:ensure_success(["fallocate",
        "--posix", % never fail, fall back to writing zeros
        "-l", Size, ?QUOTE(Path)]),

    % ensure desired size - `fallocate --posix` does not indicate
    % error in cases like too small available space
    case filelib:file_size(Path) of
        Smaller when Smaller < Size ->
            catch file:delete(Path),
            throw(?ERROR_FILE_ALLOCATION(Smaller, Size));
        _ ->
            ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Creates loopdevice from file.
%% Sometimes losetup fails to dynamically create loop device.
%% Therefore, in case of error device has to be created manually.
%% @end
%%--------------------------------------------------------------------
-spec losetup(Path :: binary()) -> device_path().
losetup(Path) ->
    global:set_lock({?LOCK_ID, self()}),
    DevicePath = case shell_utils:execute(["losetup",
        "--find", % use first available device path
        "--show", % print the assigned device path
        ?QUOTE(Path)
    ]) of
        {0, Path, _} ->
            Path;
        _Failure ->
            NewLoopDeviceNumber = integer_to_list(get_highest_loopdevice_number() + 1),
            NewLoopDevice = "/dev/loop" ++ NewLoopDeviceNumber,
            shell_utils:get_success_output(["mknod", ?QUOTE(NewLoopDevice), "b", "7", ?QUOTE(NewLoopDeviceNumber)]),
            shell_utils:get_success_output([
                "losetup", "--show",
                ?QUOTE(NewLoopDevice),
                ?QUOTE(Path)
            ])
    end,
    global:del_lock({?LOCK_ID, self()}),
    DevicePath.


-spec get_highest_loopdevice_number() -> integer().
get_highest_loopdevice_number() ->
    case shell_utils:execute(["ls /dev | grep loop | sed 's/^loop//' | sort -n | tail -1"]) of
        {0, BinaryResponse, _} ->
            binary_to_integer(BinaryResponse);
        _Failure ->
            1
    end.



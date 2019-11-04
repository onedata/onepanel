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

-type bytes() :: integer().
-type device_path() :: binary(). % /dev/loopX path

-export([ensure_loopdevice/2, detach_loopdevice/1]).
-export([list_loopdevices/1]).

-define(QUOTE(Var), onepanel_shell:quote(Var)).


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
    onepanel_shell:ensure_success(["losetup", "-d", ?QUOTE(Device)]).


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
            Output = onepanel_shell:get_success_output(["losetup",
                "--list", "-J", % JSON output
                "-j", % show devices associated with file
                ?QUOTE(Path)]),
            case string:is_empty(string:trim(Output)) of
                true ->
                    [];
                false ->
                    Map = json_utils:decode(Output),
                    List = maps:get(<<"loopdevices">>, Map, []),
                    [Path || #{<<"name">> := Path} <- List,
                        onepanel_block_device:is_blockdevice(Path)]
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
        {true, false} -> ?throw_error(?ERR_FILE_ACCESS(Path, not_regular));
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
    onepanel_shell:ensure_success(["fallocate",
        "--posix", % never fail, fall back to writing zeros
        "-l", Size, ?QUOTE(Path)]),
    case filelib:file_size(Path) of
        Smaller when Smaller < Size ->
            % fallocate does not indicate error for example when
            % desired size did not fit on the filesystem
            ?throw_error(?ERR_FILE_ALLOCATION_FAILURE(Smaller, Size));
        _ -> ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Creates loopdevice from file.
%% @end
%%--------------------------------------------------------------------
-spec losetup(Path :: binary()) -> device_path().
losetup(Path) ->
    onepanel_shell:get_success_output(["losetup",
        "--find", % use first available device path
        "--show", % print the assigned device path
        ?QUOTE(Path)
    ]).

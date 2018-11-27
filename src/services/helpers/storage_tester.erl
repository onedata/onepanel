%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains helper function for verifying
%%% access to op worker storage.
%%% @end
%%%--------------------------------------------------------------------
-module(storage_tester).
-author("Wojciech Geisler").

-include("modules/errors.hrl").

%% API
-export([verify_storage/2]).

%%--------------------------------------------------------------------
%% @private @doc Verifies that storage is accessible for reading and writing
%% on all op_worker nodes.
%% @end
%%--------------------------------------------------------------------
-spec verify_storage(Helper :: any(), UserCtx :: any()) ->
    ok | no_return().
verify_storage(Helper, UserCtx) ->
    [Node | Nodes] = service_op_worker:get_nodes(),
    {FileId, FileContent} = create_test_file(Node, Helper, UserCtx),
    {FileId2, FileContent2} = verify_test_file(
        Nodes, Helper, UserCtx, FileId, FileContent
    ),
    read_test_file(Node, Helper, UserCtx, FileId2, FileContent2),
    remove_test_file(Node, Helper, UserCtx, FileId2, size(FileContent2)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Checks whether storage is read/write accessible for all
%% op_worker service nodes.
%% On each node the test file is read, removed and new file is created
%% for the next node to be read.
%% @end
%%--------------------------------------------------------------------
-spec verify_test_file(Nodes :: [node()], Helper :: any(), UserCtx :: any(),
    FileId :: binary(), FileContent :: binary()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
verify_test_file([], _Helper, _UserCtx, FileId, FileContent) ->
    {FileId, FileContent};

verify_test_file([Node | Nodes], Helper, UserCtx, FileId, FileContent) ->
    read_test_file(Node, Helper, UserCtx, FileId, FileContent),
    remove_test_file(Node, Helper, UserCtx, FileId, size(FileContent)),
    {FileId2, FileContent2} = create_test_file(Node, Helper, UserCtx),
    verify_test_file(Nodes, Helper, UserCtx, FileId2, FileContent2).


%%--------------------------------------------------------------------
%% @private @doc Creates storage test file.
%% @end
%%--------------------------------------------------------------------
-spec create_test_file(Node :: node(), Helper :: any(), UserCtx :: any()) ->
    {FileId :: binary(), FileContent :: binary()} | no_return().
create_test_file(Node, Helper, UserCtx) ->
    FileId = rpc:call(Node, storage_detector, generate_file_id, []),
    Args = [Helper, UserCtx, FileId],
    case rpc:call(Node, storage_detector, create_test_file, Args) of
        <<_/binary>> = FileContent ->
            {FileId, FileContent};
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_CREATE, Node, Reason}, undefined, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Reads storage test file.
%% @end
%%--------------------------------------------------------------------
-spec read_test_file(Node :: node(), Helper :: any(), UserCtx :: any(),
    FileId :: binary(), FileContent :: binary()) -> ok | no_return().
read_test_file(Node, Helper, UserCtx, FileId, FileContent) ->
    Args = [Helper, UserCtx, FileId],
    ActualFileContent = rpc:call(Node, storage_detector, read_test_file, Args),

    case ActualFileContent of
        FileContent ->
            ok;
        <<_/binary>> ->
            ?throw_error({?ERR_STORAGE_TEST_FILE_READ, Node,
                {invalid_content, FileContent, ActualFileContent}});
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_READ, Node, Reason}, undefined, Stacktrace)
    end.


%%--------------------------------------------------------------------
%% @private @doc Removes storage test file.
%% @end
%%--------------------------------------------------------------------
-spec remove_test_file(Node :: node(), Helper :: any(), UserCtx :: any(),
    FileId :: binary(), Size :: non_neg_integer()) -> ok | no_return().
remove_test_file(Node, Helper, UserCtx, FileId, Size) ->
    Args = [Helper, UserCtx, FileId, Size],
    case rpc:call(Node, storage_detector, remove_test_file, Args) of
        {badrpc, {'EXIT', {Reason, Stacktrace}}} ->
            ?throw_stacktrace({?ERR_STORAGE_TEST_FILE_REMOVE, Node, Reason}, undefined, Stacktrace);
        _ -> ok
    end.



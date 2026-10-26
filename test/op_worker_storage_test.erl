%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests of what a storage removal does with op-worker's refusal. The
%%% middleware checks that the storage supports no space before the removal,
%%% so op-worker refuses only when a support appears in between or Onezone
%%% fails the delete, which no REST request can make happen on purpose.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_storage_test).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STORAGE_ID, <<"someStorageId">>).


%%%===================================================================
%%% Tests
%%%===================================================================


remove_passes_on_op_workers_storage_in_use_error_test_() ->
    Error = ?ERR_STORAGE_IN_USE(?err_ctx()),
    {setup, fun() -> mock_safe_remove(Error) end, fun unmock/1, fun() ->
        ?assertThrow(Error, op_worker_storage:remove(node(), ?STORAGE_ID))
    end}.


remove_passes_on_op_workers_other_errors_test_() ->
    Error = ?ERR_NO_CONNECTION_TO_ONEZONE(?err_ctx(), <<"zone.example.com">>),
    {setup, fun() -> mock_safe_remove(Error) end, fun unmock/1, fun() ->
        ?assertThrow(Error, op_worker_storage:remove(node(), ?STORAGE_ID))
    end}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
mock_safe_remove(Result) ->
    meck:new(op_worker_rpc, [no_link]),
    meck:expect(op_worker_rpc, storage_safe_remove, fun(_Node, _StorageId) -> Result end).


%% @private
unmock(_) ->
    meck:unload(op_worker_rpc).

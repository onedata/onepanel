%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests of op_worker_storage: what a storage removal and update return for
%%% op-worker's answers.
%%% @end
%%%-------------------------------------------------------------------
-module(op_worker_storage_test).
-author("Bartosz Walkowicz").

-include_lib("ctool/include/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STORAGE_ID, <<"someStorageId">>).
-define(STORAGE_DETAILS, #{id => ?STORAGE_ID, name => <<"someStorage">>, type => <<"posix">>}).


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


successful_update_reports_passed_verification_test_() ->
    {setup, fun() -> mock_update(ok) end, fun unmock_all/1, fun() ->
        ?assertEqual(
            ?STORAGE_DETAILS#{verificationPassed => true},
            op_worker_storage:update(?STORAGE_ID, #{})
        )
    end}.


%% The storage test runs before op-worker changes anything, so the storage is
%% left as it was.
failed_storage_test_is_reported_as_failed_verification_test_() ->
    Error = ?ERR_STORAGE_TEST_FAILED(?err_ctx(), access),
    {setup, fun() -> mock_update(Error) end, fun unmock_all/1, fun() ->
        ?assertEqual(
            ?STORAGE_DETAILS#{verificationPassed => false},
            op_worker_storage:update(?STORAGE_ID, #{})
        )
    end}.


update_passes_on_op_workers_refusal_test_() ->
    Error = ?ERR_MISSING_REQUIRED_VALUE(?err_ctx(), <<"credentials">>),
    {setup, fun() -> mock_update(Error) end, fun unmock_all/1, fun() ->
        ?assertThrow(Error, op_worker_storage:update(?STORAGE_ID, #{}))
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


%% @private
mock_update(Result) ->
    meck:new(onedata_logger, [passthrough, no_link]),
    meck:expect(onedata_logger, should_log, fun(_Loglevel) -> true end),
    meck:expect(onedata_logger, log, fun(_Loglevel, _Metadata, _FormattedLog) -> ok end),
    meck:new(op_worker_rpc, [no_link]),
    meck:expect(op_worker_rpc, storage_update, fun(_StorageId, _UpdateSpec) -> Result end),
    meck:new(op_worker_storage, [passthrough, no_link]),
    meck:expect(op_worker_storage, get, fun(_StorageId) -> ?STORAGE_DETAILS end).


%% @private
unmock_all(_) ->
    meck:unload().

%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions used in API tests.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_utils).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").

-export([load_module_from_test_distributed_dir/2]).

-export([ensure_defined/2]).
-export([maybe_substitute_bad_id/2]).
-export([get_storage_id_by_name/2]).


%%%===================================================================
%%% API
%%%===================================================================


%%% TODO VFS-6385 Reorganize and fix includes and loading modules from other dirs in tests
-spec load_module_from_test_distributed_dir(proplists:proplist(), module()) ->
    ok.
load_module_from_test_distributed_dir(Config, ModuleName) ->
    DataDir = ?config(data_dir, Config),
    ProjectRoot = filename:join(lists:takewhile(fun(Token) ->
        Token /= "test_distributed"
    end, filename:split(DataDir))),
    TestsRootDir = filename:join([ProjectRoot, "test_distributed"]),

    code:add_pathz(TestsRootDir),

    CompileOpts = [
        verbose, report_errors, report_warnings,
        {i, TestsRootDir},
        {i, filename:join([TestsRootDir, "..", "include"])},
        {i, filename:join([TestsRootDir, "..", "_build", "default", "lib"])}
    ],
    case compile:file(filename:join(TestsRootDir, ModuleName), CompileOpts) of
        {ok, ModuleName} ->
            code:purge(ModuleName),
            code:load_file(ModuleName),
            ok;
        _ ->
            ct:fail("Couldn't load module: ~p", [ModuleName])
    end.


-spec ensure_defined
    (undefined, DefaultValue) -> DefaultValue when DefaultValue :: term();
    (Value, DefaultValue :: term()) -> Value when Value :: term().
ensure_defined(undefined, DefaultValue) -> DefaultValue;
ensure_defined(Value, _DefaultValue) -> Value.


maybe_substitute_bad_id(ValidId, undefined) ->
    {ValidId, undefined};
maybe_substitute_bad_id(ValidId, Data) ->
    case maps:take(bad_id, Data) of
        {BadId, LeftoverData} -> {BadId, LeftoverData};
        error -> {ValidId, Data}
    end.


get_storage_id_by_name(Config, StorageName) ->
    StorageIds = op_worker_test_rpc:get_storage_ids(Config),
    Storages = [op_worker_test_rpc:describe_storage(Config, X) || X <- StorageIds],

    [StorageId | _] = [maps:get(<<"id">>, X) || X <- Storages, (maps:get(<<"name">>, X) == StorageName)],
    StorageId.


%%%===================================================================
%%% Internal functions
%%%===================================================================

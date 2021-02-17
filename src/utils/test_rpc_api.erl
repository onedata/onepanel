%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Module exposing onepanel functions that are used in tests.
%%% @end
%%%-------------------------------------------------------------------
-module(test_rpc_api).
-author("Piotr Duleba").

%% API
-export([
    get_op_worker_nodes/0,
    is_emergency_passphrase_set/0,
    verify_emergency_passphrase/1
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_op_worker_nodes() -> [node()].
get_op_worker_nodes() ->
    service_op_worker:get_nodes().


-spec is_emergency_passphrase_set() -> boolean().
is_emergency_passphrase_set()->
    emergency_passphrase:is_set().


-spec verify_emergency_passphrase(binary()) -> boolean().
verify_emergency_passphrase(Passphrase)->
    emergency_passphrase:verify(Passphrase).
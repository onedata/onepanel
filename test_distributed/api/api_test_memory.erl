%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions used for environment manipulation in API tests.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_memory).
-author("Bartosz Walkowicz").


-export([init/0, set/3, get/2, get/3]).

-opaque env_ref() :: integer().

-export_type([env_ref/0]).


%%%===================================================================
%%% API
%%%===================================================================


-spec init() -> env_ref().
init() ->
    erlang:unique_integer([positive]).


-spec set(env_ref(), Key :: term(), Value :: term()) -> ok.
set(EnvRef, Key, Value) ->
    node_cache:put({EnvRef, Key}, Value).


-spec get(env_ref(), Key :: term()) -> Value :: term() | no_return().
get(EnvRef, Key) ->
    node_cache:get({EnvRef, Key}).


-spec get(env_ref(), Key :: term(), Default :: term()) ->
    Value :: term() | no_return().
get(EnvRef, Key, Default) ->
    node_cache:get({EnvRef, Key}, Default).

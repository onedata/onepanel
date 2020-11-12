%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides callback functions for results validation in API tests.
%%% @end
%%%-------------------------------------------------------------------
-module(api_test_validate).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").


%% API
-export([
    http_200_ok/1,
    http_201_created/3,
    http_204_no_content/0
]).


%%%===================================================================
%%% API functions
%%%===================================================================


http_200_ok(VerifyBodyFun) ->
    fun(_, {ok, RespCode, _, Body}) ->
        ?assertEqual(?HTTP_200_OK, RespCode),
        VerifyBodyFun(Body)
    end.


http_201_created(Path, BodyKey, VerifyFun) ->
    fun(_, {ok, RespCode, Headers, Body}) ->
        HeadersItemId = api_test_utils:match_location_header(Headers, ?PATH_PREFIX ++ Path),
        BodyItemId = maps:get(BodyKey, Body),
        ?assertEqual(HeadersItemId, BodyItemId),
        ?assertEqual(?HTTP_201_CREATED, RespCode),
        VerifyFun(HeadersItemId)
    end.


http_204_no_content() ->
    fun(_, {ok, RespCode, _, Body}) ->
        ?assertEqual(?HTTP_204_NO_CONTENT, RespCode),
        ?assertEqual(#{}, Body)
    end.

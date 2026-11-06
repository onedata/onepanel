%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for debug.
%%% @end
%%%--------------------------------------------------------------------
-module(debug_rest_routes).
-author("Wojciech Geisler").

-include("http/rest.hrl").

%% API
-export([routes/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of tuples consisting of a path, a handler module and
%% an initial request state.
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), #rest_req{}}].
routes() ->
    [
        %% Get transfers mock status
        {<<"/provider/debug/transfers_mock">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = transfers_mock,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify transfers mock
        {<<"/provider/debug/transfers_mock">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = transfers_mock,
                scope = private
            },
            %% New value for the mock setting.
            data_spec = (rest_model:transfers_mock_model())
        }}

    ].

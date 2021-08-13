%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for internal.
%%% @end
%%%--------------------------------------------------------------------
-module(internal_rest_routes).
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
        %% Get details of a remote Oneprovider
        {<<"/providers/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = ?BINDING(id),
                aspect = remote_instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Perform health check
        {<<"/health">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = health,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get test image
        {<<"/test_image">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = test_image,
                scope = private
            },
            produces = [<<"image/png">>]
        }}

    ].

%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for file_popularity.
%%% @end
%%%--------------------------------------------------------------------
-module(file_popularity_rest_routes).
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
        %% Configure file popularity mechanism in the space
        {<<"/provider/spaces/:id/file-popularity/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = file_popularity_configuration,
                scope = private
            },
            %% Configuration of the file-popularity mechanism in the space.
            data_spec = (rest_model:space_file_popularity_configuration_model())
        }},

        %% Get file popularity configuration
        {<<"/provider/spaces/:id/file-popularity/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = file_popularity_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }}

    ].

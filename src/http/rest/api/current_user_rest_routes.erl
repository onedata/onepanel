%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for current_user.
%%% @end
%%%--------------------------------------------------------------------
-module('current_user_rest_routes').
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
        %% Get details of a user's cluster
        {<<"/user/clusters/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_cluster,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List user's clusters
        {<<"/user/clusters">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = current_user_clusters,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Onepanel user details of currently logged in user
        {<<"/user">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = current_user,
                scope = private
            },
            produces = [<<"application/json">>]
        }}

    ].

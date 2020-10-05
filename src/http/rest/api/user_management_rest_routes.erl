%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for user_management.
%%% @end
%%%--------------------------------------------------------------------
-module('user_management_rest_routes').
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
        %% Create Onezone user
        {<<"/zone/users">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% The user configuration details.
            data_spec = (rest_model:onezone_user_create_request_model())
        }},

        %% Set password for Onezone user
        {<<"/zone/users/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_user,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            data_spec = (rest_model:password_change_request_model())
        }},

        %% Get Onezone user details
        {<<"/zone/users/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% List Onezone users
        {<<"/zone/users">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }}

    ].

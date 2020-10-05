%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for service_configuration.
%%% @end
%%%--------------------------------------------------------------------
-module('service_configuration_rest_routes').
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
        %% Get settings of a Onezone GUI message
        {<<"/zone/gui_messages/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = {gui_message, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get Onezone policies
        {<<"/zone/policies">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = policies,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify settings of a Onezone GUI message
        {<<"/zone/gui_messages/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = {gui_message, ?BINDING(id)},
                scope = private
            },
            data_spec = (rest_model:gui_message_model())
        }},

        %% Modify current Onezone policies
        {<<"/zone/policies">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = policies,
                scope = private
            },
            %% New values for Onezone policies.
            data_spec = (rest_model:zone_policies_model())
        }}

    ].

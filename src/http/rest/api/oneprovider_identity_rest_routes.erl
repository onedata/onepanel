%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for oneprovider_identity.
%%% @end
%%%--------------------------------------------------------------------
-module(oneprovider_identity_rest_routes).
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
        %% Register provider
        {<<"/provider">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% The new provider details.
            data_spec = (rest_model:provider_register_request_model())
        }},

        %% Get Onezone information
        {<<"/provider/onezone_info">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_zone,
                id = undefined,
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Oneprovider registration token obtained from Onezone.
                %% Required if the Oneprovider is not registered.
                token => {string, optional}
            }
        }},

        %% Get provider details
        {<<"/provider">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify provider details
        {<<"/provider">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            },
            %% New values for provider configuration parameters which should be
            %% changed.
            data_spec = (rest_model:provider_modify_request_model())
        }},

        %% Unregister provider
        {<<"/provider">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_provider,
                id = undefined,
                aspect = instance,
                scope = private
            }
        }}

    ].

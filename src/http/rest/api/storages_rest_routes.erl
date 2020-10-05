%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for storages.
%%% @end
%%%--------------------------------------------------------------------
-module('storages_rest_routes').
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
        %% Add storage
        {<<"/provider/storages">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_storage,
                id = undefined,
                aspect = instances,
                scope = private
            },
            %% The configuration details of storage resources to be added to the
            %% provider deployment.
            data_spec = (rest_model:storage_create_request_model())
        }},

        %% Get storage details
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get storages
        {<<"/provider/storages">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_storage,
                id = undefined,
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify storage config
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            %% New values for storage configuration parameters which should be
            %% changed. Must contain the current type of the storage.
            data_spec = (rest_model:storage_modify_request_model())
        }},

        %% Remove storage
        {<<"/provider/storages/:id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_storage,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            }
        }}

    ].

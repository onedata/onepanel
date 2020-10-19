%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for space_support.
%%% @end
%%%--------------------------------------------------------------------
-module('space_support_rest_routes').
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
        %% Force start auto storage import scan
        {<<"/provider/spaces/:id/storage-import/auto/force-start">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = force_start_auto_storage_import_scan,
                scope = private
            }
        }},

        %% Force stop auto storage import scan
        {<<"/provider/spaces/:id/storage-import/auto/force-stop">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = force_stop_auto_storage_import_scan,
                scope = private
            }
        }},

        %% Get information about auto storage import scan
        {<<"/provider/spaces/:id/storage-import/auto/info">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_storage_import_info,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get statistics of auto storage import mechanism
        {<<"/provider/spaces/:id/storage-import/auto/stats">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = auto_storage_import_stats,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% Predefined time period for which the statistics should be
                %% fetched.
                period => string,
                %% Specify which statistic metrics should be returned - strings
                %% delimited with comma. Accepted values are:
                %% `queueLength`, `createdFiles`,
                %% `modifiedFiles`, `deletedFiles`
                metrics => string
            }
        }},

        %% Get manual storage import example
        {<<"/provider/spaces/:id/storage-import/manual/example">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = manual_storage_import_example,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get provider spaces
        {<<"/provider/spaces">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get space details
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Modify space details
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = support,
                scope = private
            },
            data_spec = (rest_model:space_modify_request_model())
        }},

        %% Revoke space support for a space
        {<<"/provider/spaces/:id">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_space,
                id = ?BINDING(id),
                aspect = support,
                scope = private
            }
        }},

        %% Support space
        {<<"/provider/spaces">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_space,
                id = undefined,
                aspect = support,
                scope = private
            },
            %% Specification of the space support request including support size
            %% and token.
            data_spec = (rest_model:space_support_request_model())
        }}

    ].

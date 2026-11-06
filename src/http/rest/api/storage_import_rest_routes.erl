%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for storage_import.
%%% @end
%%%--------------------------------------------------------------------
-module(storage_import_rest_routes).
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
        }}

    ].

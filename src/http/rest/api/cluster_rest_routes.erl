%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for cluster.
%%% @end
%%%--------------------------------------------------------------------
-module('cluster_rest_routes').
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
        %% Add given host to the cluster
        {<<"/hosts">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = instance,
                scope = private
            },
            data_spec = (rest_model:host_add_request_model())
        }},

        %% Create node invite token
        {<<"/invite_tokens">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = invite_token,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Generate cluster invitation token for a user
        {<<"/cluster/invite_user_token">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_cluster,
                id = undefined,
                aspect = invite_user_token,
                scope = private
            }
        }},

        %% Get cluster cookie
        {<<"/cookie">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = cookie,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get cluster hosts
        {<<"/hosts">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = list,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get summary of members in this cluster
        {<<"/cluster/members_summary">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_cluster,
                id = undefined,
                aspect = current_cluster_members_summary,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get public configuration
        {<<"/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get details of this cluster
        {<<"/cluster">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_cluster,
                id = undefined,
                aspect = current_cluster,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get information about current onepanel node
        {<<"/node">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = instance,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get deployment progress
        {<<"/progress">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = progress,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get background task result
        {<<"/tasks/:id">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = {task, ?BINDING(id)},
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Join existing cluster
        {<<"/join_cluster">>, #rest_req{
            method = 'POST',
            b_gri = #b_gri{
                type = onp_host,
                id = undefined,
                aspect = join_cluster,
                scope = private
            },
            data_spec = (rest_model:invite_token_model())
        }},

        %% Modify progress markers
        {<<"/progress">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = progress,
                scope = private
            },
            data_spec = (rest_model:progress_modify_model())
        }},

        %% Remove host from cluster
        {<<"/hosts/:host">>, #rest_req{
            method = 'DELETE',
            b_gri = #b_gri{
                type = onp_host,
                id = ?BINDING(host),
                aspect = instance,
                scope = private
            }
        }}

    ].

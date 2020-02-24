%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for onepanel.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_rest_routes).
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
        %% Adds given host to the cluster
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

        %% Check correctness of DNS entries for the cluster's domain.
        {<<"/dns_check">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check,
                scope = private
            },
            produces = [<<"application/json">>],
            data_spec = #{
                %% If true the DNS check cache is overridden and check is
                %% performed during handling of the request.
                forceCheck => {boolean, {optional, false}}
            }
        }},

        %% Create invite token
        {<<"/invite_token">>, #rest_req{
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

        %% Get Onepanel user details of currently logged in user.
        {<<"/user">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_user,
                id = undefined,
                aspect = current_user,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Return settings used when performing the DNS check.
        {<<"/dns_check/configuration">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check_configuration,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get emergency passphrase status
        {<<"/emergency_passphrase">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = emergency_passphrase,
                scope = private
            },
            produces = [<<"application/json">>]
        }},

        %% Get information about current onepanel node.
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

        %% Get details of a remote Oneprovider.
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

        %% Get information about SSL certificates configuration and status.
        {<<"/web_cert">>, #rest_req{
            method = 'GET',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = web_cert,
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

        %% Configure dns check
        {<<"/dns_check/configuration">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = dns_check_configuration,
                scope = private
            },
            %% The configuration changes.
            data_spec = (rest_model:dns_check_configuration_model())
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

        %% Modify SSL certificate configuration
        {<<"/web_cert">>, #rest_req{
            method = 'PATCH',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = web_cert,
                scope = private
            },
            %% New values for certificate management configuration.
            data_spec = (rest_model:web_cert_modify_request_model())
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
        }},

        %% Set emergency passphrase
        {<<"/emergency_passphrase">>, #rest_req{
            method = 'PUT',
            b_gri = #b_gri{
                type = onp_panel,
                id = undefined,
                aspect = emergency_passphrase,
                scope = private
            },
            data_spec = (rest_model:emergency_passphrase_change_request_model())
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

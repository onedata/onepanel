%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc REST API definitions for onepanel.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_api).
-author("Krzysztof Trzepla").

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
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() ->
    [
        %% Adds given host to the cluster
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:host_add_request_model()
            }]
        }},

        %% Check correctness of DNS entries for the cluster's domain.
        {<<"/api/v3/onepanel/dns_check">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = dns_check,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET',
                params_spec = #{
                    %% If true the DNS check cache is overridden and check
                    %% is performed during handling of the request.
                    forceCheck => {boolean, {optional, false}}
                }
            }]
        }},

        %% Generate cluster invitation token for a user
        {<<"/api/v3/onepanel/cluster/invite_user_token">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = invite_user_token,
            methods = [#rmethod{
                type = 'POST'
            }]
        }},

        %% Get details of a user's cluster
        {<<"/api/v3/onepanel/user/clusters/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = cluster,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get cluster cookie
        {<<"/api/v3/onepanel/cookie">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = cookie,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get cluster hosts
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get summary of members in this cluster
        {<<"/api/v3/onepanel/cluster/members_summary">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = current_cluster_members_summary,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% List user's clusters
        {<<"/api/v3/onepanel/user/clusters">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = clusters,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get public configuration
        {<<"/api/v3/onepanel/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = configuration,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get details of this cluster
        {<<"/api/v3/onepanel/cluster">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = current_cluster,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get Onepanel user details of currently logged in user.
        {<<"/api/v3/onepanel/user">>, rest_handler, #rstate{
            version = 3,
            module = rest_users,
            resource = current_user,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Return settings used when performing the DNS check.
        {<<"/api/v3/onepanel/dns_check/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = dns_check_configuration,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get emergency passphrase status
        {<<"/api/v3/onepanel/emergency_passphrase">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = emergency_passphrase,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get information about current onepanel node.
        {<<"/api/v3/onepanel/node">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = node,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get deployment progress
        {<<"/api/v3/onepanel/progress">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = progress,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get details of a remote Oneprovider.
        {<<"/api/v3/onepanel/providers/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_clusters,
            resource = remote_provider,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get background task result
        {<<"/api/v3/onepanel/tasks/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = task,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get information about SSL certificates configuration and status.
        {<<"/api/v3/onepanel/web_cert">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = web_cert,
            produces = [<<"application/json">>],
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Join existing cluster
        {<<"/api/v3/onepanel/join_cluster">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = cluster,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:join_cluster_request_model(),
                noauth = true
            }]
        }},

        %% Configure dns check
        {<<"/api/v3/onepanel/dns_check/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = dns_check_configuration,
            methods = [#rmethod{
                type = 'PATCH',
                %% The configuration changes.
                args_spec = rest_model:dns_check_configuration_model()
            }]
        }},

        %% Modify progress markers
        {<<"/api/v3/onepanel/progress">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = progress,
            methods = [#rmethod{
                type = 'PATCH',
                args_spec = rest_model:progress_modify_model()
            }]
        }},

        %% Modify SSL certificate configuration
        {<<"/api/v3/onepanel/web_cert">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = web_cert,
            methods = [#rmethod{
                type = 'PATCH',
                %% New values for certificate management configuration.
                args_spec = rest_model:web_cert_modify_request_model()
            }]
        }},

        %% Remove host from cluster
        {<<"/api/v3/onepanel/hosts/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = host,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }},

        %% Set emergency passphrase
        {<<"/api/v3/onepanel/emergency_passphrase">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = emergency_passphrase,
            methods = [#rmethod{
                type = 'PUT',
                args_spec = rest_model:emergency_passphrase_change_request_model(),
                noauth = true
            }]
        }},

        %% Get test image
        {<<"/api/v3/onepanel/test_image">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = test_image,
            produces = [<<"image/png">>],
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }}

    ].
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
        %% Create Onepanel user
        {<<"/api/v3/onepanel/users">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = users,
            methods = [#rmethod{
                type = 'POST',
                %% The user configuration details.
                args_spec = rest_model:user_create_request_model(),
                noauth = true
            }]
        }},

        %% Check correctness of DNS entries for the cluster's domain.
        {<<"/api/v3/onepanel/dns_check">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = dns_check,
            methods = [#rmethod{
                type = 'GET',
                params_spec = #{
                    %% If true the DNS check cache is overriden and check is
                    %% performed during handling of the request.
                    forceCheck => {boolean, {optional, false}}
                }
            }]
        }},

        %% Create or join cluster
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'POST',
                %% The cookie used for cluster authentication.
                args_spec = rest_model:cookie_model(),
                params_spec = #{
                    %% Hostname of an existing cluster node.
                    clusterHost => {string, optional}
                },
                noauth = true
            }]
        }},

        %% Create Onepanel user session
        {<<"/api/v3/onepanel/session">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_session,
            resource = session,
            methods = [#rmethod{
                type = 'POST'
            }]
        }},

        %% Get cluster cookie
        {<<"/api/v3/onepanel/cookie">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = cookie,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get cluster or discovered hosts
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'GET',
                params_spec = #{
                    %% Defines whether to return cluster or discovered
                    %% hosts.
                    discovered => {boolean, {optional, false}}
                }
            }]
        }},

        %% Return settings used when performing the DNS check.
        {<<"/api/v3/onepanel/dns_check/configuration">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = dns_check_configuration,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get Onepanel user session
        {<<"/api/v3/onepanel/session">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_session,
            resource = session,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get background task result
        {<<"/api/v3/onepanel/tasks/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = task,
            methods = [#rmethod{
                type = 'GET',
                noauth = true
            }]
        }},

        %% Get Onepanel user details
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get information about SSL certificates configuration and status.
        {<<"/api/v3/onepanel/web_cert">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = web_cert,
            methods = [#rmethod{
                type = 'GET'
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

        %% Modify Onepanel user details
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'PATCH',
                args_spec = rest_model:user_modify_request_model()
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

        %% Remove Onepanel user session
        {<<"/api/v3/onepanel/session">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_session,
            resource = session,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }},

        %% Remove Onepanel user
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }}

    ].
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
        %% Adds given host to the cluster.
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:host_add_request_model()
            }]
        }},

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

        %% Get cluster hosts
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get information about current onepanel node.
        {<<"/api/v3/onepanel/node">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = node,
            methods = [#rmethod{
                type = 'GET',
                noauth = true
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

        %% List onepanel users
        {<<"/api/v3/onepanel/users">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = users,
            methods = [#rmethod{
                type = 'GET',
                params_spec = #{
                    %% If present, query returns only users with specified
                    %% role.
                    role => {string, optional}
                },
                noauth = true
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
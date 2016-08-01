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
        %% Remove cluster node
        {<<"/api/v3/onepanel/hosts/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = host,
            methods = [#rmethod{
                type = 'DELETE'
            }]
        }},

        %% Remove user
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'DELETE'
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

        %% Get cluster/discovered hosts
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'GET',
                params_spec = #{
                    %% Defines whether return cluster or discovered hosts.
                    discovered => {boolean, {optional, false}}
                },
                noauth = true
            }]
        }},

        %% Get task result
        {<<"/api/v3/onepanel/tasks/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = task,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Get user details
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'GET'
            }]
        }},

        %% Modify user details
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{
                type = 'PATCH',
                args_spec = rest_model:user_modify_request_model()
            }]
        }},

        %% Create user
        {<<"/api/v3/onepanel/users">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = users,
            methods = [#rmethod{
                type = 'POST',
                args_spec = rest_model:user_create_request_model(),
                noauth = true
            }]
        }},

        %% Create or join cluster
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{
                type = 'PUT',
                args_spec = rest_model:cookie_model(),
                params_spec = #{
                    %% Hostname of an existing cluster node.
                    clusterHost => {string, optional}
                },
                noauth = true
            }]
        }}

    ].
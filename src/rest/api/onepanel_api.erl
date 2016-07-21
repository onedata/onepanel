%%%--------------------------------------------------------------------
%%% This file has been automatically generated from Swagger
%%% specification - DO NOT EDIT!
%%%
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license cited i
%%% in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc 
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
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), Module :: module(), State :: rest_handler:state()}].
routes() ->
    [
        %%
        %% Add cluster host
        %%
        {<<"/api/v3/onepanel/hosts/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = host,
            methods = [#rmethod{type = 'PUT'}]
        }},

        %%
        %% Add user
        %%
        {<<"/api/v3/onepanel/users">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = users,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %% New user account details.
                args_spec = rest_model:user_details_model(),
                noauth = true}]
        }},

        %%
        %% Get cluster hosts
        %%
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{type = 'GET',
                %% [Query parameters]
                params_spec = #{
                    %%
                    discovered => {boolean, {optional, false}}},
                noauth = true}]
        }},

        %%
        %% Get cluster hosts
        %%
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = hosts,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %%
                args_spec = rest_model:hosts_model(),
                %% [Query parameters]
                params_spec = #{
                    %%
                    discovered => {boolean, {optional, false}}},
                noauth = true}]
        }},

        %%
        %% Get task status
        %%
        {<<"/api/v3/onepanel/tasks/:id">>, rest_handler, #rstate{
            version = 3,
            module = rest_service,
            resource = task,
            methods = [#rmethod{type = 'GET'}]
        }},

        %%
        %% Get user details
        %%
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{type = 'GET'}]
        }},


        %%
        %% Change user password
        %%
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{type = 'PUT',
                %% [Body parameter]
                %% New password.
                args_spec = rest_model:user_details_update_model()}]
        }},

        %%
        %% Remove cluster host
        %%
        {<<"/api/v3/onepanel/hosts/:host">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel,
            resource = host,
            methods = [#rmethod{type = 'DELETE'}]
        }},

        %%
        %% Remove user
        %%
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3,
            module = rest_onepanel_user,
            resource = user,
            methods = [#rmethod{type = 'DELETE'}]
        }}

    ].



















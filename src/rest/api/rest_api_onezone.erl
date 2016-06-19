%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(rest_api_onezone).
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
        {<<"/api/v3/onepanel/user">>, rest_handler, #rstate{
            version = 3, module = rest_onedata_user, resource = user,
            methods = [
                {'POST', #{
                    username => {string, required},
                    password => {string, required},
                    userRole => {atom, required}
                }},
                {'PUT', #{
                    password => {string, required}
                }},
                'GET'
            ],
            noauth = ['POST']
        }}
    ].

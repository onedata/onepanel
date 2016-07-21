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
        {<<"/api/v3/onepanel/hosts">>, rest_handler, #rstate{
            version = 3, module = rest_onepanel, resource = hosts,
            methods = [
                #rmethod{type = 'PUT', noauth = true,
                    args_spec = #{hosts => {[string], {optional, []}}},
                    params_spec = #{discovered => {boolean, {optional, false}}}
                },
                #rmethod{type = 'GET', noauth = true,
                    params_spec = #{discovered => {boolean, {optional, false}}}
                }
            ]
        }},
        {<<"/api/v3/onepanel/hosts/:host">>, rest_handler, #rstate{
            version = 3, module = rest_onepanel, resource = host,
            methods = [
                #rmethod{type = 'PUT'},
                #rmethod{type = 'DELETE'}
            ]
        }},
        {<<"/api/v3/onepanel/users">>, rest_handler, #rstate{
            version = 3, module = rest_onepanel_user, resource = users,
            methods = [
                #rmethod{type = 'PUT', noauth = true, args_spec = #{
                    username => string,
                    password => string,
                    userRole => atom
                }}
            ]
        }},
        {<<"/api/v3/onepanel/users/:username">>, rest_handler, #rstate{
            version = 3, module = rest_onepanel_user, resource = user,
            methods = [
                #rmethod{type = 'PUT', args_spec = #{
                    password => string
                }},
                #rmethod{type = 'GET'},
                #rmethod{type = 'DELETE'}
            ]
        }},
        {<<"/api/v3/onepanel/service/tasks/:id">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = task,
            methods = [#rmethod{type = 'GET'}]
        }},
        {<<"/api/v3/onepanel/cluster/databases">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_couchbase,
            methods = [
                #rmethod{type = 'PUT', args_spec = #{hosts => [string]}},
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/databases/:host">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_couchbase,
            methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/managers">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_cluster_manager,
            methods = [
                #rmethod{type = 'PUT', args_spec = #{
                    mainHost => string,
                    hosts => [string]
                }},
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/managers/:host">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_cluster_manager,
            methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/workers">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_oz_worker,
            methods = [
                #rmethod{type = 'PUT', args_spec = #{hosts => [string]}},
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/workers/:host">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_oz_worker,
            methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/configuration">>, rest_handler, #rstate{
            version = 3, module = rest_service, resource = service_onezone,
            methods = [
                #rmethod{type = 'PUT', args_spec = #{
                    cluster => #{
                        domainName => string,
                        nodes => #{
                            '_' => #{
                                hostname => string
                            }
                        },
                        managers => #{
                            mainNode => string,
                            nodes => [string]
                        },
                        workers => #{
                            nodes => [string]
                        },
                        databases => #{
                            nodes => [string]
                        }
                    },
                    onezone => {#{
                        name => {string, optional},
                        domainName => {string, optional}
                    }, optional}
                }}
            ]
        }}
    ].


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
-module(rest_api_oneprovider).
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
                #rmethod{type = 'POST', noauth = true, args_spec = #{
                    username => {string, required},
                    password => {string, required},
                    userRole => {atom, required}
                }},
                #rmethod{type = 'PUT', args_spec = #{
                    password => {string, required}
                }},
                #rmethod{type = 'GET'}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/databases">>, rest_handler, #rstate{
            version = 3, module = rest_couchbase,
            resource = couchbase, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/databases/:host">>, rest_handler, #rstate{
            version = 3, module = rest_couchbase,
            resource = couchbase, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/managers">>, rest_handler, #rstate{
            version = 3, module = rest_cluster_manager,
            resource = cluster_manager, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/managers/:host">>, rest_handler, #rstate{
            version = 3, module = rest_cluster_manager,
            resource = cluster_manager, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/workers">>, rest_handler, #rstate{
            version = 3, module = rest_cluster_worker,
            resource = op_worker, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/cluster/workers/:host">>, rest_handler, #rstate{
            version = 3, module = rest_cluster_worker,
            resource = op_worker, methods = [
                #rmethod{type = 'GET'},
                #rmethod{type = 'PATCH', params_spec = #{
                    started => {boolean, {optional, true}}
                }}
            ]
        }},
        {<<"/api/v3/onepanel/configuration">>, rest_handler, #rstate{
            version = 3, module = rest_cluster_worker,
            resource = oneprovider, methods = [
                #rmethod{type = 'PUT', args_spec = #{
                    cluster => {#{
                        domain_name => string,
                        nodes => #{
                            '_' => #{
                                hostname => string
                            }
                        },
                        managers => #{
                            main_node => string,
                            nodes => [string]
                        },
                        workers => #{
                            nodes => [string]
                        },
                        databases => #{
                            nodes => [string]
                        },
                        storages => {#{
                            '_' => {oneof, [
                                #{
                                    type => {equal, {string, <<"POSIX">>}},
                                    mount_point => string
                                },
                                #{
                                    type => {equal, {string, <<"S3">>}},
                                    access_key => string,
                                    secret_key => string,
                                    s3_hostname => string,
                                    iam_hostname => string,
                                    bucket_name => string
                                },
                                #{
                                    type => {equal, {string, <<"CEPH">>}},
                                    username => string,
                                    key => string,
                                    monitor_hostname => string,
                                    cluster_name => string,
                                    pool_name => string
                                }
                            ]}
                        }, optional}
                    }, optional},
                    oneprovider => {#{
                        register => {boolean, optional},
                        name => {string, optional},
                        redirection_point => {string, optional},
                        geo_latitude => {float, optional},
                        geo_longitude => {float, optional}
                    }, optional},
                    onezone => {#{
                        domain_name => {string, optional}
                    }, optional}
                }},
                #rmethod{type = 'GET'}
            ]
        }}
    ].


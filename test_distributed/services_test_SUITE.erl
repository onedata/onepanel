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
-module(services_test_SUITE).
-author("Krzysztof Trzepla").

-include("modules/models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    service_onezone_deploy_test/1,
    service_oz_worker_stop_single_node_test/1,
    service_oz_cluster_manager_stop_single_node_test/1,
    service_oz_couchbase_stop_single_node_test/1,
    service_oz_couchbase_start_single_node_test/1,
    service_oz_cluster_manager_start_single_node_test/1,
    service_oz_worker_start_single_node_test/1,
    service_oz_worker_stop_test/1,
    service_oz_cluster_manager_stop_test/1,
    service_oz_couchbase_stop_test/1,
    service_oz_couchbase_start_test/1,
    service_oz_cluster_manager_start_test/1,
    service_oz_worker_start_test/1,
    service_onezone_stop_test/1,
    service_onezone_start_test/1,
    service_oz_couchbase_status_single_node_test/1,
    service_oz_cluster_manager_status_single_node_test/1,
    service_oz_worker_status_single_node_test/1,
    service_oz_couchbase_status_test/1,
    service_oz_cluster_manager_status_test/1,
    service_oz_worker_status_test/1,
    service_onezone_status_test/1,
    service_oneprovider_deploy_test/1,
    service_op_worker_stop_single_node_test/1,
    service_op_cluster_manager_stop_single_node_test/1,
    service_op_couchbase_stop_single_node_test/1,
    service_op_couchbase_start_single_node_test/1,
    service_op_cluster_manager_start_single_node_test/1,
    service_op_worker_start_single_node_test/1,
    service_op_worker_stop_test/1,
    service_op_cluster_manager_stop_test/1,
    service_op_couchbase_stop_test/1,
    service_op_couchbase_start_test/1,
    service_op_cluster_manager_start_test/1,
    service_op_worker_start_test/1,
    service_oneprovider_stop_test/1,
    service_oneprovider_start_test/1,
    service_op_couchbase_status_single_node_test/1,
    service_op_cluster_manager_status_single_node_test/1,
    service_op_worker_status_single_node_test/1,
    service_op_couchbase_status_test/1,
    service_op_cluster_manager_status_test/1,
    service_op_worker_status_test/1,
    service_oneprovider_status_test/1
]).

all() ->
    ?ALL([
        service_onezone_deploy_test,
        service_oneprovider_deploy_test,
        service_oz_worker_stop_single_node_test,
        service_oz_cluster_manager_stop_single_node_test,
        service_oz_couchbase_stop_single_node_test,
        service_oz_couchbase_start_single_node_test,
        service_oz_cluster_manager_start_single_node_test,
        service_oz_worker_start_single_node_test,
        service_oz_worker_stop_test,
        service_oz_cluster_manager_stop_test,
        service_oz_couchbase_stop_test,
        service_oz_couchbase_start_test,
        service_oz_cluster_manager_start_test,
        service_oz_worker_start_test,
        service_onezone_stop_test,
        service_onezone_start_test,
        service_oz_couchbase_status_single_node_test,
        service_oz_cluster_manager_status_single_node_test,
        service_oz_worker_status_single_node_test,
        service_oz_couchbase_status_test,
        service_oz_cluster_manager_status_test,
        service_oz_worker_status_test,
        service_onezone_status_test,
        service_op_worker_stop_single_node_test,
        service_op_cluster_manager_stop_single_node_test,
        service_op_couchbase_stop_single_node_test,
        service_op_couchbase_start_single_node_test,
        service_op_cluster_manager_start_single_node_test,
        service_op_worker_start_single_node_test,
        service_op_worker_stop_test,
        service_op_cluster_manager_stop_test,
        service_op_couchbase_stop_test,
        service_op_couchbase_start_test,
        service_op_cluster_manager_start_test,
        service_op_worker_start_test,
        service_oneprovider_stop_test,
        service_oneprovider_start_test,
        service_op_couchbase_status_single_node_test,
        service_op_cluster_manager_status_single_node_test,
        service_op_worker_status_single_node_test,
        service_op_couchbase_status_test,
        service_op_cluster_manager_status_test,
        service_op_worker_status_test,
        service_oneprovider_status_test
    ]).

-define(SERVICE_OP, service_oneprovider:name()).
-define(SERVICE_OZ, service_onezone:name()).
-define(SERVICE_CB, service_couchbase:name()).
-define(SERVICE_CM, service_cluster_manager:name()).
-define(SERVICE_OPW, service_op_worker:name()).
-define(SERVICE_OZW, service_oz_worker:name()).
-define(ATTEMPTS, 30).
-define(TIMEOUT, timer:seconds(10)).

%%%===================================================================
%%% Test functions
%%%===================================================================

service_onezone_deploy_test(Config) ->
    [Node | _] = Nodes = ?config(oz_nodes, Config),
    Hosts = lists:usort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE_OZ, deploy, #{
            cluster => #{
                ?SERVICE_CB => #{
                    hosts => Hosts
                },
                ?SERVICE_CM => #{
                    hosts => Hosts, main_cm_host => hd(Hosts), worker_num => 2
                },
                ?SERVICE_OZW => #{
                    hosts => Hosts, main_cm_host => hd(Hosts),
                    cm_hosts => Hosts, db_hosts => Hosts
                }
            }
        }]
    )).


service_oz_worker_stop_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OZW, stop).


service_oz_cluster_manager_stop_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, stop).


service_oz_couchbase_stop_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, stop).


service_oz_couchbase_start_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, start).


service_oz_cluster_manager_start_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, start).


service_oz_worker_start_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OZW, start).


service_oz_worker_stop_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZW, stop).


service_oz_cluster_manager_stop_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, stop).


service_oz_couchbase_stop_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, stop).


service_oz_couchbase_start_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, start).


service_oz_cluster_manager_start_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, start).


service_oz_worker_start_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZW, start).


service_onezone_stop_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZ, stop).


service_onezone_start_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZ, start).


service_oz_couchbase_status_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, status).


service_oz_cluster_manager_status_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, status).


service_oz_worker_status_single_node_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OZW, status).


service_oz_couchbase_status_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, status).


service_oz_cluster_manager_status_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, status).


service_oz_worker_status_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZW, status).


service_onezone_status_test(Config) ->
    Nodes = ?config(oz_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OZ, status).


service_oneprovider_deploy_test(Config) ->
    [Node | _] = Nodes = ?config(op_nodes, Config),
    [OzNode | _] = ?config(oz_nodes, Config),
    OzDomain = onepanel_cluster:node_to_host(OzNode),
    Hosts = lists:usort(onepanel_cluster:nodes_to_hosts(Nodes)),

    ?assertEqual(ok, rpc:call(Node, service, apply,
        [?SERVICE_OP, deploy, #{
            cluster => #{
                ?SERVICE_CB => #{
                    hosts => Hosts
                },
                ?SERVICE_CM => #{
                    hosts => Hosts, main_cm_host => hd(Hosts), worker_num => 2
                },
                ?SERVICE_OPW => #{
                    hosts => Hosts, main_cm_host => hd(Hosts),
                    cm_hosts => Hosts, db_hosts => Hosts
                }
            },
            ?SERVICE_OP => #{
                hosts => Hosts,
                oneprovider_register => true,
                onezone_domain => OzDomain
            }
        }]
    )).

service_op_worker_stop_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OPW, stop).


service_op_cluster_manager_stop_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, stop).


service_op_couchbase_stop_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, stop).


service_op_couchbase_start_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, start).


service_op_cluster_manager_start_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, start).


service_op_worker_start_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OPW, start).


service_op_worker_stop_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OPW, stop).


service_op_cluster_manager_stop_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, stop).


service_op_couchbase_stop_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, stop).


service_op_couchbase_start_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, start).


service_op_cluster_manager_start_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, start).


service_op_worker_start_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OPW, start).


service_oneprovider_stop_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OP, stop).


service_oneprovider_start_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OP, start).


service_op_couchbase_status_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CB, status).


service_op_cluster_manager_status_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_CM, status).


service_op_worker_status_single_node_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action_on_single_node(Nodes, ?SERVICE_OPW, status).


service_op_couchbase_status_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CB, status).


service_op_cluster_manager_status_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_CM, status).


service_op_worker_status_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OPW, status).


service_oneprovider_status_test(Config) ->
    Nodes = ?config(op_nodes, Config),
    execute_service_action(Nodes, ?SERVICE_OP, status).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:init(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Executes service action on a chosen node.
%%--------------------------------------------------------------------
-spec execute_service_action_on_single_node(Nodes :: [node()],
    Service :: service:name(), Action :: service:action()) -> ok.
execute_service_action_on_single_node([Node | _], Service, Action) ->
    Host = onepanel_cluster:node_to_host(Node),
    ?assertEqual(ok, rpc:call(Node, service, apply,
        [Service, Action, #{hosts => [Host]}])).


%%--------------------------------------------------------------------
%% @private
%% @doc Executes service action on all nodes where service was configured.
%%--------------------------------------------------------------------
-spec execute_service_action(Nodes :: [node()], Service :: service:name(),
    Action :: service:action()) -> ok.
execute_service_action([Node | _], Service, Action) ->
    ?assertEqual(ok, rpc:call(Node, service, apply, [Service, Action, #{}])).

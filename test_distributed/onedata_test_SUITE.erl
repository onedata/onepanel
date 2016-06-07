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
-module(onedata_test_SUITE).
-author("Krzysztof Trzepla").

-include("db/models.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/performance.hrl").

%% export for ct
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% tests
-export([
    onedata_deployment_test/1
]).

all() ->
    ?ALL([
        onedata_deployment_test
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

onedata_deployment_test(Config) ->
    [OzNode | _] = OzNodes = ?config(oz_nodes, Config),
    OzHosts = lists:usort(onepanel_utils:nodes_to_hosts(OzNodes)),

    ?assertEqual(ok, rpc:call(OzNode, service, apply,
        [?SERVICE_OZ, deploy, #{
            ?SERVICE_CB => #{
                hosts => OzHosts
            },
            ?SERVICE_CM => #{
                hosts => OzHosts, main_cm_host => hd(OzHosts), worker_num => 2
            },
            ?SERVICE_OZW => #{
                hosts => OzHosts, main_cm_host => hd(OzHosts),
                cm_hosts => OzHosts, db_hosts => OzHosts
            }
        }]
    )),

    [OpNode | _] = OpNodes = ?config(op_nodes, Config),
    OpHosts = lists:usort(onepanel_utils:nodes_to_hosts(OpNodes)),

    ?assertEqual(ok, rpc:call(OpNode, service, apply,
        [?SERVICE_OP, deploy, #{
            ?SERVICE_CB => #{
                hosts => OpHosts
            },
            ?SERVICE_CM => #{
                hosts => OpHosts, main_cm_host => hd(OpHosts), worker_num => 2
            },
            ?SERVICE_OPW => #{
                hosts => OpHosts, main_cm_host => hd(OpHosts),
                cm_hosts => OpHosts, db_hosts => OpHosts
            }
        }]
    )).

%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================

init_per_suite(Config) ->
    NewConfig = ?TEST_INIT(Config, ?TEST_FILE(Config, "env_desc.json")),
    onepanel_test_utils:ensure_initailized(NewConfig).


end_per_suite(Config) ->
    test_node_starter:clean_environment(Config).
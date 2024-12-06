%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone/Oneprovider clusters miscellaneous functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_misc_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    op_fetch_compatibility_registry_test/1,
    cluster_clocks_sync_test/1,

    oz_services_healthy_status_test/1,
    op_services_healthy_status_test/1
]).

all() -> [
    op_fetch_compatibility_registry_test,
    cluster_clocks_sync_test,

    oz_services_healthy_status_test,
    op_services_healthy_status_test
].

-define(AWAIT_CLOCK_SYNC_ATTEMPTS, 30).


%%%===================================================================
%%% Tests
%%%===================================================================


op_fetch_compatibility_registry_test(_Config) ->
    % place some initial, outdated compatibility registry on all nodes
    OpPanelNodes = panel_test_utils:get_panel_nodes(krakow),
    OldRevision = 2000010100,
    lists:foreach(fun(Node) ->
        CurrentRegistryPath = panel_test_rpc:call(Node, ctool, get_env, [current_compatibility_registry_file]),
        DefaultRegistryPath = panel_test_rpc:call(Node, ctool, get_env, [default_compatibility_registry_file]),
        OldRegistry = #{<<"revision">> => OldRevision},
        ok = panel_test_rpc:call(Node, ctool, set_env, [compatibility_registry_mirrors, []]),
        ok = panel_test_rpc:call(Node, file, write_file, [CurrentRegistryPath, json_utils:encode(OldRegistry)]),
        ok = panel_test_rpc:call(Node, file, write_file, [DefaultRegistryPath, json_utils:encode(OldRegistry)]),
        ok = panel_test_rpc:call(Node, compatibility, clear_registry_cache, [])
    end, OpPanelNodes),

    % force a registry query that should cause Onepanel to fetch a newer one from Onezone
    ChosenNode = lists_utils:random_element(OpPanelNodes),
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(ChosenNode, <<"/provider/onezone_info">>, #{auth => root})
    ),

    NewerRevision = peek_current_registry_revision_on_node(ChosenNode),
    ?assertNotEqual(NewerRevision, OldRevision),

    % in the process, the new registry should be propagated to all nodes
    lists:foreach(fun(Node) ->
        ?assertEqual(NewerRevision, peek_current_registry_revision_on_node(Node))
    end, OpPanelNodes -- [ChosenNode]).


cluster_clocks_sync_test(_Config) ->
    % the clock_synchronization_interval_seconds env is set in the env.json to
    % 5 seconds for the sake of this test

    OzpNodes = panel_test_utils:get_panel_nodes(zone),
    % master node is selected as the first from sorted list
    OzpMasterNode = hd(lists:sort(OzpNodes)),
    OzCmNode = panel_test_rpc:call(OzpMasterNode, service_cluster_manager, get_current_primary_node, []),
    OzwNodes = panel_test_rpc:call(OzpMasterNode, service_oz_worker, get_nodes, []),

    OppNodes = panel_test_utils:get_panel_nodes(krakow),
    OpCmNode = panel_test_rpc:call(hd(OppNodes), service_cluster_manager, get_current_primary_node, []),
    OpwNodes = panel_test_rpc:call(hd(OppNodes), service_op_worker, get_nodes, []),

    IsSyncedWithMaster = fun(Node) ->
        MasterTimestamp = panel_test_rpc:call(OzpMasterNode, global_clock, timestamp_millis, []),
        NodeTimestamp = panel_test_rpc:call(Node, global_clock, timestamp_millis, []),
        are_timestamps_in_sync(MasterTimestamp, NodeTimestamp)
    end,

    % after the environment is deployed and periodic sync has run at least once,
    % all nodes in Onezone and Oneprovider clusters should be synced with the master Onezone node
    AllNonMasterNodes = lists:flatten([
        OzpNodes, OzCmNode, OzwNodes,
        OppNodes, OpCmNode, OpwNodes
    ]) -- [OzpMasterNode],

    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    Bias = ?RAND_INT(20, 60),
    % simulate a situation when the time changes on the master node by 50 hours
    % and see if (after some time) the clocks are unified again
    ok = panel_test_rpc:call(OzpMasterNode, global_clock, store_bias, [local_clock, timer:hours(Bias)]),
    ?assertEqual(false, lists:all(IsSyncedWithMaster, AllNonMasterNodes)),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    % simulate a situation when the time changes on another, non-master node by
    % 50 hours and see if (after some time) it catches up with the master again
    RandomNonMasterNode = lists_utils:random_element(AllNonMasterNodes),
    panel_test_rpc:call(RandomNonMasterNode, global_clock, store_bias, [local_clock, timer:hours(-Bias)]),
    ?assertEqual(false, IsSyncedWithMaster(RandomNonMasterNode)),
    ?assertEqual(true, IsSyncedWithMaster(RandomNonMasterNode), ?AWAIT_CLOCK_SYNC_ATTEMPTS),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS),

    % simulate a situation when one of the nodes fails to synchronize its clock
    % and check if the procedure that restarts clock sync correctly awaits and
    % retries until the problem is resolved
    OppMasterNode = hd(lists:sort(OppNodes)), % master node is selected as the first from sorted list
    % below envs make it impossible for the node to successfully synchronize
    panel_test_rpc:call(OppMasterNode, ctool, set_env, [clock_sync_satisfying_delay, -1]),
    panel_test_rpc:call(OppMasterNode, ctool, set_env, [clock_sync_max_allowed_delay, -1]),

    % try to restart the clock sync in an async process (it should block until the sync is successful)
    panel_test_rpc:call(OppMasterNode, global_clock, reset_to_system_time, []),
    Master = self(),
    AsyncProcess = spawn(fun() ->
        Result = panel_test_rpc:call(OppMasterNode, oneprovider_cluster_clocks, restart_periodic_sync, []),
        Master ! {restart_result, Result}
    end),

    ?assertEqual(false, IsSyncedWithMaster(OppMasterNode)),
    timer:sleep(timer:seconds(10)),
    ?assertEqual(false, IsSyncedWithMaster(OppMasterNode)),
    % check that the async process is still waiting
    ?assert(erlang:is_process_alive(AsyncProcess)),

    % bring back the sane config and wait until the sync is successful
    panel_test_rpc:call(OppMasterNode, ctool, set_env, [clock_sync_satisfying_delay, 2000]),
    panel_test_rpc:call(OppMasterNode, ctool, set_env, [clock_sync_max_allowed_delay, 10000]),
    ?assertReceivedMatch({restart_result, ok}, timer:seconds(10)),
    ?assertEqual(true, lists:all(IsSyncedWithMaster, AllNonMasterNodes), ?AWAIT_CLOCK_SYNC_ATTEMPTS).


oz_services_healthy_status_test(_Config) ->
    cluster_services_healthy_status_test_base(zone, ?SERVICE_OZ, [
        ?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OZW
    ]).


op_services_healthy_status_test(_Config) ->
    cluster_services_healthy_status_test_base(krakow, ?SERVICE_OP, [
        ?SERVICE_CB, ?SERVICE_CM, ?SERVICE_OPW
    ]).


%% @private
cluster_services_healthy_status_test_base(EntitySelector, MainService, SubServices) ->
    PanelNodes = panel_test_utils:get_panel_nodes(EntitySelector),

    lists:foreach(fun(Service) ->
        ServiceModule = service:get_module(Service),

        lists:foreach(fun(PanelNode) ->
            Results = onepanel_test_utils:service_host_action(PanelNode, Service, status),
            onepanel_test_utils:assert_service_action_result(
                ServiceModule, status, [PanelNode], healthy, Results
            )
        end, PanelNodes),

        Results = onepanel_test_utils:service_action(hd(PanelNodes), Service, status),
        onepanel_test_utils:assert_service_action_result(
            ServiceModule, status, PanelNodes, healthy, Results
        )
    end, SubServices),

    % Assert main service status consists/includes all sub services statuses
    Results = onepanel_test_utils:service_action(hd(PanelNodes), MainService, status),
    lists:foreach(fun(Service) ->
        ServiceModule = service:get_module(Service),
        onepanel_test_utils:assert_service_action_result(
            ServiceModule, status, PanelNodes, healthy, Results
        )
    end, SubServices).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_2nodes",
        envs = [
            {oz_panel, onepanel, [
                {clock_synchronization_interval_seconds, 5}
            ]},
            {op_panel, onepanel, [
                {clock_synchronization_interval_seconds, 5}
            ]}
        ]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec peek_current_registry_revision_on_node(node()) -> integer().
peek_current_registry_revision_on_node(Node) ->
    Resolver = compatibility:build_resolver([Node], []),
    {ok, Rev} = panel_test_rpc:call(Node, compatibility, peek_current_registry_revision, [Resolver]),
    Rev.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compares two timestamps and returns true if they are at most 5 seconds apart
%% (bigger clock differences should be tested to make this a reliable check).
%% @end
%%--------------------------------------------------------------------
-spec are_timestamps_in_sync(time:millis(), time:millis()) -> boolean().
are_timestamps_in_sync(TimestampA, TimestampB) ->
    TimestampA - TimestampB > -5000 andalso TimestampA - TimestampB < 5000.

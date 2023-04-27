%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning db_disk_usage_monitor.
%%% @end
%%%-------------------------------------------------------------------
-module(db_disk_usage_monitor_test_SUITE).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    db_disk_usage_periodic_check_test/1
]).

all() -> [
    db_disk_usage_periodic_check_test
].

-define(rpc(__PANEL_SELECTOR, __EXPRESSION), panel_test_rpc:call(__PANEL_SELECTOR, fun() ->
    __EXPRESSION
end)).


%%%===================================================================
%%% API
%%%===================================================================


db_disk_usage_periodic_check_test(_Config) ->
    TargetPanelNodes = get_panel_nodes(),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    assert_service_circuit_breaker_status(disabled, TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_emergency_threshold, 0.001),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),

    timer:sleep(timer:seconds(2)),
    assert_service_circuit_breaker_status(enabled, TargetPanelNodes),
    timer:sleep(timer:seconds(2)),
    assert_service_circuit_breaker_status(enabled, TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_emergency_threshold, 0.9),

    timer:sleep(timer:seconds(2)),
    assert_service_circuit_breaker_status(disabled, TargetPanelNodes),
    timer:sleep(timer:seconds(2)),
    assert_service_circuit_breaker_status(disabled, TargetPanelNodes).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
get_panel_nodes() ->
    case rand:uniform(2) of
        1 -> oct_background:get_zone_panels();
        2 -> oct_background:get_provider_panels(krakow)
    end.


%% @private
assert_service_circuit_breaker_status(ExpStatus, Nodes) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(ExpStatus, get_panel_env(Node, service_circuit_breaker))
    end, Nodes).


%% @private
get_panel_env(Node, Env) ->
    ?rpc(Node, onepanel_env:get(Env, ?APP_NAME)).


%% @private
set_panel_env(Nodes, Env, Value) ->
    ?rpc(?RAND_ELEMENT(Nodes), onepanel_env:set(Nodes, Env, Value, ?APP_NAME)).

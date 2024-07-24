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

-include("api_test_runner.hrl").
-include("names.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1,  end_per_suite/1]).

-export([
    db_disk_usage_periodic_check_test/1,
    panel_rest_block_test/1,
    reliability_of_service_circuit_breaker_state_variable_setting/1
]).

all() -> [
    db_disk_usage_periodic_check_test,
    panel_rest_block_test
%%    reliability_of_service_circuit_breaker_state_variable_setting
].

-define(rpc(__PANEL_SELECTOR, __EXPRESSION), panel_test_rpc:call(__PANEL_SELECTOR, fun() ->
    __EXPRESSION
end)).

-define(ATTEMPTS, 10).


%%%===================================================================
%%% API
%%%===================================================================


db_disk_usage_periodic_check_test(_Config) ->
    TargetPanelNodes = get_panel_nodes_of_random_cluster(),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    set_remote_if_not_set(?RAND_ELEMENT(oct_background:get_zone_panels()), service_circuit_breaker_state),
    assert_service_circuit_breaker_state(closed, TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),
    assert_service_circuit_breaker_state(open, TargetPanelNodes),
    assert_service_circuit_breaker_state(open, TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    assert_service_circuit_breaker_state(closed, TargetPanelNodes),
    assert_service_circuit_breaker_state(closed, TargetPanelNodes).


panel_rest_block_test(_Config) ->
    TargetPanelNodes = get_panel_nodes_of_random_cluster(),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),
    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    set_remote_if_not_set(?RAND_ELEMENT(oct_background:get_zone_panels()), service_circuit_breaker_state),

    assert_service_circuit_breaker_state(open, TargetPanelNodes),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"REST call returns 503 when circuit breaker is enabled">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = #client_spec{forbidden = [
                {Client, ?ERROR_SERVICE_UNAVAILABLE} || Client <- [root, member, guest, peer]
            ]},
            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"test_image">>
            } end
        }
    ])),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    assert_service_circuit_breaker_state(closed, TargetPanelNodes),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"REST call works when circuit breaker is disabled">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = #client_spec{correct = [root, member, guest, peer]},
            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"test_image">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(_) -> ok end)
        }
    ])).


reliability_of_service_circuit_breaker_state_variable_setting(_Config) ->
    TargetPanelNodes = get_panel_nodes_of_random_cluster(),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),
    ZoneNode = ?RAND_ELEMENT(oct_background:get_zone_panels()),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),

    %% set panel variable
    ?rpc(TargetPanelNode, onepanel_env:set(service_circuit_breaker_state, some_incorrect_value, ?APP_NAME)),
    onepanel_env:set_remote(ZoneNode, [service_circuit_breaker_state], some_incorrect_value, ?SERVICE_OZW),

    %% TODO VFS-12110 after some time period helfcheck should set service_circuit_breaker_state to default
    assert_service_circuit_breaker_state(closed, TargetPanelNodes).




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
get_panel_nodes_of_random_cluster() ->
    case rand:uniform(2) of
        1 -> oct_background:get_zone_panels();
        2 -> oct_background:get_provider_panels(krakow)
    end.


%% @private
assert_service_circuit_breaker_state(ExpStatus, Nodes) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(ExpStatus, get_panel_env(Node, service_circuit_breaker_state, closed), ?ATTEMPTS),
        case lists:member(Node, oct_background:get_zone_panels()) of
            true ->
                ?assertEqual(ExpStatus, onepanel_env:get_remote(
                    Node, [service_circuit_breaker_state], ?SERVICE_OZW
                ), ?ATTEMPTS);
            false ->
                ok
        end
    end, Nodes).


%% @private
get_panel_env(Node, Env, Default) ->
    ?rpc(Node, onepanel_env:get(Env, ?APP_NAME, Default)).


%% @private
set_panel_env(Nodes, Env, Value) ->
    ?rpc(?RAND_ELEMENT(Nodes), onepanel_env:set(Nodes, Env, Value, ?APP_NAME)).


%% @private
set_remote_if_not_set(Node, Key) ->
    try
        ?assertEqual(closed, onepanel_env:get_remote(Node, [Key], ?SERVICE_OZW), ?ATTEMPTS)
    catch _Type:_Message:_Stacktrace ->
        ok = onepanel_env:set_remote(Node, [Key], closed, ?SERVICE_OZW)
    end.
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
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1,  end_per_suite/1]).

-export([
    db_disk_usage_periodic_check_test/1,
    panel_rest_block_test/1,
    worker_rest_block_test/1,
    reliability_of_service_circuit_breaker_state_variable_setting_test/1
]).

all() -> [
    db_disk_usage_periodic_check_test,
    panel_rest_block_test,
    worker_rest_block_test,
    reliability_of_service_circuit_breaker_state_variable_setting_test
].

-define(rpc(__PANEL_SELECTOR, __EXPRESSION), panel_test_rpc:call(__PANEL_SELECTOR, fun() ->
    __EXPRESSION
end)).

-define(ATTEMPTS, 10).


%%%===================================================================
%%% API
%%%===================================================================


db_disk_usage_periodic_check_test(_Config) ->
    db_disk_usage_periodic_check_test_base(?SERVICE_OZW),
    db_disk_usage_periodic_check_test_base(?SERVICE_OPW).


panel_rest_block_test(_Config) ->
    panel_rest_block_test_base(?SERVICE_OZW),
    panel_rest_block_test_base(?SERVICE_OPW).


worker_rest_block_test(_Config) ->
    worker_rest_block_test_base(?SERVICE_OZW).
    %% TODO VFS-10787 test blocking operation in op
    %% worker_rest_block_test_base(?SERVICE_OPW).


reliability_of_service_circuit_breaker_state_variable_setting_test(_Config) ->
    reliability_of_service_circuit_breaker_state_variable_setting_test_base(?SERVICE_OZW),
    reliability_of_service_circuit_breaker_state_variable_setting_test_base(?SERVICE_OPW).


%% @private
-spec db_disk_usage_periodic_check_test_base(oct_background:node()) -> ok.
db_disk_usage_periodic_check_test_base(Service) ->
    TargetPanelNodes = get_panel_nodes(Service),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    assert_service_circuit_breaker_state(closed, Service),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),
    assert_service_circuit_breaker_state(open, Service),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    assert_service_circuit_breaker_state(closed, Service).


%% @private
-spec panel_rest_block_test_base(oct_background:node()) -> ok.
panel_rest_block_test_base(Service) ->
    TargetPanelNodes = get_panel_nodes(Service),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    assert_service_circuit_breaker_state(open, Service),

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
    assert_service_circuit_breaker_state(closed, Service),

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


%% @private
-spec worker_rest_block_test_base(oct_background:node()) -> ok.
worker_rest_block_test_base(Service) ->
    TargetPanelNodes = get_panel_nodes(Service),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    Url = str_utils:format_bin(
        "https://~ts/api/v3/~ts/configuration",
        [get_worker_domain(Service), get_worker_name(Service)]
    ),
    CaCerts = panel_test_rpc:get_cert_chain_ders(TargetPanelNode),
    Opts = [{ssl_options, [{cacerts, CaCerts}]}, {recv_timeout, 60000}],

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    assert_service_circuit_breaker_state(open, Service),

    ?assertMatch({ok, ?HTTP_503_SERVICE_UNAVAILABLE, _, _},  http_client:get(Url, #{}, <<>>, Opts), ?ATTEMPTS),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    assert_service_circuit_breaker_state(closed, Service),

    ?assertMatch({ok, ?HTTP_200_OK, _, _}, http_client:get(Url, #{}, <<>>, Opts)).


%% @private
-spec reliability_of_service_circuit_breaker_state_variable_setting_test_base(oct_background:node()) -> ok.
reliability_of_service_circuit_breaker_state_variable_setting_test_base(Service) ->
    TargetPanelNodes = get_panel_nodes(Service),
    TargetPanelNode = ?RAND_ELEMENT(TargetPanelNodes),

    set_panel_env(TargetPanelNodes, db_disk_usage_check_interval_seconds, 1),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),

    ?rpc(TargetPanelNode, db_disk_usage_monitor:restart_periodic_check()),
    assert_service_circuit_breaker_state(closed, Service),

    test_utils:mock_new(TargetPanelNodes, [onepanel_env]),
    test_utils:mock_expect(TargetPanelNodes, onepanel_env, set_remote, fun(_, _, _, _) -> throw(?ERROR_FORBIDDEN) end),
    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 0.00001),

    assert_service_circuit_breaker_state(open, closed, Service),
    timer:sleep(2000),
    assert_service_circuit_breaker_state(open, closed, Service),
    test_utils:mock_unload(TargetPanelNodes),

    assert_service_circuit_breaker_state(open, Service),

    %% Simulate that Onepanel was unable to set the circuit breaker state as open at worker nodes
    %%(e.g. temporary connectivity problems) so it's still closed. The next heathcheck should fix that.
    set_worker_env(TargetPanelNodes, service_circuit_breaker_state, closed, Service),
    assert_service_circuit_breaker_state(open, Service),

    set_panel_env(TargetPanelNodes, db_disk_usage_circuit_breaker_activation_threshold, 1.0),
    assert_service_circuit_breaker_state(closed, Service),

    %% Simulate that Onepanel was unable to set the circuit breaker state as closed at worker nodes
    %%(e.g. temporary connectivity problems) so it's still open. The next heathcheck should fix that.
    set_worker_env(TargetPanelNodes, service_circuit_breaker_state, open, Service),
    assert_service_circuit_breaker_state(closed, Service).


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
get_panel_nodes(?SERVICE_OZW) -> oct_background:get_zone_panels();
get_panel_nodes(?SERVICE_OPW) -> oct_background:get_provider_panels(krakow).


%% @private
get_worker_nodes(?SERVICE_OZW) -> oct_background:get_zone_nodes();
get_worker_nodes(?SERVICE_OPW) -> oct_background:get_provider_nodes(krakow).


%% @private
get_worker_domain(?SERVICE_OZW) -> oct_background:get_zone_domain();
get_worker_domain(?SERVICE_OPW) -> oct_background:get_provider_domain(krakow).


%% @private
get_worker_name(?SERVICE_OZW) -> ?SERVICE_OZ;
get_worker_name(?SERVICE_OPW) -> ?SERVICE_OP.


%% @private
assert_service_circuit_breaker_state(ExpState, Service) ->
    assert_service_circuit_breaker_state(ExpState, ExpState, Service).

%% @private
assert_service_circuit_breaker_state(ExpStateInPanel, ExpStateInWorker, Service) ->
    lists:foreach(fun(Node) ->
        ?assertEqual(ExpStateInPanel, get_panel_env(Node, service_circuit_breaker_state, closed), ?ATTEMPTS)
    end, get_panel_nodes(Service)),
    lists:foreach(fun(Worker) ->
        ?assertEqual(ExpStateInWorker, get_worker_env(Worker, service_circuit_breaker_state, Service, closed), ?ATTEMPTS)
    end, get_worker_nodes(Service)).


%% @private
get_panel_env(Node, Env, Default) ->
    ?rpc(Node, onepanel_env:get(Env, ?APP_NAME, Default)).


%% @private
set_panel_env(Nodes, Env, Value) ->
    ?rpc(?RAND_ELEMENT(Nodes), onepanel_env:set(Nodes, Env, Value, ?APP_NAME)).


%% @private
get_worker_env(Node, Env, Service, Value) ->
    onepanel_env:get_remote(Node, [Env], Service, Value).


%% @private
set_worker_env(Node, Env, Service, Value) ->
    onepanel_env:set_remote(Node, [Env], Value, Service).


%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone/Oneprovider cluster services.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_services_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("cluster_deployment_test_utils.hrl").
-include("names.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    ones3_stop_start/1
]).

all() -> [
    ones3_stop_start
].

-define(ATTEMPTS, 60).


%%%===================================================================
%%% Tests
%%%===================================================================


ones3_stop_start(Config) ->
    [OpPanelNode1, OpPanelNode2] = ?config(op_panel_nodes, Config),
    Host1 = dns_test_utils:get_hostname(OpPanelNode1),
    Host2 = dns_test_utils:get_hostname(OpPanelNode2),

    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"healthy">>, Host2 => <<"healthy">>}),

    cluster_management_test_utils:toggle_ones3_on_host(OpPanelNode1, Host2, stop),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"healthy">>, Host2 => <<"stopped">>}),

    cluster_management_test_utils:toggle_ones3_cluster_wide(OpPanelNode1, start),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"healthy">>, Host2 => <<"healthy">>}),

    cluster_management_test_utils:toggle_ones3_cluster_wide(OpPanelNode1, stop),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"stopped">>, Host2 => <<"stopped">>}),

    cluster_management_test_utils:toggle_ones3_on_host(OpPanelNode1, Host1, start),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"healthy">>, Host2 => <<"stopped">>}),

    cluster_management_test_utils:toggle_ones3_cluster_wide(OpPanelNode1, stop),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"stopped">>, Host2 => <<"stopped">>}),

    cluster_management_test_utils:toggle_ones3_cluster_wide(OpPanelNode1, start),
    check_ones3_statuses(OpPanelNode1, #{Host1 => <<"healthy">>, Host2 => <<"healthy">>}),

    ok.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_2ones3"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
check_ones3_statuses(Node, StatusPerHost) ->
    ?assertEqual(StatusPerHost, cluster_management_test_utils:get_ones3_status_cluster_wide(Node), ?ATTEMPTS),

    maps:foreach(fun(Host, ExpStatus) ->
        ?assertEqual(ExpStatus, cluster_management_test_utils:get_ones3_status_on_host(Node, Host))
    end, StatusPerHost).

%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests of Onezone cluster services.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_services_oz_test_SUITE).
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
    oz_worker_stop_start_test/1,
    oz_cm_stop_start_test/1,
    oz_database_stop_start_test/1,

    oz_stop_restart_test/1
]).

all() -> [
    % TODO VFS-12721 fix stopping worker/manager on single host in cluster
%%    oz_worker_stop_start_test,
%%    oz_cm_stop_start_test,
    oz_database_stop_start_test,

    oz_stop_restart_test
].

-define(ALL_SERVICES, [worker, manager, database]).

-define(ATTEMPTS, 60).


%%%===================================================================
%%% Tests
%%%===================================================================


oz_worker_stop_start_test(Config) ->
    service_stop_start_test_base(worker, Config).


oz_cm_stop_start_test(Config) ->
    service_stop_start_test_base(manager, Config).


oz_database_stop_start_test(Config) ->
    service_stop_start_test_base(database, Config).


%% @private
service_stop_start_test_base(Service, Config) ->
    PanelNodes = ?config(oz_panel_nodes, Config),
    cl_services_test_base:service_stop_start_test_base(?ONEZONE, Service, PanelNodes).


oz_stop_restart_test(Config) ->
    PanelNodes = ?config(oz_panel_nodes, Config),
    cl_services_test_base:main_service_stop_restart_test_base(?ONEZONE, ?ALL_SERVICES, PanelNodes).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1oz_2nodes",
        envs = [
            {oz_panel, onepanel, [
                % Stopping service disables healthcheck BUT due to current
                % implementation it is possible for race to occur with default
                % check interval.
                {services_check_period, 6000000000}
            ]}
        ]
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

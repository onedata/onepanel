%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onepanel cluster API endpoints (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_cluster_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([
    groups/0,
    all/0,

    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_current_zone_cluster_details_test/1,
    get_current_krakow_cluster_details_test/1,

    get_zone_cluster_details_from_zone_test/1,
    get_zone_cluster_details_from_krakow_test/1,
    get_krakow_cluster_details_from_zone_test/1,
    get_krakow_cluster_details_from_krakow_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        get_current_zone_cluster_details_test,
        get_current_krakow_cluster_details_test,

        get_zone_cluster_details_from_zone_test,
        get_zone_cluster_details_from_krakow_test,
        get_krakow_cluster_details_from_zone_test,
        get_krakow_cluster_details_from_krakow_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


get_current_zone_cluster_details_test(_Config) ->
    get_current_cluster_details_test_base(zone).


get_current_krakow_cluster_details_test(_Config) ->
    get_current_cluster_details_test_base(krakow).


%% @private
-spec get_current_cluster_details_test_base(oct_background:entity_selector()) ->
    boolean().
get_current_cluster_details_test_base(PanelEntitySelector) ->
    ExpClusterDetails = get_cluster_details(PanelEntitySelector),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster details using /cluster REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(PanelEntitySelector),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"cluster">>
            } end,
            validate_result_fun = build_validate_cluster_details_fun(ExpClusterDetails)
        }
    ])).


get_zone_cluster_details_from_zone_test(_Config) ->
    get_cluster_details_test_base(zone, zone).


get_zone_cluster_details_from_krakow_test(_Config) ->
    get_cluster_details_test_base(zone, krakow).


get_krakow_cluster_details_from_zone_test(_Config) ->
    get_cluster_details_test_base(krakow, zone).


get_krakow_cluster_details_from_krakow_test(_Config) ->
    get_cluster_details_test_base(krakow, krakow).


%% @private
-spec get_cluster_details_test_base(oct_background:entity_selector(), oct_background:entity_selector()) ->
    boolean().
get_cluster_details_test_base(QueryPanelEntitySelector, TargetClusterEntitySelector) ->
    ExpClusterDetails = get_cluster_details(TargetClusterEntitySelector),
    ClusterId = maps:get(<<"id">>, ExpClusterDetails),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get specific cluster details using /user/clusters/{id} REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(QueryPanelEntitySelector),
            client_spec = api_test_utils:build_only_member_allowed_client_spec(QueryPanelEntitySelector),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = str_utils:format_bin("user/clusters/~ts", [ClusterId])
            } end,
            validate_result_fun = build_validate_cluster_details_fun(ExpClusterDetails)
        }
    ])).


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
-spec get_cluster_details(oct_background:entity_selector()) ->
    json_utils:json_map().
get_cluster_details(PanelEntitySelector) ->
    PanelNode = ?RAND_ELEMENT(panel_test_utils:get_panel_nodes(PanelEntitySelector)),
    {ok, ServiceId} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_id)),
    {ok, PanelReleaseVersion} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_release_version)),
    {ok, PanelBuildVersion} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_build_version)),
    PanelGuiPackagePath = ?rpc(PanelNode, onepanel_env:get(gui_package_path)),
    {ok, PanelGuiHash} = ?rpc(PanelNode, gui:package_hash(PanelGuiPackagePath)),

    WorkerNode = ?RAND_ELEMENT(panel_test_utils:get_worker_nodes(PanelEntitySelector)),
    {ok, WorkerReleaseVersion} = ?rpc(WorkerNode, application:get_env(ctool, onedata_service_release_version)),
    {ok, WorkerBuildVersion} = ?rpc(WorkerNode, application:get_env(ctool, onedata_service_build_version)),
    WorkerGuiPackagePath = case PanelEntitySelector of
        zone -> ozw_test_rpc:get_env(ozw_gui_package_path);
        krakow -> opw_test_rpc:get_env(WorkerNode, gui_package_path)
    end,
    {ok, WorkerGuiHash} = ?rpc(WorkerNode, gui:package_hash(WorkerGuiPackagePath)),

    #{
        <<"id">> => ServiceId,  %% Cluster id == service id
        <<"type">> => case PanelEntitySelector of
            zone -> <<"onezone">>;
            krakow -> <<"oneprovider">>
        end,
        <<"serviceId">> => ServiceId,
        <<"workerVersion">> => #{
            <<"release">> => WorkerReleaseVersion,
            <<"build">> => WorkerBuildVersion,
            <<"gui">> => WorkerGuiHash
        },
        <<"onepanelVersion">> => #{
            <<"release">> => PanelReleaseVersion,
            <<"build">> => PanelBuildVersion,
            <<"gui">> => PanelGuiHash
        },
        <<"onepanelProxy">> => true
    }.


%% @private
-spec build_validate_cluster_details_fun(json_utils:json_map()) ->
    api_test_runner:validate_call_result_fun().
build_validate_cluster_details_fun(ExpClusterDetails) ->
    api_test_validate:http_200_ok(fun(RespBody) ->
        %% TODO VFS-13055 add creationTime and creator to zone cluster info and test it
        TestedRespBody = maps:without([<<"creationTime">>, <<"creator">>], RespBody),
        ?assertEqual(ExpClusterDetails, TestedRespBody)
    end).

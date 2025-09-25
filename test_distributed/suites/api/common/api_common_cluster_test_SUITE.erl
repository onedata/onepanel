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
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_current_zone_cluster_details_test/1,
    get_current_krakow_cluster_details_test/1
]).

all() -> [
    get_current_zone_cluster_details_test,
    get_current_krakow_cluster_details_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_current_zone_cluster_details_test(Config) ->
    get_current_cluster_details_test_base(Config, ?OZ_PANEL, zone).


get_current_krakow_cluster_details_test(Config) ->
    get_current_cluster_details_test_base(Config, ?OP_PANEL, krakow).


%% @private
-spec get_current_cluster_details_test_base(test_config:config(), atom(), oct_background:entity_selector()) ->
    boolean().
get_current_cluster_details_test_base(_Config, PanelType, PanelEntitySelector) ->
    ExpClusterDetails = get_cluster_details(PanelType, PanelEntitySelector),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster details using /cluster REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = build_client_spec(PanelType, PanelEntitySelector),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"cluster">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                %% TODO VFS-13055 add creationTime and creator to zone cluster info and test it
                TestedRespBody = maps:without([<<"creationTime">>, <<"creator">>], RespBody),
                ?assertEqual(ExpClusterDetails, TestedRespBody)
            end)
        }
    ])).


%% @private
-spec get_cluster_details(atom(), oct_background:entity_selector()) ->
    json_utils:json_map().
get_cluster_details(PanelType, PanelEntitySelector) ->
    PanelNode = ?RAND_ELEMENT(panel_test_utils:get_panel_nodes(PanelEntitySelector)),
    {ok, ServiceId} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_id)),
    {ok, PanelReleaseVersion} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_release_version)),
    {ok, PanelBuildVersion} = ?rpc(PanelNode, application:get_env(ctool, onedata_service_build_version)),
    PanelGuiPackagePath = ?rpc(PanelNode, onepanel_env:get(gui_package_path)),
    {ok, PanelGuiHash} = ?rpc(PanelNode, gui:package_hash(PanelGuiPackagePath)),

    WorkerNode = ?RAND_ELEMENT(panel_test_utils:get_worker_nodes(PanelEntitySelector)),
    {ok, WorkerReleaseVersion} = ?rpc(WorkerNode, application:get_env(ctool, onedata_service_release_version)),
    {ok, WorkerBuildVersion} = ?rpc(WorkerNode, application:get_env(ctool, onedata_service_build_version)),
    WorkerGuiPackagePath = case PanelType of
        ?OZ_PANEL -> ozw_test_rpc:get_env(ozw_gui_package_path);
        ?OP_PANEL -> opw_test_rpc:get_env(WorkerNode, gui_package_path)
    end,
    {ok, WorkerGuiHash} = ?rpc(WorkerNode, gui:package_hash(WorkerGuiPackagePath)),

    #{
        <<"id">> => ServiceId,  %% Cluster id == service id
        <<"type">> => case PanelType of
            ?OZ_PANEL -> <<"onezone">>;
            ?OP_PANEL -> <<"oneprovider">>
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
-spec build_client_spec(atom(), oct_background:entity_selector()) ->
    api_test_runner:client_spec().
build_client_spec(PanelType, PanelEntitySelector) ->
    EntityId = oct_background:to_entity_id(PanelEntitySelector),

    #client_spec{
        correct = [
            root,
            member
        ],
        unauthorized = [
            guest,
            {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(PanelType, EntityId))}
            | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
        ],
        forbidden = [
            peer
        ]
    }.

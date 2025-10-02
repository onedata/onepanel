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
-include_lib("ctool/include/privileges.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([
    groups/0, all/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2
]).

-export([
    get_zone_cluster_members_summary_test/1,
    get_krakow_cluster_members_summary_test/1,

    get_current_zone_cluster_details_test/1,
    get_current_krakow_cluster_details_test/1,

    get_zone_cluster_details_from_zone_test/1,
    get_zone_cluster_details_from_krakow_test/1,
    get_krakow_cluster_details_from_zone_test/1,
    get_krakow_cluster_details_from_krakow_test/1,

    get_zone_cluster_public_configuration_test/1,
    get_krakow_cluster_public_configuration_test/1,

    get_zone_cluster_cookie_test/1,
    get_krakow_cluster_cookie_test/1,

    get_zone_cluster_progress_test/1,
    get_krakow_cluster_progress_test/1,

    update_zone_cluster_progress_test/1,
    update_krakow_cluster_progress_test/1
]).

groups() -> [
    {cluster_members_summery_tests, [], [
        % These tests are sensitive to numbers of users created and should be run first
        get_zone_cluster_members_summary_test,
        get_krakow_cluster_members_summary_test
    ]},
    {cluster_details_tests, [parallel], [
        get_current_zone_cluster_details_test,
        get_current_krakow_cluster_details_test,

        get_zone_cluster_details_from_zone_test,
        get_zone_cluster_details_from_krakow_test,
        get_krakow_cluster_details_from_zone_test,
        get_krakow_cluster_details_from_krakow_test
    ]},
    {cluster_config_tests, [parallel], [
        get_zone_cluster_public_configuration_test,
        get_krakow_cluster_public_configuration_test,

        get_zone_cluster_cookie_test,
        get_krakow_cluster_cookie_test
    ]},
    {cluster_progress_tests, [], [
        get_zone_cluster_progress_test,
        get_krakow_cluster_progress_test,

        update_zone_cluster_progress_test,
        update_krakow_cluster_progress_test
    ]}
].

all() -> [
    {group, cluster_members_summery_tests},
    {group, cluster_details_tests},
    {group, cluster_config_tests},
    {group, cluster_progress_tests}
].


-define(ZONE_PROGRESS_MARKERS, [
    <<"clusterNodes">>, <<"clusterIps">>, <<"dnsCheck">>, <<"webCertificate">>
]).
-define(PROVIDER_PROGRESS_MARKERS, [
    <<"clusterNodes">>, <<"clusterIps">>, <<"dnsCheck">>, <<"webCertificate">>, <<"storagesSetup">>
]).


%%%===================================================================
%%% API
%%%===================================================================


get_zone_cluster_members_summary_test(_Config) ->
    get_cluster_members_summary_test_base(zone).


get_krakow_cluster_members_summary_test(_Config) ->
    get_cluster_members_summary_test_base(krakow).


%% @private
-spec get_cluster_members_summary_test_base(oct_background:entity_selector()) ->
    boolean().
get_cluster_members_summary_test_base(PanelEntitySelector) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster members summary using /cluster/members_summary REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                PanelEntitySelector, [?CLUSTER_VIEW]
            ),
            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"cluster/members_summary">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                % Users can be fetched only before actual assert as api test framework creates
                % users on demand (it is not possible to fetch users once before test starts).
                % Additionally, it creates 2 users: one added to the cluster and other not
                % (see client spec - member vs user)
                ExpUsersCount = floor(length(ozw_test_rpc:list_users()) / 2),

                ExpMembersSummary = #{
                    <<"usersCount">> => ExpUsersCount,
                    <<"groupsCount">> => 0,
                    <<"effectiveUsersCount">> => ExpUsersCount,
                    <<"effectiveGroupsCount">> => 0
                },
                ?assertEqual(ExpMembersSummary, RespBody)
            end)
        }
    ])).


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
            data_spec = #data_spec{
                bad_values = [
                    {id, <<"inexistentClusterId">>, ?ERROR_NOT_FOUND}
                ]
            },
            prepare_args_fun = fun(#api_test_ctx{data = TestData}) ->
                Id = maps:get(id, TestData, ClusterId),
                #rest_args{
                    method = get,
                    path = str_utils:format_bin("user/clusters/~ts", [Id])
                }
            end,
            validate_result_fun = build_validate_cluster_details_fun(ExpClusterDetails)
        }
    ])).


get_zone_cluster_public_configuration_test(_Config) ->
    ExpConfig = #{
        <<"clusterId">> => <<"onezone">>,
        <<"serviceType">> => str_utils:to_binary(?ONEZONE),
        <<"zoneDomain">> => oct_background:get_zone_domain(),
        <<"zoneName">> => <<"dev-onezone">>,
        <<"build">> => get_build_version(zone),
        <<"version">> => get_release_version(zone),
        <<"deployed">> => true
    },
    get_cluster_public_configuration_test_base(zone, ExpConfig).


get_krakow_cluster_public_configuration_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),

    ExpConfig = #{
        <<"serviceType">> => str_utils:to_binary(?ONEPROVIDER),
        <<"providerName">> => oct_background:get_provider_name(krakow),
        <<"providerDomain">> => oct_background:get_provider_domain(krakow),
        <<"providerId">> => ProviderId,
        <<"clusterId">> => ProviderId,
        <<"zoneDomain">> => oct_background:get_zone_domain(),
        <<"build">> => get_build_version(krakow),
        <<"version">> => get_release_version(krakow),
        <<"deployed">> => true,
        <<"isRegistered">> => true
    },
    get_cluster_public_configuration_test_base(krakow, ExpConfig).


%% @private
-spec get_cluster_public_configuration_test_base(oct_background:entity_selector(), json_utils:json_map()) ->
    boolean().
get_cluster_public_configuration_test_base(PanelEntitySelector, ExpConfig) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster public configuration using /configuration REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = api_test_utils:build_all_valid_clients_allowed_client_spec(PanelEntitySelector),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"configuration">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpConfig, RespBody)
            end)
        }
    ])).


get_zone_cluster_cookie_test(_Config) ->
    get_cluster_cookie_test_base(zone).


get_krakow_cluster_cookie_test(_Config) ->
    get_cluster_cookie_test_base(krakow).


%% @private
-spec get_cluster_cookie_test_base(oct_background:entity_selector()) ->
    boolean().
get_cluster_cookie_test_base(PanelEntitySelector) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster cookie using /cookie REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = #client_spec{
                correct = [peer, member, root],
                unauthorized = [guest | api_test_utils:build_unauthorized_clients(PanelEntitySelector)],
                forbidden = []
            },

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"cookie">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(<<"cluster_node">>, RespBody)
            end)
        }
    ])).


get_zone_cluster_progress_test(_Config) ->
    ExpProgress = lists:foldl(fun(Marker, Acc) ->
        IsSet = ?RAND_BOOL(),
        update_progress_marker(zone, Marker, IsSet),
        Acc#{Marker => IsSet}
    end, #{}, ?ZONE_PROGRESS_MARKERS),

    get_cluster_progress_test_base(zone, ExpProgress).


get_krakow_cluster_progress_test(_Config) ->
    ExpProgress = lists:foldl(fun(Marker, Acc) ->
        IsSet = ?RAND_BOOL(),
        update_progress_marker(krakow, Marker, IsSet),
        Acc#{Marker => IsSet}
    end, #{<<"isRegistered">> => true}, ?PROVIDER_PROGRESS_MARKERS),

    get_cluster_progress_test_base(krakow, ExpProgress).


%% @private
-spec get_cluster_progress_test_base(oct_background:entity_selector(), json_utils:json_map()) ->
    boolean().
get_cluster_progress_test_base(PanelEntitySelector, ExpProgress) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current cluster progress using /progress REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(PanelEntitySelector),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"progress">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpProgress, RespBody)
            end)
        }
    ])).


update_zone_cluster_progress_test(_Config) ->
    update_cluster_progress_test_base(zone).


update_krakow_cluster_progress_test(_Config) ->
    update_cluster_progress_test_base(krakow).


%% @private
-spec update_cluster_progress_test_base(oct_background:entity_selector()) ->
    boolean().
update_cluster_progress_test_base(PanelEntitySelector) ->
    MemRef = api_test_memory:init(),
    api_test_memory:set(MemRef, markers_state, get_markers_state(PanelEntitySelector)),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Update current cluster progress using /progress REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                PanelEntitySelector, [?CLUSTER_UPDATE]
            ),
            data_spec = #data_spec{
                optional = [<<"clusterIps">>, <<"webCertificate">>, <<"dnsCheck">>],
                correct_values = #{
                    <<"clusterIps">> => [true, false],
                    <<"webCertificate">> => [true, false],
                    <<"dnsCheck">> => [true, false]
                },
                bad_values = [
                    {<<"clusterIps">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"clusterIps">>)},
                    {<<"webCertificate">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"webCertificate">>)},
                    {<<"dnsCheck">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"dnsCheck">>)}
                ]
            },
            prepare_args_fun = fun(#api_test_ctx{data = TestData}) ->
                #rest_args{
                    method = patch,
                    path = <<"progress">>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(TestData#{
                        % Below markers cannot be changed - no matter what is send it will be ignored
                        <<"clusterNodes">> => ?RAND_BOOL(),
                        <<"storagesSetup">> => ?RAND_BOOL(),
                        <<"isRegistered">> => ?RAND_BOOL()
                    })
                }
            end,
            validate_result_fun = api_test_validate:http_204_no_content(),

            verify_fun = fun
                (expected_success, #api_test_ctx{data = TestData}) ->
                    ExpMarkersState = maps:merge(
                        api_test_memory:get(MemRef, markers_state),
                        TestData
                    ),
                    ?assertEqual(ExpMarkersState, get_markers_state(PanelEntitySelector)),
                    api_test_memory:set(MemRef, markers_state, ExpMarkersState),

                    true;
                (expected_failure, _) ->
                    ExpMarkersState = api_test_memory:get(MemRef, markers_state),
                    ?assertEqual(ExpMarkersState, get_markers_state(PanelEntitySelector)),
                    true
            end
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


init_per_group(_Group, Config) ->
    Config.


end_per_group(cluster_progress_tests, Config) ->
    % Those tests mess with cluster deployment progress markers - ensure after tests all markers are set
    lists:foreach(fun(Marker) ->
        update_progress_marker(zone, Marker, true)
    end, ?ZONE_PROGRESS_MARKERS),

    lists:foreach(fun(Marker) ->
        update_progress_marker(krakow, Marker, true)
    end, ?PROVIDER_PROGRESS_MARKERS),

    Config;

end_per_group(_Group, Config) ->
    Config.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec get_cluster_details(oct_background:entity_selector()) ->
    json_utils:json_map().
get_cluster_details(PanelEntitySelector) ->
    PanelNode = ?RAND_ELEMENT(panel_test_utils:get_panel_nodes(PanelEntitySelector)),
    ServiceId = get_service_id(PanelNode),
    PanelGuiPackagePath = ?rpc(PanelNode, onepanel_env:get(gui_package_path)),

    WorkerNode = ?RAND_ELEMENT(panel_test_utils:get_worker_nodes(PanelEntitySelector)),
    WorkerGuiPackagePath = case PanelEntitySelector of
        zone -> ozw_test_rpc:get_env(ozw_gui_package_path);
        krakow -> opw_test_rpc:get_env(WorkerNode, gui_package_path)
    end,

    #{
        <<"id">> => ServiceId,  %% Cluster id == service id
        <<"type">> => case PanelEntitySelector of
            zone -> <<"onezone">>;
            krakow -> <<"oneprovider">>
        end,
        <<"serviceId">> => ServiceId,
        <<"workerVersion">> => #{
            <<"release">> => get_release_version(WorkerNode),
            <<"build">> => get_build_version(WorkerNode),
            <<"gui">> => calc_gui_hash(WorkerNode, WorkerGuiPackagePath)
        },
        <<"onepanelVersion">> => #{
            <<"release">> => get_release_version(PanelNode),
            <<"build">> => get_build_version(PanelNode),
            <<"gui">> => calc_gui_hash(PanelNode, PanelGuiPackagePath)
        },
        <<"onepanelProxy">> => true
    }.


%% @private
-spec get_service_id(node()) -> binary().
get_service_id(Node) ->
    {ok, ServiceId} = ?rpc(Node, application:get_env(ctool, onedata_service_id)),
    ServiceId.


%% @private
-spec get_release_version(node()) -> binary().
get_release_version(Node) ->
    {ok, ReleaseVersion} = ?rpc(Node, application:get_env(ctool, onedata_service_release_version)),
    ReleaseVersion.


%% @private
-spec get_build_version(node()) -> binary().
get_build_version(Node) ->
    case ?rpc(Node, application:get_env(ctool, onedata_service_build_version)) of
        {ok, <<>>} -> <<"unknown">>;
        {ok, BuildVersion} -> BuildVersion
    end.


%% @private
-spec calc_gui_hash(node(), string()) -> binary().
calc_gui_hash(Node, GuiPackagePath) ->
    {ok, GuiHash} = ?rpc(Node, gui:package_hash(GuiPackagePath)),
    GuiHash.


%% @private
-spec build_validate_cluster_details_fun(json_utils:json_map()) ->
    api_test_runner:validate_call_result_fun().
build_validate_cluster_details_fun(ExpClusterDetails) ->
    api_test_validate:http_200_ok(fun(RespBody) ->
        %% TODO VFS-13055 add creationTime and creator to zone cluster info and test it
        TestedRespBody = maps:without([<<"creationTime">>, <<"creator">>], RespBody),
        ?assertEqual(ExpClusterDetails, TestedRespBody)
    end).


%% @private
-spec update_progress_marker(oct_background:entity_selector(), binary(), boolean()) -> ok.
update_progress_marker(PanelEntitySelector, Marker, true) ->
    ?rpc(PanelEntitySelector, onepanel_deployment:set_marker(get_internal_marker_key(Marker)));
update_progress_marker(PanelEntitySelector, Marker, false) ->
    ?rpc(PanelEntitySelector, onepanel_deployment:unset_marker(get_internal_marker_key(Marker))).


%% @private
-spec get_internal_marker_key(binary()) -> atom().
get_internal_marker_key(Marker) ->
    {_, InternalKey} = lists:keyfind(
        binary_to_atom(Marker),
        1,
        panel_progress_middleware_handler_utils:rest_to_marker_mapping(?ONEPROVIDER)
    ),
    InternalKey.


%% @private
-spec get_markers_state(ct_background:entity_selector()) -> json_utils:json_map().
get_markers_state(PanelEntitySelector) ->
    {ok, _, _, MarkersState} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelEntitySelector, <<"/progress">>, #{auth => root})
    ),
    MarkersState.

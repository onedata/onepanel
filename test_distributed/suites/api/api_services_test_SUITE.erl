%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onepanel services API endpoints (REST).
%%%
%%% NOTE: only general behaviour is checked in this suite (invalid clients, data, etc.) -
%%% concrete responses/side effects are tested in suites:
%%% - cl_resize_op_test_SUITE
%%% - cl_services_op_test_SUITE
%%% - cl_resize_oz_test_SUITE
%%% - cl_services_oz_test_SUITE
%%% TODO VFS-13067 add real deploy service tests e.g. in cl_resize suite
%%% @end
%%%-------------------------------------------------------------------
-module(api_services_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([
    groups/0,
    all/0,

    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_zone_cluster_workers_nagios_status_test/1,
    get_krakow_cluster_workers_nagios_status_test/1,

    get_zone_cluster_managers_statuses_test/1,
    get_zone_databases_statuses_test/1,
    get_zone_cluster_workers_statuses_test/1,
    get_krakow_cluster_managers_statuses_test/1,
    get_krakow_databases_statuses_test/1,
    get_krakow_ones3_statuses_test/1,
    get_krakow_cluster_workers_statuses_test/1,

    get_zone_cluster_manager_status_on_host_test/1,
    get_zone_database_status_on_host_test/1,
    get_zone_cluster_worker_status_on_host_test/1,
    get_krakow_cluster_manager_status_on_host_test/1,
    get_krakow_database_status_on_host_test/1,
    get_krakow_ones3_status_on_host_test/1,
    get_krakow_cluster_worker_status_on_host_test/1,

    start_stop_all_zone_cluster_managers_test/1,
    start_stop_all_zone_databases_test/1,
    start_stop_all_zone_cluster_workers_test/1,
    start_stop_all_krakow_cluster_managers_test/1,
    start_stop_all_krakow_databases_test/1,
    start_stop_all_krakow_ones3_test/1,
    start_stop_all_krakow_cluster_workers_test/1,

    start_stop_zone_cluster_manager_on_host_test/1,
    start_stop_zone_database_on_host_test/1,
    start_stop_zone_cluster_worker_on_host_test/1,
    start_stop_krakow_cluster_manager_on_host_test/1,
    start_stop_krakow_database_on_host_test/1,
    start_stop_krakow_ones3_on_host_test/1,
    start_stop_krakow_cluster_worker_on_host_test/1,

    deploy_zone_cluster_manager_on_host_test/1,
    deploy_zone_database_on_host_test/1,
    deploy_zone_cluster_worker_on_host_test/1,
    deploy_krakow_cluster_manager_on_host_test/1,
    deploy_krakow_database_on_host_test/1,
    deploy_krakow_ones3_on_host_test/1,
    deploy_krakow_cluster_worker_on_host_test/1
]).

groups() -> [
    {get_service_nagios_status_tests, [parallel], [
        get_zone_cluster_workers_nagios_status_test,
        get_krakow_cluster_workers_nagios_status_test
    ]},
    {get_service_status_on_all_hosts_tests, [parallel], [
        get_zone_cluster_managers_statuses_test,
        get_zone_databases_statuses_test,
        get_zone_cluster_workers_statuses_test,
        get_krakow_cluster_managers_statuses_test,
        get_krakow_databases_statuses_test,
        get_krakow_ones3_statuses_test,
        get_krakow_cluster_workers_statuses_test
    ]},
    {get_service_status_on_host_tests, [parallel], [
        get_zone_cluster_manager_status_on_host_test,
        get_zone_database_status_on_host_test,
        get_zone_cluster_worker_status_on_host_test,
        get_krakow_cluster_manager_status_on_host_test,
        get_krakow_database_status_on_host_test,
        get_krakow_ones3_status_on_host_test,
        get_krakow_cluster_worker_status_on_host_test
    ]},
    {start_stop_service_on_all_hosts_tests, [parallel], [
        start_stop_all_zone_cluster_managers_test,
        start_stop_all_zone_databases_test,
        start_stop_all_zone_cluster_workers_test,
        start_stop_all_krakow_cluster_managers_test,
        start_stop_all_krakow_databases_test,
        start_stop_all_krakow_ones3_test,
        start_stop_all_krakow_cluster_workers_test
    ]},
    {start_stop_service_on_host_tests, [parallel], [
        start_stop_zone_cluster_manager_on_host_test,
        start_stop_zone_database_on_host_test,
        start_stop_zone_cluster_worker_on_host_test,
        start_stop_krakow_cluster_manager_on_host_test,
        start_stop_krakow_database_on_host_test,
        start_stop_krakow_ones3_on_host_test,
        start_stop_krakow_cluster_worker_on_host_test
    ]},
    {deploy_service_on_host_tests, [parallel], [
        deploy_zone_cluster_manager_on_host_test,
        deploy_zone_database_on_host_test,
        deploy_zone_cluster_worker_on_host_test,
        deploy_krakow_cluster_manager_on_host_test,
        deploy_krakow_database_on_host_test,
        deploy_krakow_ones3_on_host_test,
        deploy_krakow_cluster_worker_on_host_test
    ]}
].

all() -> [
    {group, get_service_nagios_status_tests},
    {group, get_service_status_on_all_hosts_tests},
    {group, get_service_status_on_host_tests},
    {group, start_stop_service_on_all_hosts_tests},
    {group, start_stop_service_on_host_tests},
    {group, deploy_service_on_host_tests}
].


-define(ZONE_MANAGERS_PATH, <<"zone/managers">>).
-define(ZONE_DATABASES_PATH, <<"zone/databases">>).
-define(ZONE_WORKERS_PATH, <<"zone/workers">>).

-define(PROVIDER_MANAGERS_PATH, <<"provider/managers">>).
-define(PROVIDER_DATABASES_PATH, <<"provider/databases">>).
-define(PROVIDER_ONES3_PATH, <<"provider/ones3">>).
-define(PROVIDER_WORKERS_PATH, <<"provider/workers">>).

-define(CREATE_SERVICE_HANDLER_MODULES, [
    service_cluster_manager_instances_create_middleware_handler:module_info(module),
    service_couchbase_instances_create_middleware_handler:module_info(module),
    service_ones3_instances_create_middleware_handler:module_info(module),
    service_op_worker_instances_create_middleware_handler:module_info(module),
    service_oz_worker_instances_create_middleware_handler:module_info(module)
]).

-define(UPDATE_SERVICE_HANDLER_MODULES, [
    service_cluster_manager_start_stop_all_update_middleware_handler:module_info(module),
    service_cluster_manager_start_stop_update_middleware_handler:module_info(module),
    service_couchbase_start_stop_all_update_middleware_handler:module_info(module),
    service_couchbase_start_stop_update_middleware_handler:module_info(module),
    service_ones3_start_stop_all_update_middleware_handler:module_info(module),
    service_ones3_start_stop_update_middleware_handler:module_info(module),
    service_op_worker_start_stop_all_update_middleware_handler:module_info(module),
    service_op_worker_start_stop_update_middleware_handler:module_info(module),
    service_oz_worker_start_stop_all_update_middleware_handler:module_info(module),
    service_oz_worker_start_stop_update_middleware_handler:module_info(module)
]).


%%%===================================================================
%%% API
%%%===================================================================


get_zone_cluster_workers_nagios_status_test(_Config) ->
    get_cluster_worker_nagios_status_test_base(zone, "oz_worker", <<"zone/nagios">>).


get_krakow_cluster_workers_nagios_status_test(_Config) ->
    get_cluster_worker_nagios_status_test_base(krakow, "op_worker", <<"provider/nagios">>).


%% @private
-spec get_cluster_worker_nagios_status_test_base(oct_background:entity_selector(), list(), binary()) ->
    boolean().
get_cluster_worker_nagios_status_test_base(TargetEntitySelector, WorkerPrefix, RestPath) ->
    Hosts = lists:map(fun(Host) ->
        WorkerPrefix ++ "@" ++ binary_to_list(Host)
    end,cluster_management_test_utils:get_all_hosts(TargetEntitySelector)),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Get service nagios report using /~ts rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector
            ),
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = RestPath} end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                {XmlReport, []} = xmerl_scan:string(binary_to_list(RespBody)),

                ?assertMatch(
                    #xmlElement{name = healthdata, attributes = [_, #xmlAttribute{name = status, value = "ok"}]},
                    XmlReport
                ),
                lists:foreach(fun({Host, XmlHostReport}) ->
                    ?assertMatch(
                        [#xmlAttribute{name = name, value = Host}, #xmlAttribute{name = status, value = "ok"}],
                        XmlHostReport#xmlElement.attributes
                    )
                end, lists:zip(Hosts, XmlReport#xmlElement.content))
            end)
        }
    ])).


get_zone_cluster_managers_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(zone, ?ZONE_MANAGERS_PATH).


get_zone_databases_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(zone, ?ZONE_DATABASES_PATH).


get_zone_cluster_workers_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(zone, ?ZONE_WORKERS_PATH).


get_krakow_cluster_managers_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(krakow, ?PROVIDER_MANAGERS_PATH).


get_krakow_databases_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(krakow, ?PROVIDER_DATABASES_PATH).


get_krakow_ones3_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(krakow, ?PROVIDER_ONES3_PATH).


get_krakow_cluster_workers_statuses_test(_Config) ->
    get_service_status_on_all_hosts_test_base(krakow, ?PROVIDER_WORKERS_PATH).


%% @private
-spec get_service_status_on_all_hosts_test_base(oct_background:entity_selector(), binary()) ->
    boolean().
get_service_status_on_all_hosts_test_base(TargetEntitySelector, RestPath) ->
    ExpStatuses = lists:foldl(fun(Host, Acc) ->
        Acc#{Host => <<"healthy">>}
    end, #{}, cluster_management_test_utils:get_all_hosts(TargetEntitySelector)),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Get service statuses on all hosts using /~ts rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector
            ),
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = RestPath} end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpStatuses, RespBody)
            end)
        }
    ])).


get_zone_cluster_manager_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(zone, ?ZONE_MANAGERS_PATH).


get_zone_database_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(zone, ?ZONE_DATABASES_PATH).


get_zone_cluster_worker_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(zone, ?ZONE_WORKERS_PATH).


get_krakow_cluster_manager_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(krakow, ?PROVIDER_MANAGERS_PATH).


get_krakow_database_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(krakow, ?PROVIDER_DATABASES_PATH).


get_krakow_ones3_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(krakow, ?PROVIDER_ONES3_PATH).


get_krakow_cluster_worker_status_on_host_test(_Config) ->
    get_service_status_on_host_test_base(krakow, ?PROVIDER_WORKERS_PATH).


%% @private
-spec get_service_status_on_host_test_base(oct_background:entity_selector(), binary()) ->
    boolean().
get_service_status_on_host_test_base(TargetEntitySelector, RestPath) ->
    Hosts = cluster_management_test_utils:get_all_hosts(TargetEntitySelector),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Get service status on host using /~ts/{host} rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector
            ),
            data_spec = #data_spec{
                bad_values = [
                    {host, <<"xyz">>, ?ERROR_NOT_FOUND}
                ]
            },
            prepare_args_fun = fun(#api_test_ctx{data = TestData}) ->
                TargetHost = maps:get(host, TestData, ?RAND_ELEMENT(Hosts)),
                #rest_args{
                    method = get,
                    path = str_utils:format_bin("~ts/~ts", [RestPath, TargetHost])
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(<<"healthy">>, RespBody)
            end)
        }
    ])).


start_stop_all_zone_cluster_managers_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(zone, ?ZONE_MANAGERS_PATH).


start_stop_all_zone_databases_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(zone, ?ZONE_DATABASES_PATH).


start_stop_all_zone_cluster_workers_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(zone, ?ZONE_WORKERS_PATH).


start_stop_all_krakow_cluster_managers_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(krakow, ?PROVIDER_MANAGERS_PATH).


start_stop_all_krakow_databases_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(krakow, ?PROVIDER_DATABASES_PATH).


start_stop_all_krakow_ones3_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(krakow, ?PROVIDER_ONES3_PATH).


start_stop_all_krakow_cluster_workers_test(_Config) ->
    start_stop_service_on_all_hosts_test_base(krakow, ?PROVIDER_WORKERS_PATH).


%% @private
-spec start_stop_service_on_all_hosts_test_base(oct_background:entity_selector(), binary()) ->
    boolean().
start_stop_service_on_all_hosts_test_base(TargetEntitySelector, RestPath) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Start/stop service on all hosts using /~ts rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector, [?CLUSTER_UPDATE]
            ),
            data_spec = #data_spec{
                optional = [
                    <<"started">>
                ],
                correct_values = #{
                    <<"started">> => [true, false]
                },
                bad_values = [
                    {<<"started">>, <<"valueNotAllowed">>, ?ERR_BAD_VALUE_BOOLEAN(<<"started">>)}
                ]
            },
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                #rest_args{
                    method = patch,
                    path = RestPath,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(Data)
                }
            end,
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


start_stop_zone_cluster_manager_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(zone, ?ZONE_MANAGERS_PATH).


start_stop_zone_database_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(zone, ?ZONE_DATABASES_PATH).


start_stop_zone_cluster_worker_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(zone, ?ZONE_WORKERS_PATH).


start_stop_krakow_cluster_manager_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(krakow, ?PROVIDER_MANAGERS_PATH).


start_stop_krakow_database_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(krakow, ?PROVIDER_DATABASES_PATH).


start_stop_krakow_ones3_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(krakow, ?PROVIDER_ONES3_PATH).


start_stop_krakow_cluster_worker_on_host_test(_Config) ->
    start_stop_service_on_host_test_base(krakow, ?PROVIDER_WORKERS_PATH).


%% @private
-spec start_stop_service_on_host_test_base(oct_background:entity_selector(), binary()) ->
    boolean().
start_stop_service_on_host_test_base(TargetEntitySelector, RestPath) ->
    Hosts = cluster_management_test_utils:get_all_hosts(TargetEntitySelector),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Start/stop service on host using /~ts/{host} rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector, [?CLUSTER_UPDATE]
            ),
            data_spec = #data_spec{
                optional = [
                    <<"started">>
                ],
                correct_values = #{
                    <<"started">> => [true, false]
                },
                bad_values = [
                    {<<"started">>, <<"valueNotAllowed">>, ?ERR_BAD_VALUE_BOOLEAN(<<"started">>)},
                    {host, <<"xyz">>, ?ERROR_NOT_FOUND}
                ]
            },
            prepare_args_fun = fun(#api_test_ctx{data = TestData}) ->
                TargetHost = maps:get(host, TestData, ?RAND_ELEMENT(Hosts)),
                #rest_args{
                    method = patch,
                    path = str_utils:format_bin("~ts/~ts", [RestPath, TargetHost]),
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(TestData)
                }
            end,
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


deploy_zone_cluster_manager_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        zone, ?ZONE_MANAGERS_PATH, build_deploy_cluster_manager_data_spec(zone)
    ).


deploy_zone_database_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        zone, ?ZONE_DATABASES_PATH, build_deploy_database_data_spec(zone)
    ).


deploy_zone_cluster_worker_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        zone, ?ZONE_WORKERS_PATH, build_deploy_cluster_worker_data_spec(zone)
    ).


deploy_krakow_cluster_manager_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        krakow, ?PROVIDER_MANAGERS_PATH, build_deploy_cluster_manager_data_spec(krakow)
    ).


deploy_krakow_database_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        krakow, ?PROVIDER_DATABASES_PATH, build_deploy_database_data_spec(krakow)
    ).


deploy_krakow_ones3_on_host_test(_Config) ->
    deploy_service_on_host_test_base(krakow, ?PROVIDER_ONES3_PATH, #data_spec{
        required = [<<"hosts">>],
        optional = [<<"port">>],
        correct_values = #{
            % It is enough to submit dummy host as Handler:process is mocked so no service
            % is really deployed
            <<"hosts">> => [[<<"dummyHost">>]],
            <<"port">> => [7777]
        },
        bad_values = [
            {<<"hosts">>, cluster_management_test_utils:get_all_hosts(krakow), ?ERROR_ALREADY_EXISTS},
            {<<"port">>, <<"notInt">>, ?ERR_BAD_VALUE_INTEGER(<<"port">>)}
        ]
    }).


deploy_krakow_cluster_worker_on_host_test(_Config) ->
    deploy_service_on_host_test_base(
        krakow, ?PROVIDER_WORKERS_PATH, build_deploy_cluster_worker_data_spec(krakow)
    ).


%% @private
-spec deploy_service_on_host_test_base(oct_background:entity_selector(), binary(), api_test_runner:data_spec()) ->
    boolean().
deploy_service_on_host_test_base(TargetEntitySelector, RestPath, DataSpec) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = str_utils:format_bin("Deploy service on hosts using /~ts rest endpoint", [RestPath]),
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector, [?CLUSTER_UPDATE]
            ),
            data_spec = DataSpec,
            prepare_args_fun = fun(#api_test_ctx{data = TestData}) ->
                #rest_args{
                    method = post,
                    path = RestPath,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(TestData)
                }
            end,
            validate_result_fun = api_test_validate:http_202_task_started()
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ModulesToLoad = [?MODULE, ip_test_utils],
    oct_background:init_per_suite([{?LOAD_MODULES, ModulesToLoad} | Config], #onenv_test_config{
        onenv_scenario = "1op_2nodes_2ones3",
        posthook = fun(NewConfig) ->
            mock_handlers(),
            NewConfig
        end
    }).


end_per_suite(_Config) ->
    test_utils:mock_unload(lists:flatten([
        oct_background:get_zone_panels(),
        oct_background:get_provider_panels(krakow)
    ])),
    oct_background:end_per_suite().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec mock_handlers() -> ok.
mock_handlers() ->
    PanelNodes = lists:flatten([
        oct_background:get_zone_panels(),
        oct_background:get_provider_panels(krakow)
    ]),

    ?assertEqual(ok, test_utils:mock_new(
        PanelNodes, ?CREATE_SERVICE_HANDLER_MODULES, [passthrough])
    ),
    lists:foreach(fun(Module) ->
        test_utils:mock_expect(
            PanelNodes, Module, process, fun(_) -> {ok, ?RAND_STR()} end
        )
    end, ?CREATE_SERVICE_HANDLER_MODULES),

    ?assertEqual(ok, test_utils:mock_new(
        PanelNodes, ?UPDATE_SERVICE_HANDLER_MODULES, [passthrough])
    ),
    lists:foreach(fun(Module) ->
        test_utils:mock_expect(
            PanelNodes, Module, process, fun(_) -> ok end
        )
    end, ?UPDATE_SERVICE_HANDLER_MODULES),

    ok.


%% @private
-spec build_deploy_cluster_manager_data_spec(oct_background:entity_selector()) ->
    api_test_runner:data_spec().
build_deploy_cluster_manager_data_spec(PanelSelector) ->
    #data_spec{
        required = [<<"hosts">>, <<"mainHost">>],
        correct_values = #{
            % It is enough to submit dummy host as Handler:process is mocked so no service
            % is really deployed
            <<"hosts">> => [[<<"dummyHost">>]],
            <<"mainHost">> => [<<"dummyHost">>]
        },
        bad_values = [
            {<<"hosts">>, cluster_management_test_utils:get_all_hosts(PanelSelector), ?ERROR_ALREADY_EXISTS},
            {<<"mainHost">>, 12, ?ERR_BAD_VALUE_STRING(<<"mainHost">>)}
        ]
    }.


%% @private
-spec build_deploy_database_data_spec(oct_background:entity_selector()) ->
    api_test_runner:data_spec().
build_deploy_database_data_spec(PanelSelector) ->
    #data_spec{
        required = [<<"hosts">>],
        optional = [<<"serverQuota">>, <<"bucketQuota">>],
        correct_values = #{
            % It is enough to submit dummy host as Handler:process is mocked so no service
            % is really deployed
            <<"hosts">> => [[<<"dummyHost">>]],
            <<"serverQuota">> => [100000],
            <<"bucketQuota">> => [4000]
        },
        bad_values = [
            {<<"hosts">>, cluster_management_test_utils:get_all_hosts(PanelSelector), ?ERROR_ALREADY_EXISTS},
            {<<"serverQuota">>, <<"notInt">>, ?ERR_BAD_VALUE_INTEGER(<<"serverQuota">>)},
            {<<"bucketQuota">>, <<"notInt">>, ?ERR_BAD_VALUE_INTEGER(<<"bucketQuota">>)}
        ]
    }.


%% @private
-spec build_deploy_cluster_worker_data_spec(oct_background:entity_selector()) ->
    api_test_runner:data_spec().
build_deploy_cluster_worker_data_spec(PanelSelector) ->
    #data_spec{
        required = [<<"hosts">>],
        correct_values = #{
            % It is enough to submit dummy host as Handler:process is mocked so no service
            % is really deployed
            <<"hosts">> => [[<<"dummyHost">>]]
        },
        bad_values = [
            {<<"hosts">>, cluster_management_test_utils:get_all_hosts(PanelSelector), ?ERROR_ALREADY_EXISTS}
        ]
    }.

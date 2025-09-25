%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onepanel dns and ips API endpoints (REST).
%%%
%%% NOTE: only general behaviour is checked in this suite (invalid clients, data, etc.) -
%%% concrete responses/side effects are tested in suites:
%%% - dns_op_test_SUITE
%%% - dns_oz_test_SUITE
%%% @end
%%%-------------------------------------------------------------------
-module(api_dns_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([
    groups/0,
    all/0,

    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_zone_dns_check_configuration_test/1,
    get_krakow_dns_check_configuration_test/1,

    update_zone_dns_check_configuration_test/1,
    update_krakow_dns_check_configuration_test/1,

    perform_zone_dns_check_test/1,
    perform_krakow_dns_check_test/1,

    get_zone_cluster_ips_test/1,
    get_krakow_cluster_ips_test/1,

    update_zone_cluster_ips_test/1,
    update_krakow_cluster_ips_test/1
]).

groups() -> [
    {all_tests, [parallel], [
        get_zone_dns_check_configuration_test,
        get_krakow_dns_check_configuration_test,

        update_zone_dns_check_configuration_test,
        update_krakow_dns_check_configuration_test,

        perform_zone_dns_check_test,
        perform_krakow_dns_check_test,

        get_zone_cluster_ips_test,
        get_krakow_cluster_ips_test,

        update_zone_cluster_ips_test,
        update_krakow_cluster_ips_test
    ]}
].

all() -> [
    {group, all_tests}
].


%%%===================================================================
%%% API
%%%===================================================================


get_zone_dns_check_configuration_test(_Config) ->
    get_dns_check_configuration_test_base(zone).


get_krakow_dns_check_configuration_test(_Config) ->
    get_dns_check_configuration_test_base(krakow).


%% @private
-spec get_dns_check_configuration_test_base(oct_background:entity_selector()) ->
    boolean().
get_dns_check_configuration_test_base(TargetEntitySelector) ->
    % Configuration values are tested in dns_(op|oz)_test_SUITE
    ExpFields = lists:sort([<<"dnsServers">>, <<"builtInDnsServer">>, <<"dnsCheckAcknowledged">>]),

    get_test_base(
        TargetEntitySelector,
        <<"Get dns check configuration using /dns_check/configuration rest endpoint">>,
        <<"dns_check/configuration">>,
        ExpFields
    ).


update_zone_dns_check_configuration_test(_Config) ->
    DataSpec = #data_spec{
        optional = [
            <<"dnsServers">>,
            <<"builtInDnsServer">>,  %% field only available in oz
            <<"dnsCheckAcknowledged">>
        ],
        correct_values = #{
            <<"dnsServers">> => [[<<"8.8.8.8">>], [<<"8.8.4.4">>]],
            <<"builtInDnsServer">> => [true, false],
            <<"dnsCheckAcknowledged">> => [true, false]
        },
        bad_values = [
            {<<"dnsServers">>, <<"valueNotAllowed">>, ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"dnsServers">>)},
            {<<"dnsServers">>, [<<"valueNotAllowed">>], ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"dnsServers">>)},
            {<<"builtInDnsServer">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"builtInDnsServer">>)},
            {<<"dnsCheckAcknowledged">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"dnsCheckAcknowledged">>)}
        ]
    },
    update_dns_check_configuration_test_base(zone, DataSpec).


update_krakow_dns_check_configuration_test(_Config) ->
    DataSpec = #data_spec{
        optional = [
            <<"dnsServers">>,
            <<"dnsCheckAcknowledged">>
        ],
        correct_values = #{
            <<"dnsServers">> => [[<<"8.8.8.8">>], [<<"8.8.4.4">>]],
            <<"dnsCheckAcknowledged">> => [true, false]
        },
        bad_values = [
            {<<"dnsServers">>, <<"valueNotAllowed">>, ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"dnsServers">>)},
            {<<"dnsServers">>, [<<"valueNotAllowed">>], ?ERR_BAD_VALUE_LIST_OF_IPV4_ADDRESSES(<<"dnsServers">>)},
            {<<"dnsCheckAcknowledged">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"dnsCheckAcknowledged">>)}
        ]
    },
    update_dns_check_configuration_test_base(krakow, DataSpec).


%% @private
-spec update_dns_check_configuration_test_base(oct_background:entity_selector(), api_test_runner:data_spec()) ->
    boolean().
update_dns_check_configuration_test_base(TargetEntitySelector, DataSpec) ->
    RestPath = <<"dns_check/configuration">>,

    update_test_base(
        TargetEntitySelector,
        str_utils:format_bin("Update dns check configuration using /~ts rest endpoint", [RestPath]),
        RestPath,
        DataSpec
    ).


perform_zone_dns_check_test(_Config) ->
    perform_dns_check_test_base(zone).


perform_krakow_dns_check_test(_Config) ->
    perform_dns_check_test_base(krakow).


%% @private
-spec perform_dns_check_test_base(oct_background:entity_selector()) ->
    boolean().
perform_dns_check_test_base(TargetEntitySelector) ->
    get_test_base(
        TargetEntitySelector,
        <<"Perform dns check using /dns_check rest endpoint">>,
        <<"dns_check">>,
        % Beside timestamp and domain there may be additional fields returned depending on
        % cluster type and configuration (e.g. oneS3Subdomain) - check results are tested
        % in dns_(op|oz)_test_SUITE
        skip
    ).


get_zone_cluster_ips_test(_Config) ->
    get_cluster_ips_test_base(?OZ_PANEL, zone).


get_krakow_cluster_ips_test(_Config) ->
    get_cluster_ips_test_base(?OP_PANEL, krakow).


%% @private
-spec get_cluster_ips_test_base(atom(), oct_background:entity_selector()) ->
    boolean().
get_cluster_ips_test_base(TargetPanelType, TargetEntitySelector) ->
    RestPath = case TargetPanelType of
        ?OZ_PANEL -> <<"zone/cluster_ips">>;
        ?OP_PANEL -> <<"provider/cluster_ips">>
    end,

    % Values are tested in dns_(op|oz)_test_SUITE
    ExpFields = lists:sort([<<"isConfigured">>, <<"hosts">>]),

    get_test_base(
        TargetEntitySelector,
        str_utils:format_bin("Get cluster ips using /~ts rest endpoint", [RestPath]),
        RestPath,
        ExpFields
    ).


update_zone_cluster_ips_test(_Config) ->
    update_cluster_ips_test_base(?OZ_PANEL, zone).


update_krakow_cluster_ips_test(_Config) ->
    update_cluster_ips_test_base(?OP_PANEL, krakow).


%% @private
-spec update_cluster_ips_test_base(atom(), oct_background:entity_selector()) ->
    boolean().
update_cluster_ips_test_base(TargetPanelType, TargetEntitySelector) ->
    RestPath = case TargetPanelType of
        ?OZ_PANEL -> <<"zone/cluster_ips">>;
        ?OP_PANEL -> <<"provider/cluster_ips">>
    end,

    DataSpec = #data_spec{
        required = [<<"hosts">>],
        correct_values = #{
            % Providing valid addresses and verifying changes is done in dns_(op|oz)_test_SUITE
            <<"hosts">> => [#{}]
        },
        bad_values = [
            {<<"hosts">>, <<"valueNotAllowed">>, ?ERR_MISSING_REQUIRED_VALUE(<<"hosts._">>)},
            {<<"hosts">>, #{<<"valueNotAllowed">> => 5}, ?ERR_BAD_VALUE_STRING(<<"hosts.valueNotAllowed">>)}
        ]
    },

    update_test_base(
        TargetEntitySelector,
        str_utils:format_bin("Update cluster ips using /~ts rest endpoint", [RestPath]),
        RestPath,
        DataSpec
    ).


%% @private
-spec get_test_base(
    oct_background:entity_selector(),
    binary(),
    binary(),
    skip | [binary()]
) ->
    boolean().
get_test_base(TargetEntitySelector, Description, Path, ExpFields) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = Description,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector
            ),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = Path
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                case ExpFields of
                    skip -> ok;
                    _ -> ?assertEqual(ExpFields, lists:sort(maps:keys(RespBody)))
                end
            end)
        }
    ])).


%% @private
-spec update_test_base(oct_background:entity_selector(), binary(), binary(), api_test_runner:data_spec()) ->
    boolean().
update_test_base(TargetEntitySelector, Description, RestPath, DataSpec) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = Description,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(TargetEntitySelector),
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                TargetEntitySelector, [?CLUSTER_UPDATE]
            ),
            data_spec = DataSpec,
            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                #rest_args{
                    method = patch,
                    path = RestPath,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(Data)
                }
            end,
            % Verifying if changes are persisted is done in dns_(op|oz)_test_SUITE
            validate_result_fun = api_test_validate:http_204_no_content()
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

%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onezone service configuration API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oz_service_configuration_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include("onepanel_test_utils.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_policies_test/1,
    set_policies_test/1,

    get_gui_message_settings_test/1,
    update_gui_message_settings_test/1
]).

all() -> [
    get_policies_test,
    set_policies_test,

    get_gui_message_settings_test,
    update_gui_message_settings_test
].

-define(MESSAGE_IDS, [
    <<"cookie_consent_notification">>,
    <<"privacy_policy">>,
    <<"terms_of_use">>,
    <<"signin_notification">>
]).


%%%===================================================================
%%% API
%%%===================================================================


get_policies_test(_Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),
    ExpectedPolicies = get_policies_with_rpc(),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get Onezone policies using /zone/policies endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(zone),

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"zone/policies">>
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ?assertEqual(ExpectedPolicies, Body)
            end)

        }
    ])).


set_policies_test(_Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Set Onezone policies using /zone/policies endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                zone, [?CLUSTER_UPDATE]
            ),
            data_spec = build_modify_policies_data_spec(),

            prepare_args_fun = build_modify_policies_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_modify_policies_verify_fun()
        }
    ])).


%% @private
-spec build_modify_policies_data_spec() -> api_test_runner:data_spec().
build_modify_policies_data_spec() ->
    #data_spec{
        optional = [
            <<"oneproviderRegistration">>,
            <<"subdomainDelegation">>,
            <<"guiPackageVerification">>,
            <<"harvesterGuiPackageVerification">>
        ],
        correct_values = #{
            <<"oneproviderRegistration">> => [<<"open">>, <<"restricted">>],
            <<"subdomainDelegation">> => [true, false],
            <<"guiPackageVerification">> => [true, false],
            <<"harvesterGuiPackageVerification">> => [true, false]
        },
        bad_values = [
            {<<"oneproviderRegistration">>, <<"valueNotAllowed">>, ?ERR_BAD_VALUE_NOT_ALLOWED(<<"oneproviderRegistration">>, [<<"open">>, <<"restricted">>])},
            {<<"oneproviderRegistration">>, value_not_allowed, ?ERR_BAD_VALUE_NOT_ALLOWED(<<"oneproviderRegistration">>, [<<"open">>, <<"restricted">>])},
            {<<"subdomainDelegation">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
            {<<"guiPackageVerification">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"guiPackageVerification">>)},
            {<<"harvesterGuiPackageVerification">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"harvesterGuiPackageVerification">>)}
        ]
    }.


%% @private
-spec build_modify_policies_prepare_args_fun() -> api_test_runner:prepare_args_fun().
build_modify_policies_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data}) ->
        #rest_args{
            method = patch,
            path = <<"zone/policies">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(Data)
        }
    end.


%% @private
-spec build_modify_policies_verify_fun() -> api_test_runner:verify_fun().
build_modify_policies_verify_fun() ->
    fun
        (expected_success, #api_test_ctx{data = Data}) ->
            OnezonePolicies = get_policies_with_rpc(),

            ?assert(maps_utils:is_submap(Data, OnezonePolicies)),
            true;
        (expected_failure, _) ->
            true
    end.


get_gui_message_settings_test(_Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),
    ExpSettings = rand_gui_message_settings(),
    set_gui_message_settings(ExpSettings),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get Onezone GUI message setting using /zone/gui_messages/{id} endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(zone),
            data_spec = build_get_gui_message_settings_data_spec(),

            prepare_args_fun = build_gui_message_settings_prepare_args_fun(get),
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ?assertEqual(ExpSettings, Body)
            end)
        }
    ])).


%% @private
-spec build_get_gui_message_settings_data_spec() -> api_test_runner:data_spec().
build_get_gui_message_settings_data_spec() ->
    #data_spec{
        required = [id],
        correct_values = #{id => ?MESSAGE_IDS},
        bad_values = [
            {id, <<"valueNotAllowed">>, ?ERROR_NOT_FOUND}
        ]
    }.


update_gui_message_settings_test(_Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),
    InitialSettings = rand_gui_message_settings(),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Update Onezone GUI message setting using /zone/gui_messages/{id} endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = api_test_utils:build_member_and_root_allowed_client_spec(
                zone, [?CLUSTER_UPDATE]
            ),
            data_spec = build_update_gui_message_settings_data_spec(),

            setup_fun = fun() -> set_gui_message_settings(InitialSettings) end,
            prepare_args_fun = build_gui_message_settings_prepare_args_fun(patch),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_update_gui_message_settings_verify_fun(InitialSettings)
        }
    ])).


%% @private
-spec build_update_gui_message_settings_data_spec() -> api_test_runner:data_spec().
build_update_gui_message_settings_data_spec() ->
    #data_spec{
        required = [id],
        optional = [
            <<"enabled">>,
            <<"body">>
        ],
        correct_values = #{
            id => ?MESSAGE_IDS,
            <<"enabled">> => [true, false],
            <<"body">> => [?RAND_STR()]
        },
        bad_values = [
            {id, <<"valueNotAllowed">>, ?ERROR_NOT_FOUND},
            {<<"enabled">>, not_a_boolean, ?ERR_BAD_VALUE_BOOLEAN(<<"enabled">>)},
            {<<"body">>, 1, ?ERR_BAD_VALUE_STRING(<<"body">>)}
        ]
    }.


%% @private
-spec build_update_gui_message_settings_verify_fun(json_utils:json_map()) ->
    api_test_runner:verify_fun().
build_update_gui_message_settings_verify_fun(InitialSettings) ->
    fun
        (expected_success, #api_test_ctx{data = TestData = #{id := MessageId}}) ->
            ExpSettings = maps:merge(InitialSettings, maps:without([id], TestData)),
            Settings = get_gui_message_settings(MessageId),

            ?assertEqual(ExpSettings, Settings),
            true;
        (expected_failure, _) ->
            lists:foreach(fun(MessageId) ->
                Settings = get_gui_message_settings(MessageId),
                ?assertEqual(InitialSettings, Settings)
            end, ?MESSAGE_IDS),

            true
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec get_policies_with_rpc() -> map().
get_policies_with_rpc() ->
    OneproviderRegistration = ozw_test_rpc:get_env(provider_registration_policy),
    GuiPackageVerification = ozw_test_rpc:get_env(gui_package_verification),
    HarvesterGuiPackageVerification = ozw_test_rpc:get_env(harvester_gui_package_verification),
    SubdomainDelegationSupported = ozw_test_rpc:get_env(subdomain_delegation_supported),

    #{
        <<"guiPackageVerification">> => GuiPackageVerification,
        <<"harvesterGuiPackageVerification">> => HarvesterGuiPackageVerification,
        <<"oneproviderRegistration">> => atom_to_binary(OneproviderRegistration, utf8),
        <<"subdomainDelegation">> => SubdomainDelegationSupported
    }.


%% @private
-spec rand_gui_message_settings() -> json_utils:json_map().
rand_gui_message_settings() ->
    #{
        <<"enabled">> => ?RAND_BOOL(),
        <<"body">> => ?RAND_STR()
    }.


%% @private
-spec get_gui_message_settings(binary()) -> json_utils:json_map().
get_gui_message_settings(MessageId) ->
    {ok, Settings} = ?assertMatch({ok, _}, ?rpc(zone, oz_worker_rpc:get_gui_message_as_map(MessageId))),
    kv_utils:copy_found([{body, <<"body">>}, {enabled, <<"enabled">>}], Settings).


%% @private
-spec set_gui_message_settings(json_utils:json_map()) -> ok.
set_gui_message_settings(Settings) ->
    lists:foreach(fun(MessageId) ->
        ?assertEqual(
            ok,
            ?rpc(zone, oz_worker_rpc:update_gui_message(?ROOT, MessageId, Settings))
        )
    end, ?MESSAGE_IDS).


%% @private
-spec build_gui_message_settings_prepare_args_fun(get | patch) ->
    api_test_runner:prepare_args_fun().
build_gui_message_settings_prepare_args_fun(Method) ->
    fun(#api_test_ctx{data = TestData}) ->
        case maps:take(id, TestData) of
            {MessageId, Body} ->
                #rest_args{
                    method = Method,
                    path = str_utils:format_bin("zone/gui_messages/~ts", [MessageId]),
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(Body)
                };
            error ->
                % Since 'id' is part of the path it cannot be omitted - it is not
                % possible to test ERR_MISSING_REQUIRED_VALUE
                skip
        end
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

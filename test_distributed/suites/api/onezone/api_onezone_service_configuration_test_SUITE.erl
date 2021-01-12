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
-module(api_onezone_service_configuration_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_onezone_policies_test/1,
    set_onezone_policies_test/1
]).

all() -> [
    get_onezone_policies_test,
    set_onezone_policies_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_onezone_policies_test(Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),
    ExpectedPolicies = get_onezone_policies_with_rpc(Config),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get Onezone policies using /zone/policies endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    member
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

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


set_onezone_policies_test(Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set Onezone policies using /zone/policies endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE, ?CLUSTER_SET_PRIVILEGES]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            data_spec = build_modify_onezone_policies_data_spec(),

            prepare_args_fun = build_modify_onezone_policies_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_modify_onezone_policies_verify_fun(Config)
        }
    ])).


%% @private
-spec build_modify_onezone_policies_data_spec() -> api_test_runner:data_spec().
build_modify_onezone_policies_data_spec() ->
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
            {<<"oneproviderRegistration">>, <<"valueNotAllowed">>, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"oneproviderRegistration">>, [<<"open">>, <<"restricted">>])},
            {<<"oneproviderRegistration">>, value_not_allowed, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"oneproviderRegistration">>, [<<"open">>, <<"restricted">>])},
            {<<"subdomainDelegation">>, not_a_boolean, ?ERROR_BAD_VALUE_BOOLEAN(<<"subdomainDelegation">>)},
            {<<"guiPackageVerification">>, not_a_boolean, ?ERROR_BAD_VALUE_BOOLEAN(<<"guiPackageVerification">>)},
            {<<"harvesterGuiPackageVerification">>, not_a_boolean, ?ERROR_BAD_VALUE_BOOLEAN(<<"harvesterGuiPackageVerification">>)}
        ]
    }.


%% @private
-spec build_modify_onezone_policies_prepare_args_fun() -> api_test_runner:prepare_args_fun().
build_modify_onezone_policies_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data}) ->
        #rest_args{
            method = patch,
            path = <<"zone/policies">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(Data)
        }
    end.


%% @private
-spec build_modify_onezone_policies_verify_fun(test_config:config()) -> api_test_runner:verify_fun().
build_modify_onezone_policies_verify_fun(Config) ->
    fun
        (expected_success, #api_test_ctx{data = Data}) ->
            OnezonePolicies = get_onezone_policies_with_rpc(Config),

            ?assert(maps_utils:is_submap(Data, OnezonePolicies)),
            true;
        (expected_failure, _) ->
            true
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec get_onezone_policies_with_rpc(test_config:config()) -> map().
get_onezone_policies_with_rpc(Config) ->
    ZoneConfiguration = oz_worker_test_rpc:get_zone_configuration(Config),
    OneproviderRegistration = oz_worker_test_rpc:get_env(Config, provider_registration_policy),
    GuiPackageVerification = oz_worker_test_rpc:get_env(Config, gui_package_verification),
    HarvesterGuiPackageVerification = oz_worker_test_rpc:get_env(Config, harvester_gui_package_verification),

    #{
        <<"guiPackageVerification">> => GuiPackageVerification,
        <<"harvesterGuiPackageVerification">> => HarvesterGuiPackageVerification,
        <<"oneproviderRegistration">> => atom_to_binary(OneproviderRegistration, utf8),
        <<"subdomainDelegation">> => maps:get(subdomainDelegationSupported, ZoneConfiguration)
    }.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    application:start(ssl),
    hackney:start(),
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl).

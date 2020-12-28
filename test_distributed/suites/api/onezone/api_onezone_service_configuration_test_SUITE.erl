%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onezone user management API (REST).
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
    get_onezone_policies_test/1

]).

all() -> [
    get_onezone_policies_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_onezone_policies_test(Config) ->
    OzPanelNodes = oct_background:get_zone_panels(),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get Onezone policies using /zone/policies endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    peer
                ]
            },

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"zone/policies">>
                }
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedPolicies = get_onezone_policies_with_rpc(Config),
                ?assertEqual(ExpectedPolicies, Body)
            end)

        }
    ])).


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec get_onezone_policies_with_rpc(test_config:config()) -> map().
get_onezone_policies_with_rpc(Config) ->
    ZoneConfiguration = oz_worker_test_rpc:get_zone_configuration(Config),
    RequiredAdminPriviliges = oz_worker_test_rpc:get_required_admin_priviliges(Config),
    OneproviderRegistration = case lists:member(oz_providers_invite, RequiredAdminPriviliges) of
        true -> <<"restricted">>;
        false -> <<"open">>
    end,
    GuiPackageVerification = oz_worker_test_rpc:get_env(Config, gui_package_verification),
    HarversterGuiPackageVerification = oz_worker_test_rpc:get_env(Config, harvester_gui_package_verification),

    #{
        <<"guiPackageVerification">> => GuiPackageVerification,
        <<"harvesterGuiPackageVerification">> => HarversterGuiPackageVerification,
        <<"oneproviderRegistration">> => OneproviderRegistration,
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

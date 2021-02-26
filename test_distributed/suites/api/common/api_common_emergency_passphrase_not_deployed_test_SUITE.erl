%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning onepanel Emergency Passphrase  API (REST).
%%% Note, that suite is run under undeployed environment.
%%% Not all features from oct_background module are available.
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_not_deployed_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_zone_panel_emergency_passphrase_status_test/1,
    get_provider_panel_emergency_passphrase_status_test/1,

    set_emergency_passphrase_with_op_panel_test/1,
    set_emergency_passphrase_with_oz_panel_test/1
]).

all() -> [
    get_zone_panel_emergency_passphrase_status_test,
    get_provider_panel_emergency_passphrase_status_test,

    set_emergency_passphrase_with_op_panel_test,
    set_emergency_passphrase_with_oz_panel_test
].


%%%===================================================================
%%% API
%%%===================================================================


get_zone_panel_emergency_passphrase_status_test(Config) ->
    get_emergency_passphrase_status_test_base(Config, test_config:get_all_oz_panel_nodes(Config)).


get_provider_panel_emergency_passphrase_status_test(Config) ->
    get_emergency_passphrase_status_test_base(Config, test_config:get_all_op_panel_nodes(Config)).


-spec get_emergency_passphrase_status_test_base(test_config:config(), [node()]) -> boolean().
get_emergency_passphrase_status_test_base(Config, TargetPanels) ->

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get emergency passphrase status using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = TargetPanels,

            % Not all clients are available due to undeployed oz-worker or unconfigured EP.
            client_spec = #client_spec{
                correct = [
                    peer,
                    guest
                ]
            },

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"emergency_passphrase">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedResponse = #{
                    <<"isSet">> => false
                },
                ?assertEqual(ExpectedResponse, Body)
            end),
            test_proxied_onepanel_rest_endpoint = false
        }
    ])).


set_emergency_passphrase_with_op_panel_test(Config) ->
    set_emergency_passphrase_test_base(Config, test_config:get_all_op_panel_nodes(Config)).


set_emergency_passphrase_with_oz_panel_test(Config) ->
    set_emergency_passphrase_test_base(Config, test_config:get_all_oz_panel_nodes(Config)).


%% @private
-spec set_emergency_passphrase_test_base(test_config:config(), [node()]) -> boolean().
set_emergency_passphrase_test_base(Config, PanelNodes) ->

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set emergency passphrase using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = PanelNodes,

            % Not all clients are available due to undeployed oz-worker or unconfigured EP.
            client_spec = #client_spec{
                correct = [guest],
                forbidden = [peer]
            },

            data_spec = build_set_emergency_passphrase_data_spec(),

            prepare_args_fun = build_set_emergency_passphrase_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_set_emergency_passphrase_verify_fun(hd(PanelNodes)),

            test_proxied_onepanel_rest_endpoint = false
        }
    ])).


%% @private
-spec build_set_emergency_passphrase_data_spec() -> api_test_runner:data_spec().
build_set_emergency_passphrase_data_spec() ->
    #data_spec{
        required = [<<"newPassphrase">>],
        correct_values = #{
            <<"newPassphrase">> => [new_passphrase_placeholder]
        }
    }.


%% @private
-spec build_set_emergency_passphrase_prepare_args_fun() -> api_test_runner:prepare_args_fun().
build_set_emergency_passphrase_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data}) ->
        NewPassphrase = str_utils:rand_hex(12),

        RequestData = api_test_utils:substitute_placeholders(Data, #{
            <<"newPassphrase">> => #{
                new_passphrase_placeholder => #placeholder_substitute{
                    value = NewPassphrase
                }
            }
        }),

        node_cache:put(requested_emergency_passphrase, NewPassphrase),

        #rest_args{
            method = put,
            path = <<"emergency_passphrase">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestData)
        }
    end.


%% @private
-spec build_set_emergency_passphrase_verify_fun(oct_background:entity_selector()) -> api_test_runner:verify_fun().
build_set_emergency_passphrase_verify_fun(PanelNode) ->
    fun(ExpectedResult, _) ->
        RequestedEP = node_cache:get(requested_emergency_passphrase, undefined),

        case ExpectedResult of
            expected_success ->
                ?assert(panel_test_rpc:verify_emergency_passphrase(PanelNode, RequestedEP));
            expected_failure ->
                ?assertNot(panel_test_rpc:verify_emergency_passphrase(PanelNode, RequestedEP))
        end,
        true
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_not_deployed"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

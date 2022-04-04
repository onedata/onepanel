%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains common test functions for tests concerning onepanel Emergency Passphrase API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_test_base).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/http/headers.hrl").

-export([
    get_emergency_passphrase_status_test_base/4,
    update_emergency_passphrase_test_base/4
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_emergency_passphrase_status_test_base(
    [node()], api_test_runner:client_spec(), boolean(), boolean()) -> boolean().
get_emergency_passphrase_status_test_base(TargetPanelNodes, ClientSpec, IsEPSet, TestProxied) ->
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get emergency passphrase status using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = ClientSpec,

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"emergency_passphrase">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedResponse = #{
                    <<"isSet">> => IsEPSet
                },
                ?assertEqual(ExpectedResponse, Body)
            end),

            test_proxied_onepanel_rest_endpoint = TestProxied
        }
    ])).


-spec update_emergency_passphrase_test_base(
    [node()], api_test_runner:client_spec(), boolean(), boolean()) -> boolean().
update_emergency_passphrase_test_base(TargetPanelNodes, ClientSpec, InitialEmergencyPassphrase, TestProxied) ->
    MemRef = api_test_memory:init(),
    api_test_memory:set(MemRef, current_emergency_passphrase, InitialEmergencyPassphrase),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Set emergency passphrase using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = ClientSpec,

            data_spec = build_update_emergency_passphrase_data_spec(),

            prepare_args_fun = build_update_emergency_passphrase_prepare_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_update_emergency_passphrase_verify_fun(lists_utils:random_element(TargetPanelNodes), MemRef),
            test_proxied_onepanel_rest_endpoint = TestProxied
        }
    ])).


%% @private
-spec build_update_emergency_passphrase_data_spec() -> api_test_runner:data_spec().
build_update_emergency_passphrase_data_spec() ->
    #data_spec{
        required = [<<"newPassphrase">>, <<"currentPassphrase">>],

        correct_values = #{
            <<"newPassphrase">> => [new_passphrase_placeholder],
            <<"currentPassphrase">> => [current_passphrase_placeholder]
        },
        bad_values = [
            {<<"currentPassphrase">>, <<"badCurrentPassphrase">>, ?ERROR_UNAUTHORIZED(?ERROR_BAD_BASIC_CREDENTIALS)}
        ]
    }.


%% @private
-spec build_update_emergency_passphrase_prepare_args_fun(binary()) -> api_test_runner:prepare_args_fun().
build_update_emergency_passphrase_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        CurrentPassphrase = api_test_memory:get(MemRef, current_emergency_passphrase),
        NewPassphrase = str_utils:rand_hex(12),

        RequestData = api_test_utils:substitute_placeholders(Data, #{
            <<"newPassphrase">> => #{
                new_passphrase_placeholder => #placeholder_substitute{
                    value = NewPassphrase
                }
            },
            <<"currentPassphrase">> => #{
                current_passphrase_placeholder => #placeholder_substitute{
                    value = CurrentPassphrase
                }
            }
        }),

        api_test_memory:set(MemRef, requested_emergency_passphrase, NewPassphrase),

        #rest_args{
            method = put,
            path = <<"emergency_passphrase">>,
            headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
            body = json_utils:encode(RequestData)
        }
    end.


%% @private
-spec build_update_emergency_passphrase_verify_fun(
    oct_background:entity_selector(), binary()) -> api_test_runner:verify_fun().
build_update_emergency_passphrase_verify_fun(Target, MemRef) ->
    fun
        (expected_success, _) ->
            ExpCurrentEP = api_test_memory:get(MemRef, requested_emergency_passphrase),
            PreviousEP = api_test_memory:get(MemRef, current_emergency_passphrase),

            ?assert(panel_test_rpc:verify_emergency_passphrase(Target, ExpCurrentEP)),
            ?assertNot(panel_test_rpc:verify_emergency_passphrase(Target, PreviousEP)),

            api_test_memory:set(MemRef, current_emergency_passphrase, ExpCurrentEP),
            true;
        (expected_failure, _) ->
            CurrentEP = api_test_memory:get(MemRef, current_emergency_passphrase),
            RequestedEP = api_test_memory:get(MemRef, requested_emergency_passphrase),

            ?assert(panel_test_rpc:verify_emergency_passphrase(Target, CurrentEP)),
            ?assertNot(panel_test_rpc:verify_emergency_passphrase(Target, RequestedEP)),
            true
    end.

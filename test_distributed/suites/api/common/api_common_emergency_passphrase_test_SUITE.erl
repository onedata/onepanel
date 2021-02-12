%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning onepanel common internal API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").


%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    get_emergency_passphrase_status_test/1,
    set_emergency_passphrase_with_op_panel_test/1,
    set_emergency_passphrase_with_oz_panel_test/1
]).

all() -> [
    get_emergency_passphrase_status_test,
    set_emergency_passphrase_with_op_panel_test,
    set_emergency_passphrase_with_oz_panel_test
].

-define(INITIAL_EMERGENCY_PASSPHRASE, <<"password">>).


%%%===================================================================
%%% API
%%%===================================================================


get_emergency_passphrase_status_test(Config) ->
    get_emergency_passphrase_status_test_base(Config, ?OP_PANEL, krakow),
    get_emergency_passphrase_status_test_base(Config, ?OZ_PANEL, zone).


-spec get_emergency_passphrase_status_test_base(test_config:config(), atom(), oct_background:entity_selector()) -> boolean().
get_emergency_passphrase_status_test_base(Config, TargetPanelType, Target) ->
    TargetId = oct_background:to_entity_id(Target),
    TargetPanelNodes = case TargetPanelType of
        ?OP_PANEL -> oct_background:get_provider_panels(Target);
        ?OZ_PANEL -> oct_background:get_zone_panels()
    end,

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get emergency passphrase status using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    member,
                    peer,
                    guest
                ],
                unauthorized = [
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(TargetPanelType, TargetId))}
                ]
            },

            setup_fun = build_get_emergency_passphrase_status_setup_fun(Target),
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"emergency_passphrase">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedResponse = #{
                    <<"isSet">> => node_cache:get(is_set_ep)
                },
                ?assertEqual(ExpectedResponse, Body)
            end)
        }
    ])).


%% @private
-spec build_get_emergency_passphrase_status_setup_fun(oct_background:entity_selector()) -> ok.
build_get_emergency_passphrase_status_setup_fun(Target) ->
    fun() ->
        IsEPSet = panel_test_rpc:is_set_emergency_passphrase(Target),
        node_cache:put(is_set_ep, IsEPSet)
    end.


set_emergency_passphrase_with_op_panel_test(Config) ->
    set_emergency_passphrase_test_base(Config, ?OP_PANEL, krakow).


set_emergency_passphrase_with_oz_panel_test(Config) ->
    set_emergency_passphrase_test_base(Config, ?OZ_PANEL, zone).


%% @private
-spec set_emergency_passphrase_test_base(test_config:config(), atom(), oct_background:entity_selector()) -> boolean().
set_emergency_passphrase_test_base(Config, TargetPanelType, Target) ->
    TargetId = oct_background:to_entity_id(Target),
    TargetPanelNodes = case TargetPanelType of
        ?OP_PANEL -> oct_background:get_provider_panels(Target);
        ?OZ_PANEL -> oct_background:get_zone_panels()
    end,

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set emergency passphrase using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
                ],
                forbidden = [
                    member,
                    peer
                ],
                unauthorized = [
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(TargetPanelType, TargetId))},
                    guest
                ]
            },

            data_spec = build_set_emergency_passphrase_data_spec(),
            setup_fun = build_get_emergency_passphrase_status_setup_fun(Target),

            prepare_args_fun = build_set_emergency_passphrase_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_set_emergency_passphrase_verify_fun(Target)
        }
    ])).


%% @private
-spec build_set_emergency_passphrase_data_spec() -> api_test_runner:data_spec().
build_set_emergency_passphrase_data_spec() ->
    #data_spec{
        required = [<<"newPassphrase">>],

        correct_values = #{
            <<"newPassphrase">> => [new_passphrase_placeholder]
        },
        bad_values = [
            {<<"newPassphrase">>, bad_passphrase_placeholder, ?ERROR_UNAUTHORIZED(?ERROR_BAD_BASIC_CREDENTIALS)}
        ]
    }.


%% @private
-spec build_set_emergency_passphrase_prepare_args_fun() -> api_test_runner:prepare_args_fun().
build_set_emergency_passphrase_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data, client = #api_client{role = Role}}) ->
        CurrentPassphrase = node_cache:get(current_emergency_passphrase),
        NewPassphrase = str_utils:rand_hex(12),

        Request = case maps:get(<<"newPassphrase">>, Data, undefined) of
            new_passphrase_placeholder ->
                case Role of
                    root ->
                        node_cache:put(previous_emergency_passphrase, CurrentPassphrase),
                        node_cache:put(current_emergency_passphrase, NewPassphrase);
                    _ -> ok
                end,
                #{
                    <<"newPassphrase">> => NewPassphrase,
                    <<"currentPassphrase">> => CurrentPassphrase
                };
            bad_passphrase_placeholder ->
                #{
                    <<"newPassphrase">> => NewPassphrase,
                    <<"currentPassphrase">> => <<"badPassphrase">>
                };
            _ -> Data
        end,

        node_cache:put(requested_emergency_passphrase, NewPassphrase),

        #rest_args{
            method = put,
            path = <<"emergency_passphrase">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(Request)
        }
    end.


%% @private
-spec build_set_emergency_passphrase_verify_fun(oct_background:entity_selector()) -> api_test_runner:verify_fun().
build_set_emergency_passphrase_verify_fun(Target) ->
    fun(ExpectedResult, _) ->
        RequestedEP = node_cache:get(requested_emergency_passphrase),
        PreviousEP = node_cache:get(previous_emergency_passphrase, <<"undefined">>),
        CurrentEP = node_cache:get(current_emergency_passphrase),
        ?assert(panel_test_rpc:verify_emergency_passphrase(Target, CurrentEP)),

        case ExpectedResult of
            expected_success ->
                ?assert(panel_test_rpc:verify_emergency_passphrase(Target, RequestedEP)),
                ?assertNot(panel_test_rpc:verify_emergency_passphrase(Target, PreviousEP));
            expected_failure ->
                ?assertNot(panel_test_rpc:verify_emergency_passphrase(Target, RequestedEP)),
                case PreviousEP of
                    <<"undefined">> -> ok;
                    _ -> ?assert(panel_test_rpc:verify_emergency_passphrase(Target, PreviousEP))
                end
        end,
        true
    end.

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
    application:stop(ssl),
    ok.


% test suite is started with pre-defined emergency passphrase
init_per_testcase(set_emergency_passphrase_with_op_panel_test, Config) ->
    node_cache:put(current_emergency_passphrase, ?INITIAL_EMERGENCY_PASSPHRASE),
    Config;
init_per_testcase(set_emergency_passphrase_with_oz_panel_test, Config) ->
    node_cache:put(current_emergency_passphrase, ?INITIAL_EMERGENCY_PASSPHRASE),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(set_emergency_passphrase_test, Config) ->
    Config;

end_per_testcase(_, Config) ->
    Config.

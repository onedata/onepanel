%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning onepanel Emergency Passphrase  API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    get_provider_panel_emergency_passphrase_status_test/1,
    get_zone_panel_emergency_passphrase_status_test/1,
    set_emergency_passphrase_with_op_panel_test/1,
    set_emergency_passphrase_with_oz_panel_test/1
]).

all() -> [
    get_provider_panel_emergency_passphrase_status_test,
    get_zone_panel_emergency_passphrase_status_test,
    set_emergency_passphrase_with_op_panel_test,
    set_emergency_passphrase_with_oz_panel_test
].

-define(INITIAL_EMERGENCY_PASSPHRASE, <<"password">>).


%%%===================================================================
%%% API
%%%===================================================================


get_provider_panel_emergency_passphrase_status_test(Config) ->
    get_emergency_passphrase_status_test_base(Config, ?OP_PANEL, krakow).


get_zone_panel_emergency_passphrase_status_test(Config) ->
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

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"emergency_passphrase">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedResponse = #{
                    <<"isSet">> => true
                },
                ?assertEqual(ExpectedResponse, Body)
            end)
        }
    ])).


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

            prepare_args_fun = build_set_emergency_passphrase_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_set_emergency_passphrase_verify_fun(Target)
        }
    ])).


%% @private
-spec build_set_emergency_passphrase_data_spec() -> api_test_runner:data_spec().
build_set_emergency_passphrase_data_spec() ->
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
-spec build_set_emergency_passphrase_prepare_args_fun() -> api_test_runner:prepare_args_fun().
build_set_emergency_passphrase_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data, client = #api_client{role = Role}}) ->
        CurrentPassphrase = node_cache:get(current_emergency_passphrase),
        NewPassphrase = str_utils:rand_hex(12),

        RequestData = api_test_utils:substitute_placeholders(Data, #{
            <<"newPassphrase">> => #{
                new_passphrase_placeholder => #placeholder_substitute{
                    value = NewPassphrase,
                    posthook = maybe_memorize_credentials(Data, Role,  NewPassphrase, CurrentPassphrase)
                }
            },
            <<"currentPassphrase">> => #{
                current_passphrase_placeholder => #placeholder_substitute{
                    value = CurrentPassphrase,
                    posthook = maybe_memorize_credentials(Data, Role, NewPassphrase, CurrentPassphrase)
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
-spec maybe_memorize_credentials(map(), atom(), term(), term()) -> fun(() -> ok).
maybe_memorize_credentials(Data, Client, NewEP, CurrentEP) ->
    fun() ->
        case {Client, maps:get(<<"currentPassphrase">>, Data, undefined), maps:get(<<"newPassphrase">>, Data, undefined)} of
            {root, current_passphrase_placeholder, new_passphrase_placeholder} ->
                node_cache:put(previous_emergency_passphrase, CurrentEP),
                node_cache:put(current_emergency_passphrase, NewEP);
            _ ->
                ok
        end
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
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


% test suite is started with pre-defined emergency passphrase
init_per_testcase(set_emergency_passphrase_with_op_panel_test, Config) ->
    node_cache:put(current_emergency_passphrase, ?INITIAL_EMERGENCY_PASSPHRASE),
    Config;
init_per_testcase(set_emergency_passphrase_with_oz_panel_test, Config) ->
    node_cache:put(current_emergency_passphrase, ?INITIAL_EMERGENCY_PASSPHRASE),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_, Config) ->
    Config.

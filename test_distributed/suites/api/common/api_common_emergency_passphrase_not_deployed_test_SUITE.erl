%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Note that this suite is run on an undeployed environment and some features
%%% of oct_background are not available.
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
    set_emergency_passphrase_with_oz_panel_test/1,

    update_emergency_passphrase_with_op_panel_test/1,
    update_emergency_passphrase_with_oz_panel_test/1
]).

all() -> [
    get_zone_panel_emergency_passphrase_status_test,
    get_provider_panel_emergency_passphrase_status_test,

    set_emergency_passphrase_with_op_panel_test,
    set_emergency_passphrase_with_oz_panel_test,

    update_emergency_passphrase_with_op_panel_test,
    update_emergency_passphrase_with_oz_panel_test
].

-define(GET_EP_STATUS_CLIENT_SPEC, #client_spec{
    correct = [
        peer,
        guest
    ]
}).

-define(SET_EP_CLIENT_SPEC, #client_spec{
    correct = [
        guest
    ],
    forbidden = [
        peer
    ]
}).

-define(UPDATE_EP_CLIENT_SPEC, #client_spec{
    correct = [
        root
    ],
    forbidden = [
        peer
    ],
    unauthorized = [
        guest
    ]
}).

-define(INITIAL_EMERGENCY_PASSPHRASE, <<"password">>).

%%%===================================================================
%%% API
%%%===================================================================


get_zone_panel_emergency_passphrase_status_test(Config) ->
    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),

    panel_test_rpc:unset_emergency_passphrase(lists_utils:random_element(OzPanelNodes)),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        Config, OzPanelNodes, ?GET_EP_STATUS_CLIENT_SPEC, false, false
    ),

    panel_test_rpc:set_emergency_passphrase(lists_utils:random_element(OzPanelNodes), ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        Config, OzPanelNodes, ?GET_EP_STATUS_CLIENT_SPEC, true, false
    ).


get_provider_panel_emergency_passphrase_status_test(Config) ->
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),

    panel_test_rpc:unset_emergency_passphrase(lists_utils:random_element(OpPanelNodes)),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        Config, OpPanelNodes, ?GET_EP_STATUS_CLIENT_SPEC, false, false
    ),

    panel_test_rpc:set_emergency_passphrase(lists_utils:random_element(OpPanelNodes), ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        Config, OpPanelNodes, ?GET_EP_STATUS_CLIENT_SPEC, true, false
    ).


set_emergency_passphrase_with_op_panel_test(Config) ->
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    panel_test_rpc:unset_emergency_passphrase(lists_utils:random_element(OpPanelNodes)),
    set_emergency_passphrase_test_base(Config, OpPanelNodes).


set_emergency_passphrase_with_oz_panel_test(Config) ->
    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),
    panel_test_rpc:unset_emergency_passphrase(lists_utils:random_element(OzPanelNodes)),
    set_emergency_passphrase_test_base(Config, OzPanelNodes).


%% @private
-spec set_emergency_passphrase_test_base(test_config:config(), [node()]) -> boolean().
set_emergency_passphrase_test_base(Config, PanelNodes) ->
    MemRef = api_test_memory:init(),
    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set emergency passphrase using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = PanelNodes,
            client_spec = ?SET_EP_CLIENT_SPEC,

            setup_fun = build_set_emergency_passphrase_setup_fun(hd(PanelNodes)),
            data_spec = build_set_emergency_passphrase_data_spec(),

            prepare_args_fun = build_set_emergency_passphrase_prepare_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_set_emergency_passphrase_verify_fun(lists_utils:random_element(PanelNodes), MemRef),

            test_proxied_onepanel_rest_endpoint = false
        }
    ])).


%% @private
-spec build_set_emergency_passphrase_setup_fun(node()) -> api_test_runner:setup_fun().
build_set_emergency_passphrase_setup_fun(TargetNode) ->
    fun() ->
        panel_test_rpc:unset_emergency_passphrase(TargetNode)
    end.


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
-spec build_set_emergency_passphrase_prepare_args_fun(binary()) -> api_test_runner:prepare_args_fun().
build_set_emergency_passphrase_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        NewPassphrase = str_utils:rand_hex(12),

        RequestData = api_test_utils:substitute_placeholders(Data, #{
            <<"newPassphrase">> => #{
                new_passphrase_placeholder => #placeholder_substitute{
                    value = NewPassphrase
                }
            }
        }),

        api_test_memory:set(MemRef, requested_emergency_passphrase, NewPassphrase),

        #rest_args{
            method = put,
            path = <<"emergency_passphrase">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestData)
        }
    end.


%% @private
-spec build_set_emergency_passphrase_verify_fun(oct_background:entity_selector(), binary()) -> api_test_runner:verify_fun().
build_set_emergency_passphrase_verify_fun(PanelNode, MemRef) ->
    fun(ExpectedResult, _) ->
        RequestedEP = api_test_memory:get(MemRef, requested_emergency_passphrase, undefined),

        case ExpectedResult of
            expected_success ->
                ?assert(panel_test_rpc:verify_emergency_passphrase(PanelNode, RequestedEP));
            expected_failure ->
                ?assertNot(panel_test_rpc:verify_emergency_passphrase(PanelNode, RequestedEP))
        end,
        true
    end.


update_emergency_passphrase_with_op_panel_test(Config) ->
    OpPanelNodes = test_config:get_all_op_panel_nodes(Config),
    panel_test_rpc:set_emergency_passphrase(lists_utils:random_element(OpPanelNodes), ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:update_emergency_passphrase_test_base(
        Config, OpPanelNodes, ?UPDATE_EP_CLIENT_SPEC, ?INITIAL_EMERGENCY_PASSPHRASE, false).


update_emergency_passphrase_with_oz_panel_test(Config) ->
    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),
    panel_test_rpc:set_emergency_passphrase(lists_utils:random_element(OzPanelNodes), ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:update_emergency_passphrase_test_base(
        Config, OzPanelNodes, ?UPDATE_EP_CLIENT_SPEC, ?INITIAL_EMERGENCY_PASSPHRASE, false).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op_not_deployed"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

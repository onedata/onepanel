%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Example test SUITE using onenv
%%% @end
%%%-------------------------------------------------------------------
-module(provider_api_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_provider_details/1
]).

all() -> [
    get_provider_details
].


%%%===================================================================
%%% API
%%%===================================================================


get_provider_details(Config) ->
    [P1 | _] = test_config:get_providers(Config),
    [SpaceId | _] = test_config:get_provider_spaces(Config, P1),

    PanelNodes = test_config:get_custom(Config, [provider_panels, P1]),
    OzNode = lists_utils:random_element(test_config:get_all_oz_worker_nodes(Config)),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get provider details using /provider rest endpoint">>,
            type = rest,
            target_nodes = PanelNodes,
            client_spec = #client_spec{
                correct = [
                    api_test_runner:create_root_client(PanelNodes),
                    api_test_runner:create_member_client(OzNode, P1, [])  % TODO should pass with no privileges?
                ],
                unauthorized = [
                    ?API_GUEST,
                    {api_test_runner:create_user_client(OzNode, SpaceId), ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    api_test_runner:create_peer_client(PanelNodes)
                ]
            },
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = <<"provider">>} end,
            validate_result_fun = fun(_, {ok, _RespCode, _, _RespBody} = Result) ->
                ct:pal("RESULT: ~p", [Result])
            end
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    Posthook = fun(NewConfig) ->
        application:start(ssl),
        hackney:start(),
        onenv_test_utils:prepare_base_test_config(NewConfig)
    end,
    test_config:set_many(Config, [
        {add_envs, [op_panel, onepanel, [{key, value}]]},
        {add_envs, [oz_panel, onepanel, [{another_key, another_value}]]},
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, Posthook}
    ]).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl),
    ok.

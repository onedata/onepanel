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
-module(storages_api_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_storages_ids/1
]).

all() -> [
    get_storages_ids
].


%%%===================================================================
%%% API
%%%===================================================================


get_storages_ids(Config) ->
    [P1] = test_config:get_providers(Config),
    OpPanelNodes = test_config:get_custom(Config, [provider_panels, P1]),

    ExpDetails = #{
        <<"ids">> =>
            [<<"d39c5ef29e7fbd161b5f9940219a4390ch1228">>,
            <<"332d747128d28c3ba83184eed4d38a8cch2e76">>,
            <<"63de760098fd13ba4958f94953916841che849">>,
            <<"e031713a6ae751df6b049614896c397fch490c">>]
    },

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get storage ids using /provider/storages rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = <<"provider/storages">>} end,
            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                ?assertEqual({?HTTP_200_OK, ExpDetails}, {RespCode, RespBody})
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
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, Posthook}
    ]).

end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl),
    ok.

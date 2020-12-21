%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning provider basic API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").


%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_emergency_passphrase_status_test/1
]).

all() -> [
    get_emergency_passphrase_status_test
].


%%%===================================================================
%%% API
%%%===================================================================
get_emergency_passphrase_status_test(Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get emergency passphrase status using /emergency_passphrase rest endpoint">>,
            type = rest,
            target_nodes = ProviderPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    member,
                    peer,
                    guest
                ],
                unauthorized = [
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                ]
            },

            setup_fun = build_get_emergency_passphrase_status_setup_fun(),

            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"emergency_passphrase">>}
            end,
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpectedResponse = #{<<"isSet">> => node_cache:get(is_set_ep)},
                ?assertEqual(ExpectedResponse, Body)
            end)
        }
    ])).


build_get_emergency_passphrase_status_setup_fun() ->
    fun() ->
        IsEPSet = onepanel_test_rpc:is_set_emergency_passphrase(krakow),
        node_cache:put(is_set_ep, IsEPSet)
    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    ct:pal("Config : ~p", [Config]),
    application:start(ssl),
    hackney:start(),

    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl),
    ok.

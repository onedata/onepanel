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
    set_emergency_passphrase_test/1


]).

all() -> [
    get_emergency_passphrase_status_test,
    set_emergency_passphrase_test

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


set_emergency_passphrase_test(Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set emergency passphrase using /emergency_passphrase rest endpoint">>,
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

            data_spec = build_set_emergency_passphrase_data_spec(),
            setup_fun = build_get_emergency_passphrase_status_setup_fun(),

            prepare_args_fun = build_set_emergency_passphrase_prepare_args_fun(),
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


build_set_emergency_passphrase_data_spec() ->
    #data_spec{
        required = [<<"newPassphrase">>],

        correct_values = #{
            <<"newPassphrase">> => [new_passphrase_placeholder]
        },
        bad_values = [
        ]
    }.

build_set_emergency_passphrase_prepare_args_fun() ->
    fun(#api_test_ctx{data = Data}) ->
        CurrentPassphrase = node_cache:get(current_emergency_passphrase),
        NewPassphrase = str_utils:rand_hex(10),
        {ok, Hash} = rpc:call(hd(oct_background:get_provider_panels(krakow)), emergency_passphrase, get_hash, []),
        ct:pal("Rpc Hash: ~p \n Hash: ~p", [Hash, onedata_passwords:create_hash(CurrentPassphrase)]),



        RequestData = api_test_utils:substitute_placeholders(Data, #{
%%            <<"currentPassphrase">> => #{
%%                current_passphrase_placeholder => #placeholder_substitute{
%%                    value = CurrentPassphrase
%%                }
%%            },
            <<"newPassphrase">> => #{
                new_passphrase_placeholder => #placeholder_substitute{
                    value = NewPassphrase,
                    posthook = fun() -> node_cache:put(current_emergency_passphrase, NewPassphrase) end
                }
            }
        }),
        timer:sleep(7000),
        Res = maps:put(<<"currentPassphrase">>, CurrentPassphrase, RequestData),

        ct:pal("Res Data: ~p", [Res]),

        #rest_args{
            method = put,
            path = <<"emergency_passphrase">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(Res)
        }
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


init_per_testcase(set_emergency_passphrase_test, Config) ->
    % test suite is started with pre-defined emergency passphrase
    node_cache:put(current_emergency_passphrase, <<"password">>),
    Config;

init_per_testcase(_, Config) ->
    Config.


end_per_testcase(set_emergency_passphrase_test, Config) ->
    Config;

end_per_testcase(_, Config) ->
    Config.

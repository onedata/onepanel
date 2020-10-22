%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning provider space API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(user_management_api_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
%%    get_user_ids_test/1
    create_user_test/1
%%    get_user_details_test/1
]).

all() -> [
%%    get_user_ids_test
    create_user_test
%%    get_user_details_test
].


%%%===================================================================
%%% API
%%%===================================================================
get_user_ids_test(Config) ->
%%    delete_all_oz_users(Config),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),
    InitialUsers = {ok, Users} = rpc:call(hd(OzWorkerNodes), user_logic, list, [aai:root_auth()]),
    ct:pal("Users: ~p", [InitialUsers]),
    get_user_ids_test_base(Config, [InitialUsers]).

delete_all_oz_users(Config)->
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),
    {ok, Users} = rpc:call(hd(OzWorkerNodes), user_logic, list, [aai:root_auth()]),
    [rpc:call(hd(OzWorkerNodes), user_logic, delete, [aai:root_auth(), User])||User <- Users].


get_user_ids_test_base(Config, ExpectedIds) ->
    delete_all_oz_users(Config),
    [P1] = test_config:get_providers(Config),
    OzPanelNodes = test_config:get_custom(Config, [oz_panel_nodes]),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get Onezone users using /zone/users endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_VIEW]}
                ],
                unauthorized = [
                    guest
%%                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
%%                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    peer
                ]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"zone/users">>
                }
            end,
            validate_result_fun = fun(_, {ok, RespCode, Headers, Body}) ->
                ct:pal("Code: ~p \nHeaders: ~p \nBody: ~p", [RespCode, Headers, Body]),
                ?assertEqual(#{<<"ids">> => ExpectedIds}, Body),
                ?assertEqual(?HTTP_200_OK, RespCode)
            end
        }
    ])).


create_user_test(Config) ->
    MemRef = api_test_memory:init(),
    [P1] = test_config:get_providers(Config),
    OzPanelNodes = test_config:get_custom(Config, [oz_panel_nodes]),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Add zone user using /zone/users endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_UNAUTHORIZED(?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>)))}
                ],
                forbidden = [
                    peer
                ]
            },
            prepare_args_fun = build_create_user_prepare_args_fun(MemRef),
            data_spec = build_create_user_data_spec(),
            validate_result_fun = api_test_validate:http_201_created(fun(Body) ->
                NewUserId = lists:last(binary:split(maps:get(<<"location">>, Body), <<"/">>, [global])),
                Users = oz_worker_test_rpc:get_user_ids(Config),
                ?assert(lists:member(NewUserId, Users)),
                ok
            end)
        }
    ])).


%% @private
build_create_user_data_spec() ->
    #data_spec{
        required = [<<"username">>, <<"password">>],
        correct_values = #{
            <<"username">> => [username_placeholder],
            <<"password">> => [<<"somePassword">>]
        }
    }.


%% @private
build_create_user_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        Username = str_utils:rand_hex(10),
        api_test_memory:set(MemRef, username, Username),
        RequestMap = case maps:get(<<"username">>, Data, undefined) of
            username_placeholder -> maps:put(<<"username">>, Username, Data);
            _ -> Data
        end,

        #rest_args{
            method = post,
            path = <<"zone/users">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestMap)}
    end.


get_user_details_test(Config) ->
    OzPanelNodes = test_config:get_custom(Config, [oz_panel_nodes]),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    {ok, Users} = rpc:call(hd(OzWorkerNodes), user_logic, list, [aai:root_auth()]),

    Id = lists_utils:random_element(Users),

    ct:pal("Users: ~p \n Selected Id: ~p", [Users, Id]),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get Onezone users ids using /zone/users rest endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root
%%                    {member, []}
                ],
                unauthorized = [
%%                    guest,
%%                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, P1))}
%%                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
%%                    peer
                ]
            },
            prepare_args_fun = fun(_) ->
                #rest_args{
                    method = get,
                    path = <<"zone/users/", Id/binary>>
                }
            end,
            validate_result_fun = fun(_, {ok, RespCode, Headres, Body}) ->

                ct:pal("Code: ~p \nHeaders: ~p \nBody: ~p", [RespCode, Headres, Body]),
                ?assertEqual(?HTTP_200_OK, RespCode)
            end
        }
    ])).


%%%===================================================================
%%% Helper functions
%%%===================================================================


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    application:start(ssl),
    hackney:start(),
    test_config:set_many(Config, [
        {set_onenv_scenario, ["1op"]}, % name of yaml file in test_distributed/onenv_scenarios
        {set_posthook, fun onenv_test_utils:prepare_base_test_config/1}
    ]).


end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl).


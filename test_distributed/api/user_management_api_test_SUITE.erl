%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onezone user management API (REST).
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
    create_user_test/1,
    get_user_details_test/1,
    set_user_password_test/1
]).

all() -> [
    create_user_test,
    get_user_details_test,
    set_user_password_test
].


%%%===================================================================
%%% API
%%%===================================================================


create_user_test(Config) ->
    MemRef = api_test_memory:init(),
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
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
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
    MemRef = api_test_memory:init(),
    OzPanelNodes = test_config:get_custom(Config, [oz_panel_nodes]),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get Onezone user details using /zone/users rest endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,

            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    peer
                ]
            },

            data_spec = build_get_user_details_data_spec(OzWorkerNodes),
            setup_fun = build_get_user_details_setup_fun(Config, MemRef),
            prepare_args_fun = build_get_user_details_prepare_rest_args_fun(MemRef),

            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                UserId = api_test_memory:get(MemRef, user_id),
                Username = api_test_memory:get(MemRef, username),
                Fullname = api_test_memory:get(MemRef, full_name),

                ?assertEqual(UserId, maps:get(<<"userId">>, Body)),
                ?assertEqual(Username, maps:get(<<"username">>, Body)),
                ?assertEqual(Fullname, maps:get(<<"fullName">>, Body))
            end)
        }
    ])).


%% @private
build_get_user_details_setup_fun(Config, MemRef) ->
    fun() ->
        Fullname = str_utils:rand_hex(5),
        Username = str_utils:rand_hex(5),
        Password = str_utils:rand_hex(10),
        UserId = create_user(Config, Username, Fullname, Password),

        api_test_memory:set(MemRef, full_name, Fullname),
        api_test_memory:set(MemRef, username, Username),
        api_test_memory:set(MemRef, user_id, UserId)
    end.

%% @private
build_get_user_details_data_spec(OzWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OzWorkerNodes),
    #data_spec{
        bad_values = [{bad_id, <<"NonExistentUserId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}]
    }.


%% @private
build_get_user_details_prepare_rest_args_fun(Memref) ->
    fun(#api_test_ctx{data = Data}) ->
        UserId = api_test_memory:get(Memref, user_id),
        {Id, _} = api_test_utils:maybe_substitute_bad_id(UserId, Data),

        #rest_args{
            method = get,
            path = <<"zone/users/", Id/binary>>
        }
    end.


set_user_password_test(Config) ->
    MemRef = api_test_memory:init(),
    OzPanelNodes = test_config:get_custom(Config, [oz_panel_nodes]),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Set Onezone user password using /zone/users rest endpoint">>,
            type = rest,
            target_nodes = OzPanelNodes,

            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OZ_PANEL, <<"onezone">>))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    peer
                ]
            },

            data_spec = build_set_user_password_data_spec(OzWorkerNodes),
            setup_fun = build_set_user_password_setup_fun(Config, MemRef),
            prepare_args_fun = build_set_user_password_prepare_rest_args_fun(MemRef),
            verify_fun = build_set_user_password_verify_fun(MemRef, Config),
            validate_result_fun = api_test_validate:http_204_no_content()
        }
    ])).


%% @private
build_set_user_password_data_spec(OzWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OzWorkerNodes),
    #data_spec{
        required = [<<"newPassword">>],
        correct_values = #{
            <<"newPassword">> => [password_placeholder]
        },
        bad_values = [
            {bad_id, <<"NonExistentUserId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}
        ]
    }.


%% @private
build_set_user_password_setup_fun(Config, MemRef) ->
    fun() ->
        Fullname = str_utils:rand_hex(5),
        Username = str_utils:rand_hex(5),
        Password = str_utils:rand_hex(10),
        UserId = create_user(Config, Fullname, Username, Password),

        api_test_memory:set(MemRef, password, Password),
        api_test_memory:set(MemRef, user_id, UserId)
    end.


%% @private
build_set_user_password_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        UserId = api_test_memory:get(MemRef, user_id),
        NewPassword = str_utils:rand_hex(10),
        api_test_memory:set(MemRef, new_password, NewPassword),

        {Id, DataWithoutId} = api_test_utils:maybe_substitute_bad_id(UserId, Data),
        RequestMap = case maps:get(<<"newPassword">>, DataWithoutId, undefined) of
            password_placeholder -> maps:put(<<"newPassword">>, NewPassword, DataWithoutId);
            _ -> DataWithoutId
        end,

        #rest_args{
            method = patch,
            path = <<"zone/users/", Id/binary>>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestMap)
        }
    end.


%% @private
build_set_user_password_verify_fun(MemRef, Config) ->
    fun(ExpectedResult, _) ->
        UserId = api_test_memory:get(MemRef, user_id),
        OldPassword = api_test_memory:get(MemRef, password),
        UpdatedPassword = api_test_memory:get(MemRef, new_password),

        case ExpectedResult of
            expected_success ->
                ?assertEqual(fail, oz_worker_test_rpc:change_user_password(Config, UserId, OldPassword, <<"somePassword">>)),
                ?assertEqual(ok, oz_worker_test_rpc:change_user_password(Config, UserId, UpdatedPassword, <<"somePassword">>));
            expected_failure ->
                ?assertEqual(fail, oz_worker_test_rpc:change_user_password(Config, UserId, UpdatedPassword, <<"somePassword">>))
        end,
        true
    end.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
-spec create_user(test_config:config(), binary(), binary(), basic_auth:password()) -> od_user:id().
create_user(Config, Username, Fullname, Password) ->
    UserData = #{
        <<"username">> => Username,
        <<"fullName">> => Fullname,
        <<"password">> => Password
    },
    oz_worker_test_rpc:create_user(Config, UserData).


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

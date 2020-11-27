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
-include("api_test_utils.hrl").
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

    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    GroupIds = [oz_worker_test_rpc:create_group(Config, str_utils:rand_hex(6)) || _ <- lists:seq(1, 3)],

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

            setup_fun = build_create_user_setup_fun(Config, MemRef),
            data_spec = build_create_user_data_spec(OzWorkerNodes, GroupIds),
            prepare_args_fun = build_create_user_prepare_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_201_created("zone/users/", <<"id">>,
                fun(NewUserId) ->
                    api_test_memory:set(MemRef, user_id, NewUserId),
                    Users = oz_worker_test_rpc:list_users(Config),
                    ?assert(lists:member(NewUserId, Users))
                end
            ),
            verify_fun = build_create_user_verify_fun(MemRef, Config, GroupIds)
        }
    ])).


%% @private
-spec build_create_user_setup_fun(test_config:config(), api_test_memory:env_ref()) -> api_test_runner:setup_fun().
build_create_user_setup_fun(Config, MemRef) ->
    fun() ->
        GroupName = str_utils:rand_hex(10),
        GroupId = oz_worker_test_rpc:create_group(Config, GroupName),
        api_test_memory:set(MemRef, group_id, GroupId),
        ok
    end.


%% @private
-spec build_create_user_data_spec([node()], [binary()]) -> api_test_runner:data_spec().
build_create_user_data_spec(OzWorkerNodes, GroupIds) ->
    HostNames = api_test_utils:to_hostnames(OzWorkerNodes),
    TooShortPassword = <<"abcdefg">>,
    TooLongFullname = str_utils:rand_hex(52),
    TooShortFullname = <<"a">>,
    #data_spec{
        required = [<<"username">>, <<"password">>],
        optional = [<<"fullName">>, <<"groups">>],
        correct_values = #{
            <<"username">> => [username_placeholder],
            <<"password">> => [<<"somePassword">>],
            <<"fullName">> => [fullname_placeholder],
            <<"groups">> => [GroupIds]
        },
        bad_values = [
            {<<"password">>, <<>>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)},
            {<<"password">>, <<"">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)},
            {<<"password">>, TooShortPassword, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)},
            {<<"fullName">>, <<>>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_FULL_NAME, HostNames)},
            {<<"fullName">>, TooShortFullname, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_FULL_NAME, HostNames)},
            {<<"fullName">>, TooLongFullname, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_FULL_NAME, HostNames)},
            {<<"groups">>, [<<>>], ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)},
            {<<"groups">>, [<<"inexistentGroupId">>], ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}
        ]
    }.


%% @private
-spec build_create_user_prepare_args_fun(integer()) -> api_test_runner:prepare_ergs_fun().
build_create_user_prepare_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        Username = str_utils:rand_hex(10),
        FullName = str_utils:rand_hex(10),
        RequestData = api_test_utils:substitute_placeholders(Data, #{
            <<"username">> => #{
                username_placeholder => #placeholder_substitute{
                    value = Username
                }
            },
            <<"fullName">> => #{
                fullname_placeholder => #placeholder_substitute{
                    value = FullName
                }
            }
        }),
        api_test_memory:set(MemRef, request_data, RequestData),

        #rest_args{
            method = post,
            path = <<"zone/users">>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestData)}
    end.


%% @private
-spec build_create_user_verify_fun(api_test_memory:env_ref(), test_config:config(), [binary()]) -> api_test_runner:verify_fun().
build_create_user_verify_fun(MemRef, Config, Groups) ->
    fun
        (expected_success, _) ->
            UserId = api_test_memory:get(MemRef, user_id),
            UserDetails = oz_worker_test_rpc:get_user_details(Config, UserId),
            RequestData = api_test_memory:get(MemRef, request_data),
            ExpectedUsername = maps:get(<<"username">>, RequestData),
            ExpectedPassword = maps:get(<<"password">>, RequestData),

            ?assertEqual(maps:is_key(<<"groups">>, RequestData), is_user_member_of_groups(Config, UserId, Groups)),
            ?assertEqual(maps:get(<<"fullName">>, RequestData, <<"Unnamed User">>), maps:get(<<"fullName">>, UserDetails)),
            ?assertEqual(ExpectedUsername, maps:get(<<"username">>, UserDetails)),
            ?assert(authentication_succeeds(Config, ExpectedUsername, ExpectedPassword)),
            true;
        (expected_failure, _) ->
            true
    end.


%% @private
-spec is_user_member_of_groups(test_config:config(), binary(), [binary()]) -> boolean().
is_user_member_of_groups(Config, UserId, GroupIds) ->
    lists:foldl(fun(GroupId, Acc) ->
        Acc and lists:member(UserId, oz_worker_test_rpc:get_group_users(Config, GroupId))
    end, true, GroupIds).


get_user_details_test(Config) ->
    MemRef = api_test_memory:init(),
    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),
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

            setup_fun = build_get_user_details_setup_fun(Config, MemRef),
            data_spec = build_get_user_details_data_spec(OzWorkerNodes),
            prepare_args_fun = build_get_user_details_prepare_rest_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_200_ok(fun(Body) ->
                ExpUserDetails = api_test_memory:get(MemRef, user_details),
                ?assertEqual(ExpUserDetails, Body)
            end)
        }
    ])).


%% @private
-spec build_get_user_details_setup_fun(test_config:config(), api_test_memory:env_ref()) -> api_test_runner:setup_fun().
build_get_user_details_setup_fun(Config, MemRef) ->
    fun() ->
        Fullname = str_utils:rand_hex(5),
        Username = str_utils:rand_hex(5),
        Password = str_utils:rand_hex(10),
        UserId = create_user(Config, Username, Fullname, Password),
        UserDetails = #{
            <<"userId">> => UserId,
            <<"username">> => Username,
            <<"fullName">> => Fullname
        },

        api_test_memory:set(MemRef, user_details, UserDetails),
        api_test_memory:set(MemRef, user_id, UserId)
    end.


%% @private
-spec build_get_user_details_data_spec([node()]) -> api_test_runner:data_spec().
build_get_user_details_data_spec(OzWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OzWorkerNodes),
    #data_spec{
        bad_values = [{bad_id, <<"NonExistentUserId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)}]
    }.


%% @private
-spec build_get_user_details_prepare_rest_args_fun(api_test_memory:env_ref()) -> api_test_runner:prepare_args_fun().
build_get_user_details_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        UserId = api_test_memory:get(MemRef, user_id),
        {Id, _} = api_test_utils:maybe_substitute_bad_id(UserId, Data),

        #rest_args{
            method = get,
            path = <<"zone/users/", Id/binary>>
        }
    end.


set_user_password_test(Config) ->
    MemRef = api_test_memory:init(),
    OzPanelNodes = test_config:get_all_oz_panel_nodes(Config),
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),

    Fullname = str_utils:rand_hex(5),
    Username = str_utils:rand_hex(10),
    Password = str_utils:rand_hex(10),
    UserId = create_user(Config, Username, Fullname, Password),

    api_test_memory:set(MemRef, user_id, UserId),
    api_test_memory:set(MemRef, old_password, Password),
    api_test_memory:set(MemRef, user_name, Username),

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
            prepare_args_fun = build_set_user_password_prepare_rest_args_fun(MemRef),
            validate_result_fun = api_test_validate:http_204_no_content(),
            verify_fun = build_set_user_password_verify_fun(MemRef, Config)
        }
    ])).


%% @private
-spec build_set_user_password_data_spec([node()]) -> api_test_runner:data_spec().
build_set_user_password_data_spec(OzWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OzWorkerNodes),
    TooShortPassword = <<"abcdefg">>,
    #data_spec{
        required = [<<"newPassword">>],
        correct_values = #{
            <<"newPassword">> => [new_password_placeholder, old_password_placeholder]
        },
        bad_values = [
            {bad_id, <<"NonExistentUserId">>, ?ERROR_ON_NODES(?ERROR_NOT_FOUND, HostNames)},
            {<<"newPassword">>, <<>>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)},
            {<<"newPassword">>, <<"">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)},
            {<<"newPassword">>, TooShortPassword, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_PASSWORD, HostNames)}
        ]
    }.


%% @private
-spec build_set_user_password_prepare_rest_args_fun(api_test_memory:env_ref()) -> api_test_runner:prepare_args_fun().
build_set_user_password_prepare_rest_args_fun(MemRef) ->
    fun(#api_test_ctx{data = Data}) ->
        UserId = api_test_memory:get(MemRef, user_id),
        {Id, DataWithoutId} = api_test_utils:maybe_substitute_bad_id(UserId, Data),

        NewPassword = str_utils:rand_hex(10),
        OldPassword = api_test_memory:get(MemRef, old_password),

        RequestData = api_test_utils:substitute_placeholders(DataWithoutId, #{
            <<"newPassword">> => #{
                new_password_placeholder => #placeholder_substitute{
                    value = NewPassword,
                    posthook = fun() ->
                        api_test_memory:set(MemRef, new_password, NewPassword)
                    end
                },
                old_password_placeholder => #placeholder_substitute{
                    value = OldPassword,
                    posthook = fun() ->
                        api_test_memory:set(MemRef, new_password, OldPassword)
                    end

                }
            }
        }),

        #rest_args{
            method = patch,
            path = <<"zone/users/", Id/binary>>,
            headers = #{<<"content-type">> => <<"application/json">>},
            body = json_utils:encode(RequestData)
        }
    end.


%% @private
-spec build_set_user_password_verify_fun(api_test_memory:env_ref(), test_config:config()) -> api_test_runner:verify_fun().
build_set_user_password_verify_fun(MemRef, Config) ->
    fun(ExpectedResult, _) ->
        UserName = api_test_memory:get(MemRef, user_name),
        OldPassword = api_test_memory:get(MemRef, old_password),
        NewPassword = api_test_memory:get(MemRef, new_password),

        case ExpectedResult of
            expected_success ->
                ?assert(authentication_succeeds(Config, UserName, NewPassword)),
                NewPassword /= OldPassword andalso ?assertNot(authentication_succeeds(Config, UserName, OldPassword));
            expected_failure ->
                ?assert(authentication_succeeds(Config, UserName, OldPassword)),
                NewPassword /= OldPassword andalso ?assertNot(authentication_succeeds(Config, UserName, NewPassword))
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


%% @private
-spec authentication_succeeds(test_config:config(), binary(), basic_auth:password()) -> boolean().
authentication_succeeds(Config, UserName, Password) ->
    case oz_worker_test_rpc:authenticate(Config, UserName, Password) of
        {true, _} -> true;
        {error, _} -> false
    end.


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

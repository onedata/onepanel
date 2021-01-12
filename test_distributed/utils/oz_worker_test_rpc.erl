%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides functions for making rpc calls to OzWorker Nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(oz_worker_test_rpc).
-author("Piotr Duleba").

-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([
    get_zone_configuration/1,
    get_env/2,
    create_group/2,
    get_group_users/2,
    list_users/1,
    create_user/1,
    create_user/2,
    get_user_details/2,
    authenticate/3,
    create_space/3,
    create_space_support_token/3,
    get_spaces_ids/1,
    delete_space/2
]).


-spec get_zone_configuration(test_config:config()) -> map().
get_zone_configuration(Config) ->
    {ok, ZoneConfiguration} = ?assertMatch({ok, _}, call_zone_node(Config, zone_logic, get_configuration, [])),
    ZoneConfiguration.


-spec get_env(test_config:config(), atom()) -> term().
get_env(Config, Env) ->
    ?assertMatch(_, call_zone_node(Config, oz_worker, get_env, [Env])).


-spec create_group(test_config:config(), binary()) -> binary().
create_group(Config, GroupName) ->
    {ok, GroupId} = ?assertMatch({ok, _}, call_zone_node(Config, group_logic, create, [aai:root_auth(), GroupName])),
    GroupId.


-spec get_group_users(test_config:config(), binary()) -> [binary()].
get_group_users(Config, GroupId) ->
    {ok, Users} = ?assertMatch({ok, _}, call_zone_node(Config, group_logic, get_users, [aai:root_auth(), GroupId])),
    Users.


-spec list_users(test_config:config()) -> [binary()].
list_users(Config) ->
    {ok, Users} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, list, [aai:root_auth()])),
    Users.


-spec create_user(test_config:config()) -> binary().
create_user(Config) ->
    {ok, UserId} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, create, [aai:root_auth()])),
    UserId.


-spec create_user(test_config:config(), map()) -> binary().
create_user(Config, Data) ->
    {ok, UserId} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, create, [aai:root_auth(), Data])),
    UserId.


-spec get_user_details(test_config:config(), binary()) -> map().
get_user_details(Config, UserId) ->
    {ok, UserDetails} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, get_protected_data, [aai:root_auth(), UserId])),
    UserDetails.


-spec authenticate(test_config:config(), binary(), binary()) -> {true, aai:auth()} | {error, errors:unauthorized_error()}.
authenticate(Config, UserName, Password) ->
    call_zone_node(Config, basic_auth, authenticate, [UserName, Password]).


-spec create_space(test_config:config(), binary(), binary()) -> binary().
create_space(Config, UserId, SpaceName) ->
    {ok, SpaceId} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, create_space, [aai:user_auth(UserId), UserId, SpaceName])),
    SpaceId.


-spec create_space_support_token(test_config:config(), binary(), binary()) -> binary().
create_space_support_token(Config, UserId, SpaceId) ->
    {ok, Token} = ?assertMatch({ok, _}, call_zone_node(Config, space_logic, create_space_support_token, [aai:user_auth(UserId), SpaceId])),
    Token.


-spec get_spaces_ids(test_config:config()) -> [binary()].
get_spaces_ids(Config) ->
    {ok, Spaces} = ?assertMatch({ok, _}, call_zone_node(Config, space_logic, list, [aai:root_auth()])),
    Spaces.


-spec delete_space(test_config:config(), binary()) -> ok.
delete_space(Config, SpaceId) ->
    ?assertMatch(ok, call_zone_node(Config, space_logic, delete, [aai:root_auth(), SpaceId])).


%%%===================================================================
%%% Helper functions
%%%===================================================================


call_zone_node(Config, Module, Function, Args) ->
    OzWorkerNodes = test_config:get_all_oz_worker_nodes(Config),
    rpc:call(lists_utils:random_element(OzWorkerNodes), Module, Function, Args).
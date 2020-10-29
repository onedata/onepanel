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
    create_user/1,
    create_space/3,
    create_space_support_token/3,
    get_spaces_ids/1,
    delete_space/2
]).

-spec create_user(test_config:config()) -> binary().
create_user(Config) ->
    {ok, UserId} = ?assertMatch({ok, _}, call_zone_node(Config, user_logic, create, [aai:root_auth()])),
    UserId.


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
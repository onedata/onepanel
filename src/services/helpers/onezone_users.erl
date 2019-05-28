%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains functions invoked by service_onezone
%%% action steps for managing Onezone users.
%%% @end
%%%--------------------------------------------------------------------
-module(onezone_users).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% API
-export([user_exists/1]).

%% Steps
-export([create_default_admin/1, migrate_users/1]).
-export([add_users/1, get_user/1, list_users/1, set_user_password/1]).

-define(DEFAULT_ADMIN_USERNAME,
    onepanel_env:get(zone_default_admin_username, ?APP_NAME, <<"admin">>)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec user_exists(binary()) -> boolean().
user_exists(UserId) ->
    {Node, _} = get_node_and_client(),
    rpc:call(Node, user_logic, exists, [UserId]).


%%%===================================================================
%%% Step functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Adds Onezone users with basic auth enabled.
%% Silently skips users whose username is already occupied.
%% @end
%%--------------------------------------------------------------------
-spec add_users(#{onezone_users := [User]}) -> ok when
    User :: #{username := binary(), password := binary(), groups := [binary()]}.
add_users(#{onezone_users := Users}) ->
    {OzNode, Client} = get_node_and_client(),
    lists:foreach(fun(User) ->
        Data = onepanel_maps:get_store_multiple([
            {username, <<"username">>},
            {password, <<"password">>}
        ], User),
        case rpc:call(OzNode, user_logic, create, [Client, Data]) of
            {ok, UserId} ->
                Groups = maps:get(groups, User),
                add_user_to_groups(OzNode, Client, UserId, Groups);
            ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>) -> ok;
            Error -> ?throw_error(Error)
        end
    end, Users).

%% @private
-spec add_user_to_groups(OzNode :: node(), onezone_client:logic_client(),
    UserId :: binary(), Groups :: [binary()]) -> ok.
add_user_to_groups(OzNode, LogicClient, UserId, Groups) ->
    lists:foreach(fun(GroupId) ->
        {ok, _} = rpc:call(OzNode, group_logic, add_user,
            [LogicClient, GroupId, UserId])
    end, Groups).


%%--------------------------------------------------------------------
%% @doc
%% Lists Ids of Onezone users.
%% @end
%%--------------------------------------------------------------------
-spec list_users(service:ctx()) -> #{ids := [UserId :: binary()]}.
list_users(_Ctx) ->
    {OzNode, Client} = get_node_and_client(),
    {ok, Ids} = rpc:call(OzNode, user_logic, list, [Client]),
    #{ids => Ids}.


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone user information.
%% @end
%%--------------------------------------------------------------------
-spec get_user(#{user_id := binary()}) -> #{atom() := binary()}.
get_user(#{user_id := UserId}) ->
    {OzNode, Client} = get_node_and_client(),
    {ok, Details} = rpc:call(OzNode, user_logic, get_as_user_details, [Client, UserId]),
    #user_details{id = UserId, username = Username, full_name = FullName} = Details,
    #{userId => UserId, username => Username, fullName => FullName}.


%%--------------------------------------------------------------------
%% @doc
%% If the default admin user does not exist in Onezone,
%% it is created with password set to onepanel's emergency passphrase.
%% @end
%%--------------------------------------------------------------------
-spec create_default_admin(service:ctx()) -> ok.
create_default_admin(_Ctx) ->
    case get_by_username(?DEFAULT_ADMIN_USERNAME) of
        {ok, _} -> ok;
        {error, not_found} ->
            {ok, PasswordHash} = emergency_passphrase:get_hash(),
            RandomId = onepanel_utils:gen_uuid(),
            migrate_user(RandomId, ?DEFAULT_ADMIN_USERNAME, PasswordHash, admin)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds onepanel_users as Onezone users with basic auth.
%% @end
%%--------------------------------------------------------------------
-spec migrate_users(service:ctx()) -> ok.
migrate_users(_Ctx) ->
    lists:foreach(fun migrate_user/1, onepanel_user:list()).


%%--------------------------------------------------------------------
%% @doc
%% Sets password for an existing user.
%% @end
%%--------------------------------------------------------------------
-spec set_user_password(#{user_id := binary(), new_password := binary()}) -> ok.
set_user_password(#{user_id := UserId, new_password := NewPassword}) ->
    {OzNode, Client} = get_node_and_client(),
    ok = rpc:call(OzNode, user_logic, set_password, [Client, UserId, NewPassword]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec migrate_user(onepanel_user:record()) -> ok.
migrate_user(OnepanelUser) ->
    #onepanel_user{
        uuid = UUID, username = Username,
        password_hash = PasswordHash, role = Role
    } = OnepanelUser,
    migrate_user(UUID, Username, PasswordHash, Role).

%% @private
-spec migrate_user(onepanel_user:uuid(), Username :: binary(),
    PasswordHash :: binary(), Role :: onepanel_user:role()) -> ok.
migrate_user(UserUUID, Username, PasswordHash, Role) ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {ok, _} = rpc:call(OzNode, basic_auth, migrate_onepanel_user_to_onezone,
        [UserUUID, Username, PasswordHash, Role]),
    ok.


%% @private
-spec get_by_username(Username :: binary()) ->
    {ok, UserId :: binary()} | {error, not_found}.
get_by_username(Username) ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    rpc:call(OzNode, od_user, get_by_username, [Username]).


%% @private
-spec get_node_and_client() ->
    {OzwNode :: node(), Client :: onezone_client:logic_client()}.
get_node_and_client() ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {ok, Client} = service_oz_worker:get_root_logic_client(),
    {OzNode, Client}.

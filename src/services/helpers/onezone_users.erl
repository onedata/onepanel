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
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% API
-export([user_exists/1]).

%% Steps
-export([create_default_admin/1, migrate_users/1]).
-export([add_user/1, add_users/1, get_user/1, list_users/1,
    set_user_password/1]).

-define(DEFAULT_ADMIN_USERNAME,
    onepanel_env:get(zone_default_admin_username, ?APP_NAME, <<"admin">>)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec user_exists(binary()) -> boolean().
user_exists(UserId) ->
    oz_worker_rpc:user_exists(UserId).


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
    lists:foreach(fun(UserData) ->
        case add_user(UserData) of
            {ok, _Id} -> ok;
            ?ERROR_BAD_VALUE_IDENTIFIER_OCCUPIED(<<"username">>) -> ok;
            Error -> throw(Error)
        end
    end, Users).


%%--------------------------------------------------------------------
%% @doc
%% Adds Onezone user with basic auth enabled.
%% @end
%%--------------------------------------------------------------------
-spec add_user(User) -> {ok, UserId :: binary()} | errors:error() when
    User :: #{username := binary(), password := binary(), fullName := binary(), groups := [binary()]}.
add_user(User) ->
    {OzNode, Client} = get_node_and_client(),
    Data = kv_utils:copy_found([
        {username, <<"username">>},
        {password, <<"password">>},
        {fullName, <<"fullName">>}
    ], User),
    case oz_worker_rpc:create_user(OzNode, Client, Data) of
        {ok, UserId} ->
            Groups = maps:get(groups, User),
            add_user_to_groups(OzNode, Client, UserId, Groups),
            {ok, UserId};
        Error ->
            Error
    end.


%% @private
-spec add_user_to_groups(OzNode :: node(), aai:auth(),
    UserId :: binary(), Groups :: [binary()]) -> ok.
add_user_to_groups(OzNode, Auth, UserId, Groups) ->
    lists:foreach(fun(GroupId) ->
        case oz_worker_rpc:add_user_to_group(OzNode, Auth, GroupId, UserId) of
            {ok, _} -> ok;
            {error, _} = Error -> throw(Error)
        end
    end, Groups).


%%--------------------------------------------------------------------
%% @doc
%% Lists Ids of Onezone users.
%% @end
%%--------------------------------------------------------------------
-spec list_users(service:step_ctx()) -> [UserId :: binary()].
list_users(_Ctx) ->
    {OzNode, Client} = get_node_and_client(),
    {ok, Ids} = oz_worker_rpc:list_users(OzNode, Client),
    Ids.


%%--------------------------------------------------------------------
%% @doc
%% Returns Onezone user information.
%% @end
%%--------------------------------------------------------------------
-spec get_user(#{user_id := binary()}) -> #{atom() := binary()}.
get_user(#{user_id := UserId}) ->
    {_, Client} = get_node_and_client(),
    case oz_worker_rpc:get_user_details(Client, UserId) of
        {ok, Details} ->
            #user_details{id = UserId, username = Username, full_name = FullName} = Details,
            #{userId => UserId, username => Username, fullName => FullName};
        Error ->
            throw(Error)
    end.


%%--------------------------------------------------------------------
%% @doc
%% If the default admin user does not exist in Onezone,
%% it is created with password set to onepanel's emergency passphrase.
%% @end
%%--------------------------------------------------------------------
-spec create_default_admin(service:step_ctx()) -> ok.
create_default_admin(_Ctx) ->
    case oz_worker_rpc:username_exists(?DEFAULT_ADMIN_USERNAME) of
        true -> ok;
        false ->
            {ok, PasswordHash} = emergency_passphrase:get_hash(),
            RandomId = onepanel_utils:gen_uuid(),
            migrate_user(RandomId, ?DEFAULT_ADMIN_USERNAME, PasswordHash, admin)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Adds onepanel_users as Onezone users with basic auth.
%% @end
%%--------------------------------------------------------------------
-spec migrate_users(service:step_ctx()) -> ok.
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
    oz_worker_rpc:set_user_password(OzNode, Client, UserId, NewPassword).


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
    {ok, _} = oz_worker_rpc:migrate_onepanel_user_to_onezone(
        UserUUID, Username, PasswordHash, Role),
    ok.

%% @private
-spec get_node_and_client() ->
    {OzwNode :: node(), aai:auth()}.
get_node_and_client() ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {OzNode, ?ROOT}.

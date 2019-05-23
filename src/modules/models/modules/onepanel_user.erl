%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains user management functions.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_user).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("validation.hrl").

-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, create/1, save/1, update/2,
    upgrade/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([create/3, create_noexcept/3,
    authenticate_by_auth_token/1,
    authenticate_by_basic_auth/2,
    hash_password/1, change_password/2]).
-export([no_admin_exists/0, get_by_role/1]).
-export([validate_username/1, validate_password/1, validate_role/1]).

-type name() :: binary().
-type password() :: binary().
-type password_hash() :: binary().
-type role() :: admin | regular.
-type uuid() :: binary().
-type record() :: #onepanel_user{}.

-export_type([name/0, password/0, password_hash/0, role/0, uuid/0, record/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_fields/0}
%% @end
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_record_version/0}
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> model_behaviour:version().
get_record_version() ->
    1.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:upgrade/2}
%% @end
%%--------------------------------------------------------------------
-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    no_return().
upgrade(1, _Record) ->
    ?throw_error(?ERR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    ok.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, name()} | #error{} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Record :: record()) -> ok | no_return().
save(Record) ->
    model:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | no_return().
update(Key, Diff) ->
    model:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: record()} | #error{} | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    model:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:delete/1}
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | no_return().
delete(Key) ->
    model:delete(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:list/0}
%% @end
%%--------------------------------------------------------------------
-spec list() -> Records :: [model_behaviour:record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Validates credentials and creates user account. Throws an exception
%% if username is not available.
%% @end
%%--------------------------------------------------------------------
-spec create(Username :: name(), Password :: password(), Role :: role()) ->
    {ok, name()} | no_return().
create(Username, Password, Role) ->
    case create_noexcept(Username, Password, Role) of
        {ok, Username} -> {ok, Username};
        #error{reason = ?ERR_ALREADY_EXISTS} ->
            ?throw_error(?ERR_USERNAME_NOT_AVAILABLE)
    end.


%%--------------------------------------------------------------------
%% @doc Validates credentials and creates user account.
%% @end
%%--------------------------------------------------------------------
-spec create_noexcept(Username :: name(), Password :: password(), Role :: role()) ->
    {ok, name()} | #error{}.
create_noexcept(Username, Password, Role) ->
    validate_credentials(Username, Password, Role),
    create(#onepanel_user{
        username = Username, role = Role, uuid = onepanel_utils:gen_uuid(),
        password_hash = hash_password(Password)
    }).


%%--------------------------------------------------------------------
%% @doc Authenticates user by REST API token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_auth_token(onepanel_session:auth_token()) ->
    {ok, User :: #onepanel_user{}} | #error{}.
authenticate_by_auth_token(RestApiToken) ->
    case onepanel_session:find_by_valid_auth_token(RestApiToken) of
        {ok, Session} ->
            case onepanel_user:get(onepanel_session:get_username(Session)) of
                {ok, User} -> {ok, User};
                #error{} = Error ->
                    ?warning("No user matching session ~s found: ~p", [
                        onepanel_session:get_id(Session), Error
                    ]),
                    Error
            end;
        #error{} ->
            ?make_error(?ERR_INVALID_AUTH_TOKEN)
    end.


%%--------------------------------------------------------------------
%% @doc Authenticates user by checking provided hash against the one stored in
%% user model instance.
%% Only users with role 'admin' are allowed to use basic auth.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Username :: name(), Password :: password()) ->
    {ok, User :: #onepanel_user{}} | #error{}.
authenticate_by_basic_auth(Username, Password) ->
    case onepanel_user:get(Username) of
        {ok, #onepanel_user{password_hash = undefined}} ->
            ?make_error(?ERR_INVALID_USERNAME_OR_PASSWORD);
        {ok, #onepanel_user{password_hash = Hash} = User} ->
            case onedata_passwords:verify(Password, Hash) of
                true -> {ok, User};
                false -> ?make_error(?ERR_INVALID_USERNAME_OR_PASSWORD)
            end;
        _ -> ?make_error(?ERR_INVALID_USERNAME_OR_PASSWORD)
    end.


%%--------------------------------------------------------------------
%% @doc Creates password hash.
%% @end
%%--------------------------------------------------------------------
-spec hash_password(Password :: password()) -> PasswordHash :: password_hash().
hash_password(Password) ->
    onedata_passwords:create_hash(Password).


%%--------------------------------------------------------------------
%% @doc Validates and updates user password.
%% @end
%%--------------------------------------------------------------------
-spec change_password(Username :: name(), NewPassword :: password()) ->
    ok | no_return().
change_password(Username, NewPassword) ->
    ?MODULE:validate_password(NewPassword),
    update(Username, #{password_hash => hash_password(NewPassword)}).


%%--------------------------------------------------------------------
%% @doc Checks if any user with admin privileges exists.
%% @end
%%--------------------------------------------------------------------
-spec no_admin_exists() -> boolean().
no_admin_exists() ->
    get_by_role(admin) == [].


%%--------------------------------------------------------------------
%% @doc Returns a list of users with the specified role.
%% @end
%%--------------------------------------------------------------------
-spec get_by_role(Role :: role()) -> Users :: [#onepanel_user{}].
get_by_role(Role) ->
    case model:exists(?MODULE) of
        true -> model:select(?MODULE,
            [{#onepanel_user{role = Role, _ = '_'}, []}]);
        false -> []
    end.


%%--------------------------------------------------------------------
%% @doc Validates username. It must not be empty and must not contain
%% a colon character.
%% @end
%%--------------------------------------------------------------------
-spec validate_username(Username :: name()) -> ok | no_return().
validate_username(<<>>) ->
    ?throw_error(?ERR_INVALID_USERNAME, [<<>>]);
validate_username(Username) ->
    case re:run(Username, ?USERNAME_VALIDATION_REGEXP, [{capture, none}]) of
        match -> ok;
        _ -> ?throw_error(?ERR_INVALID_USERNAME, [Username])
    end.


%%--------------------------------------------------------------------
%% @doc Validates user password. It must not be empty.
%% @end
%%--------------------------------------------------------------------
-spec validate_password(Password :: password()) -> ok | no_return().
validate_password(Password) when size(Password) < ?PASSWORD_MIN_LENGTH ->
    ?throw_error(?ERR_INVALID_PASSWORD, [<<>>]);
validate_password(Password) ->
    case binary:match(Password, ?PASSWORD_FORBIDDEN_REGEXP) of
        nomatch -> ok;
        _ -> ?throw_error(?ERR_INVALID_PASSWORD)
    end.


%%--------------------------------------------------------------------
%% @doc Validates user role. It must be one of 'admin', 'regular'.
%% @end
%%--------------------------------------------------------------------
-spec validate_role(Role :: term()) -> ok | no_return().
validate_role(admin) -> ok;
validate_role(regular) -> ok;
validate_role(Role) -> ?throw_error(?ERR_INVALID_ROLE, [Role]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Validates user credentials.
%% @end
%%--------------------------------------------------------------------
-spec validate_credentials(Username :: name(), Password :: password(), Role :: role()) -> ok.
validate_credentials(Username, Password, Role) ->
    ?MODULE:validate_username(Username),
    ?MODULE:validate_password(Password),
    ?MODULE:validate_role(Role).

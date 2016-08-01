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

%% Model behaviour callbacks
-export([get_fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1,
    list/0]).

%% API
-export([new/3, authenticate/2, change_password/2, get_by_role/1]).
-export([validate_username/1, validate_password/1, validate_role/1]).

-type name() :: binary().
-type password() :: binary().
-type password_hash() :: binary().
-type role() :: admin | regular.
-type uuid() :: binary().

-export_type([name/0, password/0, password_hash/0, role/0, uuid/0]).

-define(USERNAME_REGEX, <<"^[a-zA-Z0-9]{", (erlang:integer_to_binary(
    onepanel_env:get(min_username_length)))/binary, ",}$">>).
-define(PASSWORD_REGEX, <<"^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])[^:]{",
    (erlang:integer_to_binary(onepanel_env:get(min_password_length)))/binary,
    ",}$">>).

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
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: model_behaviour:record()) ->
    ok | #error{} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Record :: model_behaviour:record()) -> ok | no_return().
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
    {ok, Record :: model_behaviour:record()} | #error{} | no_return().
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
%% @doc Validates credentials and creates user account.
%% @end
%%--------------------------------------------------------------------
-spec new(Username :: name(), Password :: password(), Role :: role()) ->
    ok | no_return().
new(Username, Password, Role) ->
    ?MODULE:validate_username(Username),
    ?MODULE:validate_password(Password),
    ?MODULE:validate_role(Role),
    WorkFactor = onepanel_env:get(becrypt_work_factor),
    case ?MODULE:create(#onepanel_user{
        username = Username, role = Role, uuid = onepanel_utils:gen_uuid(),
        password_hash = onepanel_user_nif:hash_password(Password, WorkFactor)
    }) of
        ok -> ok;
        _ -> ?throw(?ERR_USERNAME_NOT_AVAILABLE)
    end.


%%--------------------------------------------------------------------
%% @doc Authenticates user by checking provided hash against the one stored in
%% user model instance.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Username :: name(), Password :: password()) ->
    {ok, User :: #onepanel_user{}} | #error{}.
authenticate(Username, Password) ->
    case onepanel_user:get(Username) of
        {ok, #onepanel_user{password_hash = Hash} = User} ->
            case onepanel_user_nif:check_password(Password, Hash) of
                true -> {ok, User};
                false -> ?error(?ERR_INVALID_USERNAME_OR_PASSWORD)
            end;
        _ -> ?error(?ERR_INVALID_USERNAME_OR_PASSWORD)
    end.


%%--------------------------------------------------------------------
%% @doc Validates and updates user password.
%% @end
%%--------------------------------------------------------------------
-spec change_password(Username :: name(), NewPassword :: password()) ->
    ok | no_return().
change_password(Username, NewPassword) ->
    ?MODULE:validate_password(NewPassword),
    WorkFactor = onepanel_env:get(becrypt_work_factor),
    ?MODULE:update(Username, #{
        password_hash => onepanel_user_nif:hash_password(NewPassword, WorkFactor)
    }).


%%--------------------------------------------------------------------
%% @doc Returns a list of users with the specified role.
%% @end
%%--------------------------------------------------------------------
-spec get_by_role(Role :: role()) -> Users :: [#onepanel_user{}] | no_return().
get_by_role(Role) ->
    model:select(?MODULE, [{#onepanel_user{role = Role, _ = '_'}, [], ['$_']}]).


%%--------------------------------------------------------------------
%% @doc Validates username. It must be at least 'min_username_length'
%% characters long and contain only alphanumeric characters [a-zA-Z0-9].
%% @end
%%--------------------------------------------------------------------
-spec validate_username(Username :: name()) -> ok | no_return().
validate_username(Username) ->
    case re:run(Username, ?USERNAME_REGEX) of
        {match, _} -> ok;
        nomatch -> ?throw(?ERR_INVALID_USERNAME)
    end.


%%--------------------------------------------------------------------
%% @doc Validates user password. It must be at least 'min_password_length'
%% characters long and contain a minimum of 1 lower case letter [a-z]
%% and a minimum of 1 upper case letter [A-Z] and a minimum of 1 numeric
%% character [0-9]. Password must not contain a colon character [:].
%% @end
%%--------------------------------------------------------------------
-spec validate_password(Password :: password()) -> ok | no_return().
validate_password(Password) ->
    case re:run(Password, ?PASSWORD_REGEX) of
        {match, _} -> ok;
        nomatch -> ?throw(?ERR_INVALID_PASSWORD)
    end.


%%--------------------------------------------------------------------
%% @doc Validates user role. It must be one of 'admin', 'regular'.
%% @end
%%--------------------------------------------------------------------
-spec validate_role(Role :: term()) -> ok | no_return().
validate_role(admin) -> ok;
validate_role(regular) -> ok;
validate_role(_) -> ?throw(?ERR_INVALID_ROLE).
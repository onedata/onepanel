%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains user management functions. It allows
%% to authenticate user and change his password.
%% @end
%% ===================================================================
-module(user_logic).

-include("registered_names.hrl").
-include("gui_modules/errors.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([hash_password/2, check_password/2]).
-export([create_user/2, create_user/3, change_username/2, authenticate/2,
    change_password/2, change_password/4, delete_default_user/0]).

-on_load(init/0).

-define(UUID_LEN, 32).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/0
%% ====================================================================
%% @doc Initializes NIF library.
%% @end
-spec init() -> ok | no_return().
%% ====================================================================
init() ->
    {ok, NifPrefix} = application:get_env(?APP_NAME, nif_prefix_dir),
    ok = erlang:load_nif(filename:join(NifPrefix, "user_logic_drv"), 0).


%% hash_password/2
%% ====================================================================
%% @doc Call underlying NIF function. Hashes password using bcrypt
%% algorithm with custom work factor. Can throw an exception if nif
%% was not properly loaded.
%% @end
-spec hash_password(Password :: string(), WorkFactor :: integer()) -> Result when
    Result :: string() | no_return().
%% ====================================================================
hash_password(_, _) ->
    throw("NIF library not loaded.").


%% check_password/2
%% ====================================================================
%% @doc Call underlying NIF function. Checks whether password matches
%% hash. Returns 0 in case of successful match and 1 otherwise.
%% Can throw an exception if nif was not properly loaded.
%% @end
-spec check_password(Password :: string(), Hash :: string()) -> Result when
    Result :: 0 | 1 | no_return().
%% ====================================================================
check_password(_, _) ->
    throw("NIF library not loaded.").


%% create_user/2
%% ====================================================================
%% @doc Creates user in database.
%% @end
-spec create_user(Username :: binary(), Password :: binary()) ->
    ok | {error, Reason :: term()}.
%% ====================================================================
create_user(Username, Password) ->
    create_user(Username, Password, admin).


%% create_user/2
%% ====================================================================
%% @doc Creates user in database.
%% @end
-spec create_user(Username :: binary(), Password :: binary(),
    Role :: admin | regular) -> ok | {error, Reason :: term()}.
%% ====================================================================
create_user(Username, Password, Role) ->
    case {verify_username(Username), verify_password(Password)} of
        {Username, Password} ->
            try
                Transaction = fun() ->
                    {error, <<"Record not found.">>} = dao:get_record(?USER_TABLE, Username),
                    {ok, WorkFactor} = application:get_env(?APP_NAME, bcrypt_work_factor),
                    PasswordHash = hash_password(Password, WorkFactor),
                    ok = dao:save_record(?USER_TABLE, #?USER_RECORD{username = Username,
                        password_hash = list_to_binary(PasswordHash), role = Role,
                        uuid = http_utils:base64url_encode(crypto:rand_bytes(?UUID_LEN))})
                end,
                mnesia:activity(transaction, Transaction)
            catch
                _:{aborted, {{badmatch, {ok, #?USER_RECORD{}}}, _}} ->
                    {error, <<"Username is not available">>};
                _:Reason ->
                    ?error_stacktrace("Cannot create user ~p: ~p", [Username, Reason]),
                    {error, Reason}
            end;
        {{error, Reason}, _} -> {error, Reason};
        {_, {error, Reason}} -> {error, Reason}
    end.


%% authenticate/2
%% ====================================================================
%% @doc Check whether user exists and whether it is a valid password
%% @end
-spec authenticate(Username :: binary(), Password :: binary()) -> Result when
    Result :: {ok, User :: #?USER_RECORD{}} | {error, ErrorId :: binary()}.
%% ====================================================================
authenticate(Username, Password) ->
    case dao:get_record(?USER_TABLE, Username) of
        {ok, #?USER_RECORD{username = Username, password_hash = PasswordHash} = User} ->
            try
                true = check_password(Password, PasswordHash),
                {ok, User}
            catch
                _:{badmatch, false} ->
                    {error, ?AUTHENTICATION_ERROR};
                _:Reason ->
                    ?error_stacktrace("Cannot authenticate user: ~p", [Reason]),
                    {error, ?INTERNAL_SERVER_ERROR}
            end;
        {error, <<"Record not found.">>} -> {error, ?AUTHENTICATION_ERROR};
        Other ->
            ?error("Cannot authenticate user: ~p", [Other]),
            {error, ?INTERNAL_SERVER_ERROR}
    end.


%% change_username/2
%% ====================================================================
%% @doc Changes user's name in database.
%% @end
-spec change_username(Username :: binary(), NewUsername :: binary()) -> Result when
    Result :: ok | {error, Reason :: binary()}.
%% ====================================================================
change_username(Username, Username) ->
    ok;

change_username(Username, NewUsername) ->
    case verify_username(NewUsername) of
        NewUsername ->
            try
                Transaction = fun() ->
                    {error, <<"Record not found.">>} = dao:get_record(?USER_TABLE, NewUsername),
                    {ok, User} = dao:get_record(?USER_TABLE, Username),
                    ok = dao:delete_record(?USER_TABLE, Username),
                    ok = dao:save_record(?USER_TABLE, User#?USER_RECORD{username = NewUsername})
                end,
                mnesia:activity(transaction, Transaction)
            catch
                _:{aborted, {{badmatch, {ok, #?USER_RECORD{}}}, _}} ->
                    {error, <<"Username is not available">>};
                error:{badmatch, {error, Reason}} when is_binary(Reason) ->
                    {error, Reason};
                _:Reason ->
                    ?error_stacktrace("Cannot change name of user ~p: ~p", [Username, Reason]),
                    {error, <<"Internal server error">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% change_password/3
%% ====================================================================
%% @doc Changes user's password if authenticated
%% @end
-spec change_password(Username :: binary(), CurrentPassword :: binary(),
    NewPassword :: binary(), ConfirmedNewPassword :: binary()) -> Result
    when Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Username, CurrentPassword, NewPassword, NewPassword) ->
    case authenticate(Username, CurrentPassword) of
        {ok, _} ->
            change_password(Username, NewPassword);
        {error, ?AUTHENTICATION_ERROR} ->
            {error, <<"Invalid username or password.">>};
        _ ->
            {error, <<"Internal server error">>}
    end;

change_password(_, _, _, _) ->
    {error, <<"Passwords do not match.">>}.


%% change_password/2
%% ====================================================================
%% @doc Changes user's password.
%% @end
-spec change_password(Username :: binary(), NewPassword :: binary()) -> Result
    when Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Username, NewPassword) ->
    case verify_password(NewPassword) of
        NewPassword ->
            try
                {ok, WorkFactor} = application:get_env(?APP_NAME, bcrypt_work_factor),
                PasswordHash = hash_password(binary_to_list(NewPassword), WorkFactor),
                ok = dao:update_record(?USER_TABLE, Username, [{password_hash, PasswordHash}]),
                ok = gen_server:call(?ONEPANEL_SERVER, {set_password, Username, NewPassword})
            catch
                error:{badmatch, {error, Reason}} when is_binary(Reason) ->
                    {error, Reason};
                _:Reason ->
                    ?error_stacktrace("Cannot change password for user ~p: ~p", [Username, Reason]),
                    {error, <<"Internal server error">>}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Verifies user name.
%% @end
%%--------------------------------------------------------------------
-spec verify_username(Username :: binary()) -> ok | {error, Reason :: term()}.
verify_username(<<>>) ->
    {error, empty_username};
verify_username(Username) ->
    try
        check_pattern(check_nonempty(Username), <<":">>)
    catch
        throw:Reason -> {error, <<"Username validation error: ", Reason/binary>>}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Verifies user password.
%% @end
%%--------------------------------------------------------------------
-spec verify_password(Username :: binary()) -> ok | {error, Reason :: term()}.
verify_password(Password) ->
    {ok, MinLength} = application:get_env(?APP_NAME, min_user_password_length),
    try
        check_pattern(check_length(Password, MinLength), <<":">>)
    catch
        throw:Reason -> {error, <<"Password validation error: ", Reason/binary>>}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks whether provided string is nonempty.
%% @end
%%--------------------------------------------------------------------
-spec check_nonempty(Binary :: binary()) -> Binary :: binary() | no_return().
check_nonempty(<<>>) ->
    throw(<<"must be nonempty">>);

check_nonempty(Binary) ->
    Binary.


%%--------------------------------------------------------------------
%% @doc
%% Checks whether provided string contains given pattern.
%% @end
%%--------------------------------------------------------------------
-spec check_pattern(Binary :: binary(), Pattern :: binary()) ->
    Binary :: binary() | no_return().
check_pattern(Binary, Pattern) ->
    case binary:matches(Binary, Pattern) of
        [] -> Binary;
        _ -> throw(<<"must not contain '", Pattern/binary, "' character">>)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Checks whether provided string satisfies minimum length.
%% @end
%%--------------------------------------------------------------------
-spec check_length(Binary :: binary(), MinLength :: non_neg_integer()) ->
    Binary :: binary() | no_return().
check_length(Binary, MinLength) ->
    case size(Binary) < MinLength of
        true -> throw(<<"must contain at least ",
            (integer_to_binary(MinLength))/binary, " characters">>);
        false -> Binary
    end.


%%--------------------------------------------------------------------
%% @doc
%% Removes default user.
%% @end
%%--------------------------------------------------------------------
-spec delete_default_user() -> ok | {error, Reason :: term()}.
delete_default_user() ->
    {ok, DefaultUsername} = application:get_env(?APP_NAME, default_username),
    dao:delete_record(?USER_TABLE, DefaultUsername).
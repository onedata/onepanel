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
-export([create_user/2, change_username/2, authenticate/2, change_password/4]).

-on_load(init/0).

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
-spec create_user(Username :: binary(), Password :: binary()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
create_user(Username, Password) ->
    try
        Transaction = fun() ->
            {error, <<"Record not found.">>} = dao:get_record(?USER_TABLE, Username),
            {ok, WorkFactor} = application:get_env(?APP_NAME, bcrypt_work_factor),
            PasswordHash = hash_password(binary_to_list(Password), WorkFactor),
            ok = dao:save_record(?USER_TABLE, #?USER_RECORD{username = Username, password_hash = PasswordHash})
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            ?error("Cannot create user ~p: ~p", [Username, Reason]),
            {error, Reason}
    end.


%% authenticate/2
%% ====================================================================
%% @doc Check whether user exists and whether it is a valid password
%% @end
-spec authenticate(Username :: binary(), Password :: binary()) -> Result when
    Result :: ok | {error, ErrorId :: binary()}.
%% ====================================================================
authenticate(Username, Password) ->
    case dao:get_record(?USER_TABLE, Username) of
        {ok, #?USER_RECORD{username = Username, password_hash = PasswordHash}} ->
            try
                0 = check_password(binary_to_list(Password), PasswordHash),
                ok
            catch
                _:{badmatch, 1} ->
                    {error, ?AUTHENTICATION_ERROR};
                _:Reason ->
                    ?error("Cannot authenticate user: ~p", [Reason]),
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
change_username(_, <<>>) ->
    {error, <<"Username cannot be empty.">>};

change_username(Username, Username) ->
    ok;

change_username(Username, NewUsername) ->
    try
        Transaction = fun() ->
            {error, <<"Record not found.">>} = dao:get_record(?USER_TABLE, NewUsername),
            {ok, User} = dao:get_record(?USER_TABLE, Username),
            ok = dao:delete_record(?USER_TABLE, Username),
            ok = dao:save_record(?USER_TABLE, User#?USER_RECORD{username = NewUsername}),
            {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
            ok = installer_db:change_username(Dbs, Username, NewUsername)
        end,
        mnesia:activity(transaction, Transaction)
    catch
        error:{badmatch, {ok, Record}} when is_record(Record, ?USER_RECORD) ->
            {error, <<"Username is not available.">>};
        error:{badmatch, {error, Reason}} when is_binary(Reason) ->
            {error, Reason};
        _:Reason ->
            ?error("Cannot change name of user ~p: ~p", [Username, Reason]),
            {error, <<"Internal server error.">>}
    end.


%% change_password/3
%% ====================================================================
%% @doc Changes user's password if authenticated
%% @end
-spec change_password(Username :: binary(), CurrentPassword :: binary(), NewPassword :: binary(), ConfirmedNewPassword :: binary()) -> Result
    when Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Username, CurrentPassword, NewPassword, NewPassword) ->
    {ok, MinimumPasswordLength} = application:get_env(?APP_NAME, min_user_password_length),
    case size(NewPassword) < MinimumPasswordLength of
        true ->
            {error, <<"Password should be at least ", (integer_to_binary(MinimumPasswordLength))/binary, " characters long.">>};
        _ ->
            case authenticate(Username, CurrentPassword) of
                ok ->
                    try
                        {ok, WorkFactor} = application:get_env(?APP_NAME, bcrypt_work_factor),
                        PasswordHash = hash_password(binary_to_list(NewPassword), WorkFactor),
                        ok = dao:update_record(?USER_TABLE, Username, [{password_hash, PasswordHash}]),
                        {ok, #?GLOBAL_CONFIG_RECORD{dbs = Dbs}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
                        ok = installer_db:change_password(Dbs, Username, CurrentPassword, NewPassword),
                        ok = gen_server:call(?ONEPANEL_SERVER, {set_password, Username, NewPassword})
                    catch
                        error:{badmatch, {error, Reason}} when is_binary(Reason) ->
                            {error, Reason};
                        _:Reason ->
                            ?error("Cannot change password for user ~p: ~p", [Username, Reason]),
                            {error, <<"Internal server error">>}
                    end;
                {error, ?AUTHENTICATION_ERROR} ->
                    {error, <<"Invalid username or password.">>};
                _ ->
                    {error, <<"Internal server error">>}
            end
    end;

change_password(_, _, _, _) ->
    {error, <<"Passwords do not match.">>}.
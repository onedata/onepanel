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
-include("gui_modules/common.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% Length of salt added to user password
-define(SALT_LENGTH, 10).

%% API
-export([create_user/2, change_username/2, authenticate/2, change_password/4]).

%% ====================================================================
%% API functions
%% ====================================================================

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
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, PasswordHash} = bcrypt:hashpw(Password, Salt),
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
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
authenticate(Username, Password) ->
    case dao:get_record(?USER_TABLE, Username) of
        {ok, #?USER_RECORD{username = Username, password_hash = ValidPasswordHash}} ->
            {ok, PasswordHash} = bcrypt:hashpw(Password, ValidPasswordHash),
            case PasswordHash of
                ValidPasswordHash -> ok;
                _ -> {error, ?AUTHENTICATION_ERROR}
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
change_password(_, _, NewPassword, NewPassword) when size(NewPassword) < ?MIN_PASSWORD_LENGTH ->
    {error, <<"Password should be at least ", (integer_to_binary(?MIN_PASSWORD_LENGTH))/binary, " characters long.">>};

change_password(Username, CurrentPassword, NewPassword, NewPassword) ->
    case authenticate(Username, CurrentPassword) of
        ok ->
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, PasswordHash} = bcrypt:hashpw(NewPassword, Salt),
            try
                ok = dao:update_record(?USER_TABLE, Username, [{password_hash, PasswordHash}, {salt, Salt}]),
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
    end;

change_password(_, _, _, _) ->
    {error, <<"Passwords do not match.">>}.
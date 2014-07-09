%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains user management functions. It allows
%% to authenticate user and change his password.
%% @end
%% ===================================================================
-module(user_logic).

-include("gui_modules/common.hrl").
-include("onepanel_modules/user_logic.erl").
-include_lib("ctool/include/logging.hrl").

%% Length of salt added to user password
-define(SALT_LENGTH, 10).

%% API
-export([create_user/2, hash_password/1, authenticate/2, change_password/3]).

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
        Salt = list_to_binary(onepanel_utils:random_ascii_lowercase_sequence(?SALT_LENGTH)),
        PasswordHash = user_logic:hash_password(<<Password/binary, Salt/binary>>),
        ok = dao:save_record(?USER_TABLE, #?USER_RECORD{username = Username, hash = PasswordHash, salt = Salt})
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
        {ok, #?USER_RECORD{username = Username, hash = ValidPasswordHash, salt = Salt}} ->
            PasswordHash = hash_password(<<Password/binary, Salt/binary>>),
            case ValidPasswordHash of
                PasswordHash -> ok;
                _ -> {error, ?AUTHENTICATION_ERROR}
            end;
        {error, "Record not found."} -> {error, ?AUTHENTICATION_ERROR};
        Other ->
            ?error("Cannot authenticate user: ~p", [Other]),
            {error, ?INTERNAL_SERVER_ERROR}
    end.


%% change_password/3
%% ====================================================================
%% @doc Changes user's password if authenticated
%% @end
-spec change_password(Username :: binary(), OldPassword :: binary(), NewPassword :: binary()) -> Result
    when Result :: ok | {error, Reason :: term()}.
%% ====================================================================
change_password(Username, OldPassword, NewPassword) ->
    case authenticate(Username, OldPassword) of
        ok ->
            case create_user(Username, NewPassword) of
                ok -> ok;
                Other ->
                    ?error("Cannot change user password: ~p", [Other]),
                    {error, ?INTERNAL_SERVER_ERROR}
            end;
        _ -> {error, ?AUTHENTICATION_ERROR}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% hash_password/1
%% ====================================================================
%% @doc Returns sha512 hash of given password.
%% @end
-spec hash_password(Password :: binary()) -> Result when
    Result :: binary().
%% ====================================================================
hash_password(Password) ->
    Hash = crypto:hash_update(crypto:hash_init(sha512), Password),
    integer_to_binary(binary:decode_unsigned(crypto:hash_final(Hash)), 16).
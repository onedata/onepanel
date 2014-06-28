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

-include("onepanel_modules/db_logic.hrl").
-include("onepanel_modules/install_logic.hrl").
-include("registered_names.hrl").

%% API
-export([hash_password/1, authenticate/2, change_password/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%% authenticate/2
%% ====================================================================
%% @doc Check whether user exists and whether it is a valid password
%% @end
-spec authenticate(Username :: string(), Password :: string()) -> Result when
    Result :: ok | {error, Reason :: string()}.
%% ====================================================================
authenticate(Username, Password) ->
    PasswordHash = hash_password(Password),
    case dao:get_record(?USER_TABLE, Username) of
        {ok, #?USER_RECORD{username = Username, password = ValidPasswordHash}} ->
            case ValidPasswordHash of
                PasswordHash -> ok;
                _ -> {error, "Invaild username or password."}
            end;
        {error, not_found} -> {error, "Invaild username or password."};
        Other ->
            lager:error("Cannot authenticate user: ~p", [Other]),
            {error, "Internal server error."}
    end.


%% change_password/3
%% ====================================================================
%% @doc Changes user's password if authenticated
%% @end
-spec change_password(Username :: string(), OldPassword :: string(), NewPassword :: string()) -> ok | {error, Reason :: string()}.
%% ====================================================================
change_password(Username, OldPassword, NewPassword) ->
    PasswordHash = user_logic:hash_password(NewPassword),
    case authenticate(Username, OldPassword) of
        ok ->
            case dao:save_record(?USER_TABLE, #?USER_RECORD{username = Username, password = PasswordHash}) of
                ok -> ok;
                Other ->
                    lager:error("Cannot change user password: ~p", [Other]),
                    {error, "Internal server error."}
            end;
        _ -> {error, "Invaild username or password."}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% hash_password/1
%% ====================================================================
%% @doc Returns md5 hash of given password.
%% @end
-spec hash_password(Password :: string()) -> Result when
    Result :: string().
%% ====================================================================
hash_password(Password) ->
    Hash = crypto:hash_update(crypto:hash_init(md5), Password),
    integer_to_list(binary:decode_unsigned(crypto:hash_final(Hash)), 16).
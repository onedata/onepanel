%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database management functions
%% @end
%% ===================================================================
-module(user_logic).

-include("spanel_modules/db.hrl").
-include("spanel_modules/errors.hrl").

%% API
-export([hash_password/1, authenticate/2, change_password/3]).

%% hash_password/1
%% ====================================================================
%% @doc Returns md5 password hash
%% @end
-spec hash_password(Password :: string()) -> PasswordHash :: string().
%% ====================================================================
hash_password(Password) ->
  Hash = crypto:hash_update(crypto:hash_init(md5), Password),
  integer_to_list(binary:decode_unsigned(crypto:hash_final(Hash)), 16).


%% authenticate/2
%% ====================================================================
%% @doc Check whether user exists and whether it is a valid password
%% @end
-spec authenticate(Username :: string(), Password :: string()) -> ok | {error, Reason :: string()}.
%% ====================================================================
authenticate(Username, Password) ->
  PasswordHash = hash_password(Password),
  case dao:get_record(users, Username) of
    {ok, #user{username = Username, password = PasswordHash}} -> ok;
    _ -> {error, ?AUTHENTICATION_ERROR}
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
      case dao:save_record(users, #user{username = Username, password = PasswordHash}) of
        ok -> ok;
        _ -> {error, ?INTERNAL_ERROR}
      end;
    _ -> {error, ?AUTHENTICATION_ERROR}
  end.

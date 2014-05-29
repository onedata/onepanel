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
-include("registered_names.hrl").
-include("spanel_modules/install.hrl").

%% API
-export([hash_password/1, authenticate/2, change_password/3, register_in_global_registry/0]).

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

%% register_in_global_registry/0
%% ====================================================================
%% @doc Registers provider in global registry.
%% @end
-spec register_in_global_registry() -> ok | error.
%% ====================================================================
register_in_global_registry() ->
  try
    {ok, Dir} = application:get_env(?APP_NAME, grp_dir),
    {ok, KeyName} = application:get_env(?APP_NAME, grpkey_name),
    {ok, KeyPath} = application:get_env(?APP_NAME, grpkey_file),
    {ok, CsrPath} = application:get_env(?APP_NAME, grpcsr_file),
    {ok, CertName} = application:get_env(?APP_NAME, grpcert_name),
    {ok, CertPath} = application:get_env(?APP_NAME, grpcert_file),
    {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
    Path = ?DEFAULT_NODES_INSTALL_PATH ++ ?DEFAULT_WORKER_NAME ++ "/certs",
    file:make_dir(Dir),
    0 = pkcs10:create_csr("", KeyPath, CsrPath),
    {ok, Csr} = file:read_file(CsrPath),
    {ok, Key} = file:read_file(KeyPath),
    case install_utils:save_file_on_hosts(Path, KeyName, Key) of
      ok -> ok;
      {error, ErrorHosts} ->
        lager:error("Failed to save private key on following hosts: ~p", [ErrorHosts]),
        throw(error)
    end,
    Urls = "[" ++ string:join(lists:map(fun(Host) -> "\"" ++ Host ++ "\"" end, install_utils:get_hosts()), ", ") ++ "]",
    Body = "{\"urls\" : " ++ Urls ++ ", \"csr\" : \"" ++ binary_to_list(Csr) ++ "\", \"redirectionPoint\" : \"https://127.0.0.1:8080\"}",
    {ok, "200", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url ++ "/provider", [{content_type, "application/json"}], post, Body),
    List = mochijson2:decode(ResponseBody, [{format, proplist}]),
    Cert = proplists:get_value(<<"certificate">>, List),
    case Cert of
      undefined -> throw(error);
      _ ->
        ok = file:write_file(CertPath, Cert),
        case install_utils:save_file_on_hosts(Path, CertName, Cert) of
          ok -> ok;
          {error, FailedHosts} ->
            lager:error("Failed to save certificate on following hosts: ~p", [FailedHosts]),
            throw(error)
        end
    end,
    ok
  catch
    _:_ -> error
  end.
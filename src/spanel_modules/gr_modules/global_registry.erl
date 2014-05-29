%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains global registry interaction functions
%% @end
%% ===================================================================
-module(global_registry).

-include("registered_names.hrl").
-include("spanel_modules/install.hrl").

%% API
-export([register/0, check_ip_address/0]).

%% register_in_global_registry/0
%% ====================================================================
%% @doc Registers provider in global registry.
%% @end
-spec register() -> ok | error.
%% ====================================================================
register() ->
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

%% check_ip_address/0
%% ====================================================================
%% @doc Returns ip address that is visible for global registry
%% @end
-spec check_ip_address() -> IpAddress :: string() | undefined.
%% ====================================================================
check_ip_address() ->
  try
    {ok, Url} = application:get_env(?APP_NAME, global_registry_url),
    {ok, "200", _ResponseHeaders, ResponseBody} = ibrowse:send_req(Url ++ "/provider/test/check_my_ip", [{content_type, "application/json"}], get),
    binary_to_list(mochijson2:decode(ResponseBody))
  catch
    _:_ -> undefined
  end.
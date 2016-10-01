%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an adapter for the openssl command.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_ssl).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([gen_rsa/1, gen_csr/2, exec/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generates private key.
%% @end
%%--------------------------------------------------------------------
-spec gen_rsa(Length :: non_neg_integer()) -> Key :: binary() | no_return().
gen_rsa(Length) ->
    KeyFile = onepanel_shell:mktemp(),
    exec("genrsa", ["-out", KeyFile, Length]),
    {ok, Key} = file:read_file(KeyFile),
    file:delete(KeyFile),
    Key.


%%--------------------------------------------------------------------
%% @doc Generates certificate signing request using provided private key.
%% @end
%%--------------------------------------------------------------------
-spec gen_csr(Subject :: string(), Key :: binary()) -> Csr :: binary() | no_return().
gen_csr(Subject, Key) ->
    KeyFile = onepanel_shell:mktemp(),
    CsrFile = onepanel_shell:mktemp(),
    file:write_file(KeyFile, Key),
    exec("req", ["-subj", "/CN=" ++ Subject, "-new", "-key", KeyFile, "-out", CsrFile]),
    {ok, Csr} = file:read_file(CsrFile),
    file:delete(KeyFile),
    file:delete(CsrFile),
    Csr.


%%--------------------------------------------------------------------
%% @doc Executes openssl command.
%% @end
%%--------------------------------------------------------------------
-spec exec(Cmd :: string(), Args :: [term()]) ->
    Result :: string() | no_return().
exec(Cmd, Args) ->
    OpenSSL = get_executable("openssl"),
    onepanel_shell:check_output([OpenSSL, Cmd | Args]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns path to the provided executable. Throws an exception
%% if the executable is not found.
%% @end
%%--------------------------------------------------------------------
-spec get_executable(Name :: string()) -> Path :: string() | no_return().
get_executable(Name) ->
    case os:find_executable(Name) of
        false -> ?make_error({Name, not_found});
        Path -> Path
    end.
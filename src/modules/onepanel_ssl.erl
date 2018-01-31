%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an adapter for the openssl command.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_ssl).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type pem() :: binary().

-export_type([pem/0]).

%% API
-export([generate_csr_and_key/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generates Certificate Signing Request and matching private key.
%% @end
%%--------------------------------------------------------------------
-spec generate_csr_and_key(Domain :: string) ->
    {ok, CSR :: pem(), Key :: pem()} | error.
generate_csr_and_key(Domain) ->
    KeyFile = onepanel_shell:mktemp(),
    CSRFile = onepanel_shell:mktemp(),

    try
        os:cmd([
            "openssl req -new -batch -subj '/CN=", Domain, "/'",
            " -nodes", % no password on keyfile
            " -keyout ", KeyFile,
            " -out ", CSRFile]),

        {ok, KeyPem} = file:read_file(KeyFile),
        {ok, CSRPem} = file:read_file(CSRFile),

        % Check expected result
        case {public_key:pem_decode(CSRPem), public_key:pem_decode(KeyPem)} of
            {[{'CertificationRequest', _CSRDer, not_encrypted}],
                [{'PrivateKeyInfo', _KeyDer, not_encrypted}]} ->
                {ok, CSRPem, KeyPem};
            _ ->
                ?error("Error generating CSR for Let's Encrypt."),
                error
        end
    after
        file:delete(KeyFile),
        file:delete(CSRFile)
    end.

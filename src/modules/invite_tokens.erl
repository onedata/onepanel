%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Utilities for managing invite tokens.
%%% @end
%%%--------------------------------------------------------------------
-module(invite_tokens).
-author("Bartosz Walkowicz").

-include("authentication.hrl").

-type invite_token() :: binary().

-export_type([invite_token/0]).

%% API
-export([create/0, get_nonce/1, get_hostname/1]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec create() -> {ok, binary()} | {error, term()}.
create() ->
    case authorization_nonce:create() of
        {ok, Nonce} ->
            SelfHost = hosts:self(),
            Token = onepanel_utils:join(
                [?ONEPANEL_INVITE_TOKEN_PREFIX, Nonce, SelfHost],
                <<?ONEPANEL_TOKEN_SEPARATOR>>
            ),
            {ok, Token};
        {error, _} = Error ->
            Error
    end.


-spec get_nonce(invite_token()) -> authorization_nonce:nonce().
get_nonce(InviteToken) ->
    [<<?ONEPANEL_INVITE_TOKEN_PREFIX>>, Nonce, _Hostname] =
        string:split(InviteToken, ?ONEPANEL_TOKEN_SEPARATOR, all),
    Nonce.


-spec get_hostname(invite_token()) -> binary().
get_hostname(InviteToken) ->
    [<<?ONEPANEL_INVITE_TOKEN_PREFIX>>, _Nonce, Hostname] =
        string:split(InviteToken, ?ONEPANEL_TOKEN_SEPARATOR, all),
    Hostname.

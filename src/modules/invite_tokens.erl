%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Utilities for managing invite tokens, that is tokens used to join
%%% existing cluster.
%%% @end
%%%--------------------------------------------------------------------
-module(invite_tokens).
-author("Bartosz Walkowicz").

-include("authentication.hrl").

-type invite_token() :: binary().

-export_type([invite_token/0]).

%% API
-export([create/0, get_nonce/1, get_cluster_host/1]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec create() -> {ok, binary()} | {error, term()}.
create() ->
    case authorization_nonce:create() of
        {ok, Nonce} ->
            Token = encode_invite_token(#{
                <<"nonce">> => Nonce,
                <<"clusterHost">> => list_to_binary(hosts:self())
            }),
            {ok, Token};
        {error, _} = Error ->
            Error
    end.


-spec get_nonce(invite_token()) -> authorization_nonce:nonce().
get_nonce(InviteToken) ->
    maps:get(<<"nonce">>, decode_invite_token(InviteToken)).


-spec get_cluster_host(invite_token()) -> service:host().
get_cluster_host(InviteToken) ->
    binary_to_list(maps:get(<<"clusterHost">>, decode_invite_token(InviteToken))).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec encode_invite_token(map()) -> invite_token().
encode_invite_token(Desc) ->
    onepanel_utils:join(
        [?ONEPANEL_INVITE_TOKEN_PREFIX, base64:encode(json_utils:encode(Desc))],
        <<?ONEPANEL_TOKEN_SEPARATOR>>
    ).


%% @private
-spec decode_invite_token(invite_token()) -> map().
decode_invite_token(InviteToken) ->
    [<<?ONEPANEL_INVITE_TOKEN_PREFIX>>, Desc] = string:split(
        InviteToken, ?ONEPANEL_TOKEN_SEPARATOR, all
    ),
    json_utils:decode(base64:decode(Desc)).

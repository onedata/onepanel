%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Utilities for managing node invite tokens, that is tokens used to join
%%% existing cluster.
%%% @end
%%%--------------------------------------------------------------------
-module(invite_tokens).
-author("Bartosz Walkowicz").

-include("authentication.hrl").
-include_lib("ctool/include/errors.hrl").

-type invite_token() :: binary().

-export_type([invite_token/0]).

%% API
-export([create/0, get_nonce/1, get_cluster_host/1]).


-type token_params() :: #{
    nonce := authorization_nonce:nonce(),
    cluster_host := service:host()
}.


%%%===================================================================
%%% API functions
%%%===================================================================


-spec create() -> {ok, binary()} | {error, term()}.
create() ->
    case authorization_nonce:create() of
        {ok, Nonce} ->
            Token = encode_invite_token(#{
                nonce => Nonce,
                cluster_host => hosts:self()
            }),
            {ok, Token};
        {error, _} = Error ->
            Error
    end.


-spec get_nonce(invite_token()) -> authorization_nonce:nonce().
get_nonce(InviteToken) ->
    maps:get(nonce, decode_invite_token(InviteToken)).


-spec get_cluster_host(invite_token()) -> service:host().
get_cluster_host(InviteToken) ->
    maps:get(cluster_host, decode_invite_token(InviteToken)).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec encode_invite_token(token_params()) -> invite_token().
encode_invite_token(#{nonce := Nonce, cluster_host := ClusterHost}) ->
    Payload = base64:encode(json_utils:encode(#{
        <<"nonce">> => Nonce,
        <<"clusterHost">> => list_to_binary(ClusterHost)
    })),
    onepanel_utils:join(
        [?ONEPANEL_INVITE_TOKEN_PREFIX, Payload],
        <<?ONEPANEL_TOKEN_SEPARATOR>>
    ).


%% @private
-spec decode_invite_token(invite_token()) -> token_params() | no_return().
decode_invite_token(InviteToken) ->
    try
        [<<?ONEPANEL_INVITE_TOKEN_PREFIX>>, Payload] = string:split(
            InviteToken, ?ONEPANEL_TOKEN_SEPARATOR, all
        ),
        json_utils:decode(base64:decode(Payload))
    of
        #{<<"nonce">> := Nonce, <<"clusterHost">> := ClusterHostBin} ->
            #{
                nonce => Nonce,
                cluster_host => binary_to_list(ClusterHostBin)
            };
        _ ->
            throw(?ERROR_TOKEN_INVALID)
    catch _:_ ->
        throw(?ERROR_TOKEN_INVALID)
    end.

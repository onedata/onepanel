%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles creation and manipulation of JSON Web Signature
%%% objects.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_jws).
-author("Wojciech Geisler").

-include_lib("public_key/include/OTP-PUB-KEY.hrl").
-include_lib("ctool/include/logging.hrl").

-export([load_key/1, generate_keys/2, sign/3, thumbprint/1, key_to_jwk_map/1]).

%%--------------------------------------------------------------------
%% @doc
%% Generates private and public key which can be used for JWS signing.
%% @end
%%--------------------------------------------------------------------
-spec generate_keys(file:name_all(), file:name_all()) -> ok.
generate_keys(PrivateKeyPath, PublicKeyPath) ->
    os:cmd(["openssl genrsa", " -out ", PrivateKeyPath]),
    os:cmd(["openssl rsa ", " -in ", PrivateKeyPath,  " -pubout ", " -out ", PublicKeyPath]),

    % Ensure success
    {ok, #'RSAPrivateKey'{}} = load_key(PrivateKeyPath),
    {ok, #'RSAPublicKey'{}} = load_key(PublicKeyPath),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Loads RSA key from given path.
%% @end
%%--------------------------------------------------------------------
-spec load_key(Path :: file:name_all()) ->
    {ok, #'RSAPrivateKey'{}}  | {ok, #'RSAPublicKey'{}} |
    {error, bad_key} | {error, term()}.
load_key(Path) ->
    case file:read_file(Path) of
        {ok, Pem} ->
            case public_key:pem_decode(Pem) of
                [{'RSAPrivateKey', _Der, not_encrypted} = Entry] ->
                    #'RSAPrivateKey'{} = Key = public_key:pem_entry_decode(Entry),
                    {ok, Key};
                [{'SubjectPublicKeyInfo', _Der, not_encrypted} = Entry] ->
                    #'RSAPublicKey'{} = Key = public_key:pem_entry_decode(Entry),
                    {ok, Key};
                _ ->
                    {error, bad_key}
            end;
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Translates OTP key representation to JWK standard.
%% @end
%%--------------------------------------------------------------------
-spec key_to_jwk_map(#'RSAPrivateKey'{} | #'RSAPublicKey'{}) -> map().
key_to_jwk_map(#'RSAPrivateKey'{publicExponent = E, modulus = N}) ->
    key_to_jwk_map(#'RSAPublicKey'{publicExponent = E, modulus = N});

key_to_jwk_map(#'RSAPublicKey'{publicExponent = E, modulus = N}) ->
    #{<<"e">> => base64url:encode(binary:encode_unsigned(E)),
      <<"kty">> => <<"RSA">>,
      <<"n">> => base64url:encode(binary:encode_unsigned(N))}.


%%--------------------------------------------------------------------
%% @doc
%% Creates JWS map signed with given key using algorithm given
%% in ProtectedHeader field `<<"alg">>'.
%% Currently only RS256 algorithm is supported. It will automatically set
%% if `<<"alg">>' field is omitted from ProtectedHeader.
%%
%% If payload is not a binary tries to encode it as JSON, may throw 
%% {error, {invalid_string, term()}}
%% @end
%%--------------------------------------------------------------------
-spec sign(Payload :: map() | binary(),
    ProtectedHeader :: map(), Key :: #'RSAPrivateKey'{}) ->
    {ok, map()} | {error, unsupported_alg} | no_return().
sign(Payload, ProtectedHeader, Key) when not(is_binary(Payload)) ->
    sign(json_utils:encode(Payload), ProtectedHeader, Key);

sign(Payload, #{<<"alg">> := <<"RS256">>} = ProtectedHeader,
    #'RSAPrivateKey'{} = Key) ->

    ProtectedB64 = base64url:encode(json_utils:encode(ProtectedHeader)),
    PayloadB64 = base64url:encode(Payload),

    Content = <<ProtectedB64/binary, $., PayloadB64/binary>>,
    Signature = public_key:sign(Content, sha256, Key),

    {ok, #{<<"protected">> => ProtectedB64,
           <<"payload">>   => PayloadB64,
           <<"signature">> => base64url:encode(Signature)}};

sign(_Payload, #{<<"alg">> := _}, _Key) ->
    {error, unsupported_alg};

sign(Payload, #{} = ProtectedHeader, #'RSAPrivateKey'{} = Key) ->
    sign(Payload, ProtectedHeader#{<<"alg">> => <<"RS256">>}, Key).


%%--------------------------------------------------------------------
%% @doc
%% Creates base64-encoded JWK thumbprint using sha256 algorithm.
%% See https://tools.ietf.org/html/draft-ietf-jose-jwk-thumbprint-08
%% @end
%%--------------------------------------------------------------------
-spec thumbprint(#'RSAPrivateKey'{} | #'RSAPublicKey'{}) -> binary().
thumbprint(Key) ->
    % jwk thumbprint requires JSON keys to be in alphabetical order
    KeyParams = lists:sort(maps:to_list(key_to_jwk_map(Key))),
    JSON = json_utils:encode({KeyParams}),
    base64url:encode(crypto:hash(sha256, JSON)).

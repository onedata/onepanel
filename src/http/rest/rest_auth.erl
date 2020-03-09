%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Module verifying authentication of REST requests.
%%% There are 3 methods of authentication:
%%% - Onezone-issued access token of a cluster member,
%%%   results in 'member' client role
%%% - Basic auth header with the emergency passphrase as its only content
%%%   or as the password for the virtual username "onepanel",
%%%   results in 'root' client role
%%% - Onepanel-issued token for a client using the emergency passphrase,
%%%   results in 'root' client role
%%% @end
%%%-------------------------------------------------------------------
-module(rest_auth).
-author("Wojciech Geisler").

-include("authentication.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/headers.hrl").

%% API
-export([authenticate/1, authenticate_by_basic_auth/1]).
-export([root_client/0, guest_client/0, peer_client/0]).

%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Authenticates user using any available methods.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Req :: cowboy_req:req()) -> {Result, cowboy_req:req()} when
    Result :: {true, #client{}} | {false, errors:error()}.
authenticate(Req) ->
    authenticate(Req, [
        fun authenticate_by_basic_auth/1,
        fun authenticate_by_onepanel_auth_token/1,
        fun authenticate_by_onezone_auth_token/1
    ]).


%%--------------------------------------------------------------------
%% @doc Authenticates user using basic authorization method.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | {error, _} | ignore.
authenticate_by_basic_auth(Req) ->
    case cowboy_req:header(?HDR_AUTHORIZATION, Req) of
        <<"Basic ", Base64/binary>> ->
            {check_basic_credentials(Base64), Req};
        _ ->
            {ignore, Req}
    end.


-spec root_client() -> middleware:client().
root_client() -> #client{
    role = root,
    zone_credentials = onezone_client:root_auth(),
    auth = aai:root_auth()
}.


-spec guest_client() -> middleware:client().
guest_client() -> #client{
    role = guest,
    auth = aai:nobody_auth()
}.


-spec peer_client() -> middleware:client().
peer_client() -> #client{
    role = peer,
    auth = aai:nobody_auth()
}.

%%%==================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Authenticates user using provided authorization methods.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Req :: cowboy_req:req(), Methods :: [fun()]) ->
    {Result, cowboy_req:req()} when
    Result :: {true, #client{}} | {false, errors:error()}.
authenticate(Req, []) ->
    {{true, guest_client()}, Req};
authenticate(Req, [AuthMethod | AuthMethods]) ->
    try AuthMethod(Req) of
        {#client{} = Client, Req2} ->
            {{true, Client}, Req2};
        {{error, _} = Error, Req2} ->
            {{false, Error}, Req2};
        {ignore, Req2} ->
            authenticate(Req2, AuthMethods)
    catch
        throw:Error ->
            {{false, Error}, Req};
        _:_ ->
            {{false, ?ERROR_UNAUTHORIZED}, Req}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Authenticates user using Onepanel-generated token used
%% for either sessions authenticated with the emergency passphrase or
%% peer nodes invited to join cluster.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_onepanel_auth_token(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | {error, _} | ignore.
authenticate_by_onepanel_auth_token(Req) ->
    case tokens:parse_access_token_header(Req) of
        <<?ONEPANEL_USER_AUTH_TOKEN_PREFIX, ?ONEPANEL_TOKEN_SEPARATOR, _/binary>> = OnepanelToken ->
            case onepanel_session:find_by_valid_auth_token(OnepanelToken) of
                {ok, #onepanel_session{username = ?LOCAL_SESSION_USERNAME}} ->
                    {root_client(), Req};
                error ->
                    {?ERROR_TOKEN_INVALID, Req}
            end;
        <<?ONEPANEL_INVITE_TOKEN_PREFIX, ?ONEPANEL_TOKEN_SEPARATOR, _/binary>> = InviteToken ->
            case authorization_nonce:verify(invite_tokens:get_nonce(InviteToken)) of
                true ->
                    {peer_client(), Req};
                false ->
                    {?ERROR_TOKEN_INVALID, Req}
            end;
        _ ->
            {ignore, Req}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Authenticates user using REST API token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_onezone_auth_token(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | {error, _} | ignore.
authenticate_by_onezone_auth_token(Req) ->
    case tokens:parse_access_token_header(Req) of
        undefined ->
            {ignore, Req};
        AccessToken ->
            PeerIp = resolve_peer_ip(Req),
            {onezone_tokens:authenticate_user(AccessToken, PeerIp), Req}
    end.


%% @private
-spec check_basic_credentials(Credentials :: binary() | [binary()]) ->
    #client{} | {error, _}.
check_basic_credentials(<<Base64/binary>>) ->
    Decoded = base64:decode(Base64),
    case check_emergency_passphrase(Decoded) of
        #client{} = Client ->
            % basic auth consisting solely of a valid passphrase is accepted
            Client;
        _Error ->
            case binary:split(Decoded, <<":">>) of
                [Decoded] ->
                    ?ERROR_BAD_BASIC_CREDENTIALS;
                [?LOCAL_USERNAME, Passphrase] ->
                    check_emergency_passphrase(Passphrase);
                [_Username, _Password] ->
                    ?ERROR_BAD_BASIC_CREDENTIALS
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Determines peer IP. Honours x-onedata-forwarded-for header
%% to retrieve original IP in case of Onedata proxy.
%% Note: proxy is not authenticated in any way, client connecting with
%% the proxy can present arbitrary IP by providing this header.
%% @end
%%--------------------------------------------------------------------
-spec resolve_peer_ip(cowboy_req:req()) -> inet:ip4_address().
resolve_peer_ip(Req) ->
    ForwarderFor = cowboy_req:header(?HDR_X_ONEDATA_FORWARDED_FOR, Req, undefined),
    case ip_utils:to_ip4_address(ForwarderFor) of
        {ok, Addr} ->
            Addr;
        _ ->
            {PeerIp, _Port} = cowboy_req:peer(Req),
            PeerIp
    end.


%% @private
-spec check_emergency_passphrase(Passphrase :: binary()) ->
    #client{} | {error, _}.
check_emergency_passphrase(Passphrase) ->
    case emergency_passphrase:verify(Passphrase) of
        true -> root_client();
        false -> ?ERROR_BAD_BASIC_CREDENTIALS
    end.

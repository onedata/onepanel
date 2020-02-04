%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Functions for handling tokens issued by Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(onezone_tokens).
-author("Wojciech Geisler").

-include("names.hrl").
-include("http/rest.hrl").
-include("authentication.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% API
-export([authenticate_user/2, authenticate_user/3]).
-export([read_domain/1]).

-define(USER_DETAILS_CACHE_KEY(Token, PeerIp), {user_details, {Token, PeerIp}}).
-define(USER_DETAILS_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl, ?APP_NAME, 0)).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Authenticates client as a user belonging to
%% the current cluster. Accepts either Onedata access token
%% or gui token, both issued by Onezone.
%% In the process obtains privileges held by the user in the current
%% cluster.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(tokens:serialized(), PeerIp :: ip_utils:ip()) ->
    #client{} | {error, _}.
authenticate_user(Token, PeerIp) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case authenticate_user(ClusterType, Token, PeerIp) of
        #client{} = Client -> Client;
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Reads Onezone domain from a Oneprovider registration token.
%% @end
%%--------------------------------------------------------------------
-spec read_domain(tokens:serialized()) -> Domain :: binary() | no_return().
read_domain(RegistrationToken) ->
    case tokens:deserialize(RegistrationToken) of
        {ok, Token} -> Token#token.onezone_domain;
        Error -> throw(?ERROR_BAD_VALUE_TOKEN(<<"token">>, Error))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc Authenticate user and obtain their info based on an access token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(
    onedata:cluster_type(), tokens:serialized(), PeerIp :: ip_utils:ip()
) -> #client{} | {error, _}.
authenticate_user(onezone, Token, PeerIp) ->
    case service_oz_worker:get_auth_by_token(Token, PeerIp) of
        {ok, ?USER(_) = Auth} ->
            {ok, Details} = service_oz_worker:get_user_details(Auth),
            user_details_to_client(Details, Auth, {rpc, Auth});
        {ok, _} -> ?ERROR_UNAUTHORIZED;
        {error, _} = Error -> Error
    end;

authenticate_user(oneprovider, SerializedToken, PeerIp) ->
    % Peer IP caveat is verified here for authentication. However, the client
    % IP is not sent to Onezone when authorizing further requests with
    % the user's token, which means they will fail unless the Onepanel's IP
    % is in the token's whitelist.

    FetchDetailsFun = fun() ->
        try
            {ok, AaiAuth, TokenTTL} = verify_access_token(SerializedToken, PeerIp),
            TTL = min(TokenTTL, ?USER_DETAILS_CACHE_TTL),
            OzPluginAuth = {token, SerializedToken},
            {ok, Details} = fetch_details(OzPluginAuth),
            {true, {Details, AaiAuth, OzPluginAuth}, TTL}
        catch
            error:{badmatch, {error, _} = Error} -> Error
        end
    end,

    case simple_cache:get(
        ?USER_DETAILS_CACHE_KEY(SerializedToken, PeerIp),
        FetchDetailsFun
    ) of
        {ok, {Details, AaiAuth, OzPluginAuth}} ->
            user_details_to_client(Details, AaiAuth, {rest, OzPluginAuth});
        {error, _} = Error ->
            Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Queries the Onezone to check token's validity.
%% @end
%%--------------------------------------------------------------------
-spec verify_access_token(tokens:serialized(), IP :: ip_utils:ip()) ->
    {ok, aai:auth(), TTL :: time_utils:seconds() | undefined} | errors:error().
verify_access_token(SerializedToken, PeerIp) ->
    {ok, Token} = tokens:deserialize(SerializedToken),
    {ok, BinaryIp} = ip_utils:to_binary(PeerIp),
    ReqBody = json_utils:encode(#{
        <<"token">> => SerializedToken, <<"peerIp">> => BinaryIp
    }),
    case oz_endpoint:request(
        op_panel, "/tokens/verify_access_token", post, ReqBody
    ) of
        {ok, _Code, _, RespBodyJson} ->
            case json_utils:decode(RespBodyJson) of
                #{<<"error">> := Error} ->
                    errors:from_json(Error);
                #{<<"subject">> := JsonSubject, <<"ttl">> := TTL} ->
                    Subject = aai:subject_from_json(JsonSubject),
                    Auth = #auth{
                        subject = Subject,
                        caveats = tokens:get_caveats(Token),
                        peer_ip = PeerIp
                    },
                    {ok, Auth, utils:null_to_undefined(TTL)}
            end;
        {error, _} ->
            ?ERROR_NO_CONNECTION_TO_ONEZONE
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves user details via REST.
%% Duplicates oz_users:get_details/1 to avoid warning in the logs
%% on every failed login attempt.
%% @end
%%--------------------------------------------------------------------
-spec fetch_details(RestAuth :: oz_plugin:auth()) -> {ok, #user_details{}} | {error, _}.
fetch_details(RestAuth) ->
    case oz_endpoint:request(RestAuth, "/user", get) of
        {ok, ?HTTP_200_OK, _ResponseHeaders, ResponseBody} ->
            Map = json_utils:decode(ResponseBody),
            UserDetails = #user_details{
                id = maps:get(<<"userId">>, Map),
                full_name = maps:get(<<"name">>, Map),
                linked_accounts = maps:get(<<"linkedAccounts">>, Map),
                username = maps:get(<<"username">>, Map),
                emails = maps:get(<<"emails">>, Map)
            },
            {ok, UserDetails};
        {ok, _, _, ResponseBody} ->
            #{<<"error">> := Error} = json_utils:decode(ResponseBody),
            errors:from_json(Error);
        {error, _} ->
            ?ERROR_NO_CONNECTION_TO_ONEZONE
    end.


%% @private
-spec user_details_to_client(#user_details{}, aai:auth(),
    rest_handler:zone_credentials()) -> #client{} | {error, _} | no_return().
user_details_to_client(Details, Auth, ZoneCredentials) ->
    #user_details{id = OnezoneUserId} = Details,
    case clusters:get_user_privileges(OnezoneUserId) of
        {ok, Privileges} ->
            #client{
                privileges = Privileges,
                user = Details,
                auth = Auth,
                zone_credentials = ZoneCredentials,
                role = member
            };
        ?ERROR_NO_CONNECTION_TO_ONEZONE -> throw(?ERROR_NO_CONNECTION_TO_ONEZONE);
        _Error -> throw(?ERROR_FORBIDDEN)
    end.

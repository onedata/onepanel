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
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("ctool/include/http/codes.hrl").

%% API
-export([authenticate_user/2, authenticate_user/3]).
-export([read_domain/1]).

-define(USER_DETAILS_CACHE_KEY(Token), {user_details, Token}).
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
-spec authenticate_user(Token :: binary(), PeerIp :: ip_utils:ip()) ->
    #client{} | #error{}.
authenticate_user(Token, PeerIp) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case authenticate_user(ClusterType, Token, PeerIp) of
        #client{} = Client -> Client;
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Reads Onezone domain from a Oneprovider registration token.
%% @end
%%--------------------------------------------------------------------
-spec read_domain(RegistrationToken :: binary()) -> Domain :: binary() | no_return().
read_domain(RegistrationToken) ->
    {ok, Token} = tokens:deserialize(RegistrationToken),
    Token#token.onezone_domain.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Authenticate user and obtain their info.
%% Attempts treating the Token as access token or gui token, whichever
%% is accepted, as they cannot be easily distinguished.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(
    onedata:cluster_type(), Token :: binary(), PeerIp :: ip_utils:ip()
) -> #client{} | #error{}.
authenticate_user(onezone, Token, PeerIp) ->
    case service_oz_worker:get_auth_by_token(Token, PeerIp) of
        {ok, Auth} ->
            {ok, Details} = service_oz_worker:get_user_details(Auth),
            user_details_to_client(Details, {rpc, Auth});
        #error{} = Error -> Error
    end;

authenticate_user(oneprovider, Token, _PeerIp) ->
    % Does nothing to relay peer IP to Onezone - which means
    % token with IP or geolocation caveats will not work, unless
    % they whitelist the Onepanel IP as seen by Onezone.
    FetchDetailsFun = fun() ->
        Auth = {token, Token},
        case fetch_details(Auth) of
            {ok, Details} -> {true, {Details, Auth}, ?USER_DETAILS_CACHE_TTL};
            Error -> Error
        end
    end,

    case simple_cache:get(?USER_DETAILS_CACHE_KEY(Token), FetchDetailsFun) of
        {ok, {Details, Auth}} -> user_details_to_client(Details, {rest, Auth});
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves user details via REST.
%% Duplicates oz_users:get_details/1 to avoid warning in the logs
%% on every failed login attempt.
%% @end
%%--------------------------------------------------------------------
-spec fetch_details(RestAuth :: oz_plugin:auth()) -> {ok, #user_details{}} | #error{}.
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
            ?make_error(errors:from_json(Error));
        {error, Reason} -> ?make_error(Reason)
    end.


%% @private
-spec user_details_to_client(#user_details{}, rest_handler:zone_auth()) ->
    #client{} | #error{} | no_return().
user_details_to_client(Details, Auth) ->
    #user_details{id = OnezoneUserId} = Details,
    case clusters:get_user_privileges(OnezoneUserId) of
        {ok, Privileges} ->
            #client{
                privileges = Privileges,
                user = Details,
                zone_auth = Auth,
                role = member
            };
        Error -> Error
    end.

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
-include_lib("ctool/include/oz/oz_users.hrl").
-include_lib("macaroons/src/macaroon.hrl").

%% API
-export([authenticate_user/1]).
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
-spec authenticate_user(Token :: binary()) ->
    #client{} | #error{}.
authenticate_user(Token) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case authenticate_user(ClusterType, Token) of
        #client{} = Client -> Client;
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Reads Onezone domain from a Oneprovider registration token.
%% @end
%%--------------------------------------------------------------------
-spec read_domain(RegistrationToken :: binary()) -> Domain :: binary() | no_return().
read_domain(RegistrationToken) ->
    {ok, Macaroon} = macaroons:deserialize(RegistrationToken),
    Macaroon#macaroon.location.


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
    ClusterType :: onedata:cluster_type(), Token :: binary()
) -> #client{} | #error{}.
authenticate_user(onezone, Token) ->
    ClientOrError = case service_oz_worker:get_logic_client_by_gui_token(Token) of
        {ok, Client} -> {ok, Client};
        _Error -> service_oz_worker:get_logic_client_by_access_token(Token)
    end,
    case ClientOrError of
        {ok, LogicClient} ->
            {ok, Details} = service_oz_worker:get_user_details(LogicClient),
            user_details_to_client(Details, {rpc, LogicClient});
        _ ->
            ?make_error(?ERR_INVALID_AUTH_TOKEN)
    end;

authenticate_user(oneprovider, Token) ->
    FetchDetailsFun = fun() ->
        Auth1 = {gui_token, Token},
        Auth2 = {access_token, Token},
        case fetch_details(Auth1) of
            {ok, Details} -> {true, {Details, Auth1}, ?USER_DETAILS_CACHE_TTL};
            _ ->
                case fetch_details(Auth2) of
                    {ok, Details} -> {true, {Details, Auth2}, ?USER_DETAILS_CACHE_TTL};
                    Error -> Error
                end
        end
    end,

    case simple_cache:get(?USER_DETAILS_CACHE_KEY(Token), FetchDetailsFun) of
        {ok, {Details, Auth}} -> user_details_to_client(Details, {rest, Auth});
        #error{reason = {401, _, _}} -> ?make_error(?ERR_INVALID_AUTH_TOKEN)
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
        {ok, 200, _ResponseHeaders, ResponseBody} ->
            Map = json_utils:decode(ResponseBody),
            UserDetails = #user_details{
                id = maps:get(<<"userId">>, Map),
                full_name = maps:get(<<"name">>, Map),
                linked_accounts = maps:get(<<"linkedAccounts">>, Map),
                username = maps:get(<<"username">>, Map),
                emails = maps:get(<<"emails">>, Map)
            },
            {ok, UserDetails};
        {ok, Code, _, _ResponseBody} -> ?make_error({Code, _ResponseBody, <<>>});
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

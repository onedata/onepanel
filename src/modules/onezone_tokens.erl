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

-type auth() :: rest_handler:zone_auth().

-define(USER_DETAILS_CACHE_KEY(Token), {user_details, Token}).
-define(USER_DETAILS_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl, ?APP_NAME, 0)).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Fetches information about a Onezone user basing on his
%% access token, verifies that he belongs to the cluster and
%% fetches his privileges.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_user(AccessToken :: binary()) ->
    #client{} | #error{}.
authenticate_user(AccessToken) ->
    ClusterType = onepanel_env:get_cluster_type(),
    case authenticate_user(ClusterType, AccessToken) of
        #client{} = Client -> Client;
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Reads Onezone domain from a Oneprovider registration token.
%% @end
%%--------------------------------------------------------------------
-spec read_domain(RegistrationToken :: binary()) -> Domain :: binary() | no_return().
read_domain(RegistrationToken) ->
    {ok, Macaroon} = onedata_macaroons:deserialize(RegistrationToken),
    Macaroon#macaroon.location.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec authenticate_user(
    ClusterType :: onedata:cluster_type(), AccessToken :: binary()
) -> #client{} | #error{}.
authenticate_user(onezone, AccessToken) ->
    case service_oz_worker:get_logic_client(AccessToken) of
        {ok, LogicClient} ->
            {ok, Details} = service_oz_worker:get_user_details(LogicClient),
            user_details_to_client(Details, {rpc, LogicClient});
        _ ->
            ?make_error(?ERR_INVALID_AUTH_TOKEN)
    end;

authenticate_user(oneprovider, AccessToken) ->
    Fetched = simple_cache:get(?USER_DETAILS_CACHE_KEY(AccessToken), fun() ->
        case fetch_details(AccessToken) of
            {ok, Details} -> {true, Details, ?USER_DETAILS_CACHE_TTL};
            Error -> Error
        end
    end),
    case Fetched of
        {ok, Details} -> user_details_to_client(Details, {rest, {access_token, AccessToken}});
        #error{reason = {401, _, _}} -> ?make_error(?ERR_INVALID_AUTH_TOKEN)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves user details via REST.
%% Duplicates oz_users:get_details/1 to avoid warning in the logs
%% on every failed login attempt.
%% @end
%%--------------------------------------------------------------------
-spec fetch_details(AccessToken :: binary()) -> {ok, #user_details{}} | #error{}.
fetch_details(AccessToken) ->
    case oz_endpoint:request({access_token, AccessToken}, "/user", get) of
        {ok, 200, _ResponseHeaders, ResponseBody} ->
            Proplist = json_utils:decode_deprecated(ResponseBody),
            UserDetails = #user_details{
                id = lists_utils:key_get(<<"userId">>, Proplist),
                name = lists_utils:key_get(<<"name">>, Proplist),
                linked_accounts = lists_utils:key_get(<<"linkedAccounts">>, Proplist),
                alias = lists_utils:key_get(<<"alias">>, Proplist),
                email_list = lists_utils:key_get(<<"emailList">>, Proplist)
            },
            {ok, UserDetails};
        {ok, Code, _, _ResponseBody} -> ?make_error({Code, _ResponseBody, <<>>});
        {error, Reason} -> ?make_error(Reason)
    end.


%% @private
-spec user_details_to_client(#user_details{}, auth()) ->
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

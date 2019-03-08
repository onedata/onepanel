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
-module(zone_tokens).
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
-export([authenticate_by_onezone_access_token/1]).
-export([get_zone_domain/1]).

-type auth() :: rest_handler:zone_auth().

-define(USER_DETAILS_CACHE_KEY(Token), {user_details, Token}).
-define(USER_DETAILS_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl, ?APP_NAME, 0)).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Fetches information about a Onezone user basing on his
%% access tokens, verifies that he belongs to the cluster and
%% fetches his privileges.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_onezone_access_token(binary()) ->
    #client{} | #error{}.
authenticate_by_onezone_access_token(AccessToken) ->
    ReleaseType = onepanel_env:get_release_type(),
    case authenticate_by_onezone_access_token(ReleaseType, AccessToken) of
        #client{} = Client -> Client;
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Reads Onezone domain from a Oneprovider registration token.
%% @end
%%--------------------------------------------------------------------
-spec get_zone_domain(RegistrationToken :: binary()) -> Domain :: binary() | no_return().
get_zone_domain(RegistrationToken) ->
    {ok, Macaroon} = onedata_macaroons:deserialize(RegistrationToken),
    Macaroon#macaroon.location.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec authenticate_by_onezone_access_token(
    ReleaseType :: onepanel_env:rel_type(), AccessToken :: binary()
) -> #client{} | #error{}.
authenticate_by_onezone_access_token(onezone, AccessToken) ->
    case service_oz_worker:get_logic_client(AccessToken) of
        {ok, LogicClient} ->
            {ok, Details} = service_oz_worker:get_user_details(LogicClient),
            user_details_to_client(Details, {rpc, LogicClient});
        #error{} = Error -> Error
    end;

authenticate_by_onezone_access_token(oneprovider, AccessToken) ->
    case fetch_details(AccessToken) of
        {ok, Details} -> user_details_to_client(Details, {rest, {access_token, AccessToken}});
        #error{reason = {401, _, _}} -> ?make_error(?ERR_INVALID_ACCESS_TOKEN)
    end.


%% @private
-spec fetch_details(AccessToken :: binary()) -> {ok, #user_details{}} | #error{}.
fetch_details(AccessToken) ->
    simple_cache:get(?USER_DETAILS_CACHE_KEY(AccessToken), fun() ->
        % @fixme get_details uses wrapper reporting errors,
        % which makes incorrent login not-silent
        case oz_users:get_details({access_token, AccessToken}) of
            {ok, Details} ->
                {true, Details, ?USER_DETAILS_CACHE_TTL};
            {error, Reason} ->
                ?make_error(Reason)
        end
    end).


%% @private
-spec user_details_to_client(#user_details{}, auth()) ->
    #client{} | #error{} | no_return().
user_details_to_client(Details, Auth) ->
    #user_details{id = OnezoneId} = Details,
    case clusters:get_user_privileges(Auth, OnezoneId) of
        {ok, Privileges} ->
            #client{
                privileges = Privileges,
                user = Details,
                zone_auth = Auth,
                role = user
            };
        Error -> Error
    end.

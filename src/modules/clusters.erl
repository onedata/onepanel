%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Utilities for fetching clusters information from Onezone.
%%% @end
%%%--------------------------------------------------------------------
-module(clusters).
-author("Wojciech Geisler").

-include("names.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/onedata.hrl").

-type id() :: binary().

-export_type([id/0]).

%% API
-export([get_id/0]).
-export([get_user_privileges/1]).
-export([get_current_cluster/0, get_details/2, list_user_clusters/1,
    get_members_summary/1]).
-export([fetch_remote_provider_info/2]).
-export([acquire_provider_identity_token/0]).
-export([create_invite_token_for_admin/0]).

-define(IDENTITY_TOKEN_CACHE_KEY, identity_token_cache).
-define(PRIVILEGES_CACHE_KEY(OnezoneUserId), {privileges, OnezoneUserId}).
-define(ONEZONE_AUTH_CACHE_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl_seconds, ?APP_NAME, 0)).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns Id of this cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_id() -> id().
get_id() ->
    case onepanel_env:get_cluster_type() of
        onezone ->
            ?ONEZONE_CLUSTER_ID;
        oneprovider ->
            <<_Id/binary>> = service_oneprovider:get_id()
    end.


%%--------------------------------------------------------------------
%% @doc Returns details of current cluster.
%% Uses the root authorization, since all users with onepanel access
%% should have access to the cluster details.
%% @end
%%--------------------------------------------------------------------
-spec get_current_cluster() ->
    #{atom() := term()} | {error, _}.
get_current_cluster() ->
    try
        Auth = onezone_client:root_auth(),
        {ok, Details} = get_details(Auth, get_id()),
        store_in_cache(cluster, Details),
        Details
    catch Type:Error ->
        try_cached(cluster, Type, Error)
    end.


%%--------------------------------------------------------------------
%% @doc Returns summary with counts of users and groups belonging
%% to the current cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_members_summary(rest_handler:zone_credentials()) ->
    #{atom() := non_neg_integer()} | no_return().
get_members_summary(Auth) ->
    Users = get_members_count(Auth, users, direct),
    EffUsers = get_members_count(Auth, users, effective),
    Groups = get_members_count(Auth, groups, direct),
    EffGroups = get_members_count(Auth, groups, effective),
    #{
        users_count => Users, groups_count => Groups,
        effective_users_count => EffUsers, effective_groups_count => EffGroups
    }.


%%--------------------------------------------------------------------
%% @doc Returns user privileges in the current cluster by UserId.
%% Throws if connection to Onezone could not be established.
%% Retrieves the credentials using root client authorization
%% as the user might not have enough privileges to view his own privileges.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(OnezoneUserId :: binary()) ->
    {ok, [privileges:cluster_privilege()]} | {error, _} | no_return().
get_user_privileges(OnezoneUserId) ->
    RootAuth = onezone_client:root_auth(),
    get_user_privileges(RootAuth, OnezoneUserId).


%%--------------------------------------------------------------------
%% @private
%% @doc Returns user privileges in the current cluster by UserId.
%% Uses specified authentication for the request.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(rest_handler:zone_credentials(), OnezoneUserId :: binary()) ->
    {ok, [privileges:cluster_privilege()]} | {error, _} | no_return().
get_user_privileges({rest, RestAuth}, OnezoneUserId) ->
    node_cache:acquire(?PRIVILEGES_CACHE_KEY(OnezoneUserId), fun() ->
        case zone_rest(RestAuth, "/clusters/~s/effective_users/~s/privileges",
            [get_id(), OnezoneUserId]) of
            {ok, #{privileges := Privileges}} ->
                ListOfAtoms = onepanel_utils:convert(Privileges, {seq, atom}),
                {ok, ListOfAtoms, ?ONEZONE_AUTH_CACHE_CACHE_TTL};
            ?ERROR_NOT_FOUND -> ?ERROR_USER_NOT_IN_CLUSTER;
            {error, _} = Error -> Error
        end
    end);

get_user_privileges({rpc, Auth}, OnezoneUserId) ->
    case oz_worker_rpc:cluster_get_eff_user_privileges(
        Auth, get_id(), OnezoneUserId
    ) of
        ?ERROR_NOT_FOUND -> ?ERROR_USER_NOT_IN_CLUSTER;
        {error, _} = Error -> Error;
        {ok, Privileges} -> {ok, Privileges}
    end.


%%--------------------------------------------------------------------
%% @doc Returns protected details of a cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: rest_handler:zone_credentials(), ClusterId :: id()) ->
    {ok, #{atom() := term()}} | {error, _}.
get_details({rpc, Auth}, ClusterId) ->
    case oz_worker_rpc:get_protected_cluster_data(Auth, ClusterId) of
        {ok, ClusterData} ->
            {ok, kv_utils:copy_found([
                {<<"onepanelVersion">>, onepanelVersion},
                {<<"workerVersion">>, workerVersion},
                {<<"onepanelProxy">>, onepanelProxy},
                {<<"type">>, type}
            ], ClusterData, #{id => ClusterId, serviceId => ClusterId})};
        Error -> Error
    end;

get_details({rest, Auth}, ClusterId) ->
    case zone_rest(Auth, "/clusters/~s", [ClusterId]) of
        {ok, Map} ->
            Map2 = maps:without([clusterId], Map),
            {ok, Map2#{id => ClusterId, serviceId => ClusterId}};
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Returns ids of clusters belonging to the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec list_user_clusters(rest_handler:zone_credentials()) ->
    {ok, [id()]} | errors:error().
list_user_clusters({rpc, Auth}) ->
    case oz_worker_rpc:get_eff_clusters_by_user_auth(Auth) of
        {ok, Ids} -> {ok, Ids};
        Error -> Error
    end;

list_user_clusters({rest, Auth}) ->
    case zone_rest(Auth, "/user/effective_clusters/", []) of
        {ok, #{clusters := Ids}} -> {ok, Ids};
        {error, _} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Fetches information about a remote provider.
%% User must belong to its cluster.
%% @end
%%--------------------------------------------------------------------
-spec fetch_remote_provider_info(Auth :: rest_handler:zone_credentials(), ProviderId :: binary()) ->
    #{binary() := term()}.
fetch_remote_provider_info({rpc, Client}, ProviderId) ->
    case oz_worker_rpc:get_protected_provider_data(Client, ProviderId) of
        {ok, ProviderData} -> format_provider_info(ProviderData);
        Error -> throw(Error)
    end;

fetch_remote_provider_info({rest, RestAuth}, ProviderId) ->
    URN = "/providers/" ++ binary_to_list(ProviderId),
    case oz_endpoint:request(RestAuth, URN, get) of
        {ok, ?HTTP_200_OK, _, BodyJson} ->
            format_provider_info(json_utils:decode(BodyJson));
        {ok, ?HTTP_404_NOT_FOUND, _, _} ->
            throw(?ERROR_NOT_FOUND);
        {error, _} ->
            throw(?ERROR_NO_CONNECTION_TO_ONEZONE)
    end.


%%--------------------------------------------------------------------
%% @doc Returns cached or newly created provider identity token.
%% @end
%%--------------------------------------------------------------------
-spec acquire_provider_identity_token() -> {ok, tokens:serialized()} | errors:error().
acquire_provider_identity_token() ->
    node_cache:acquire(?IDENTITY_TOKEN_CACHE_KEY, fun() ->
        ValidUntil = global_clock:timestamp_seconds() + ?ONEZONE_AUTH_CACHE_CACHE_TTL,
        Body = json_utils:encode(#{
            <<"type">> => token_type:to_json(?IDENTITY_TOKEN),
            <<"caveats">> => [caveats:to_json(#cv_time{valid_until = ValidUntil})]
        }),
        case zone_rest(post, provider, "/provider/tokens/temporary", [], Body) of
            {ok, #{token := Token}} -> {ok, Token, ?ONEZONE_AUTH_CACHE_CACHE_TTL};
            Error -> Error
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% Obtains token which enables a Onezone user to join current cluster.
%% @end
%%--------------------------------------------------------------------
-spec create_invite_token_for_admin() -> {ok, tokens:serialized()} | {error, _}.
create_invite_token_for_admin() ->
    create_invite_token_for_admin(onezone_client:root_auth()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec zone_rest(Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()]) ->
    {ok, #{atom() => term()}} | {error, _}.
zone_rest(Auth, URNFormat, FormatArgs) ->
    zone_rest(get, Auth, URNFormat, FormatArgs).

%% @private
-spec zone_rest(Method :: http_client:method(), Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()]) ->
    {ok, #{atom() => term()}} | errors:error().
zone_rest(Method, Auth, URNFormat, FormatArgs) ->
    zone_rest(Method, Auth, URNFormat, FormatArgs, <<"">>).

%% @private
-spec zone_rest(Method :: http_client:method(), Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()], Body :: http_client:request_body()) ->
    {ok, #{atom() => term()}} | {error, _}.
zone_rest(Method, Auth, URNFormat, FormatArgs, Body) ->
    URN = str_utils:format(URNFormat, FormatArgs),
    case oz_endpoint:request(Auth, URN, Method, Body) of
        {ok, Code, _, RespBodyJson} ->
            case json_utils:decode(RespBodyJson) of
                #{<<"error">> := Error} when Code >= 400 ->
                    errors:from_json(Error);
                RespBody ->
                    {ok, onepanel_utils:convert(RespBody, {keys, atom})}
            end;
        {error, _} ->
            ?ERROR_NO_CONNECTION_TO_ONEZONE
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Stores given data in service's ctx to make them available
%% when the service nodes are offline.
%% @end
%%--------------------------------------------------------------------
-spec store_in_cache(Key :: term(), Value :: term()) -> ok.
store_in_cache(Key, Value) ->
    Service = onepanel_env:get_cluster_type(),
    {ok, _} = service:update_ctx(Service, #{Key => Value}),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves given key from service ctx.
%% Throws given Error on failure.
%% @end
%%--------------------------------------------------------------------
-spec try_cached(Key :: term(), throw | error | exit, ErrorResult :: term()) ->
    FoundValue :: term().
try_cached(Key, ErrorClass, Error) ->
    Service = onepanel_env:get_cluster_type(),
    case service:get_ctx(Service) of
        #{Key := Value} -> Value;
        _ -> erlang:ErrorClass(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns number of given entities (users or groups)
%% belonging to the cluster - either directly or effectively.
%% @end
%%--------------------------------------------------------------------
-spec get_members_count(Auth :: rest_handler:zone_credentials(),
    UsersOrGroups :: users | groups, DirectOrEffective :: direct | effective) ->
    non_neg_integer().
get_members_count({rest, Auth}, UsersOrGroups, DirectOrEffective) ->
    {Resource, ResponseKey} = case {UsersOrGroups, DirectOrEffective} of
        {users, direct} -> {"users", users};
        {users, effective} -> {"effective_users", users};
        {groups, direct} -> {"groups", groups};
        {groups, effective} -> {"effective_groups", groups}
    end,

    case zone_rest(Auth, "/clusters/~s/~s", [get_id(), Resource]) of
        {ok, #{ResponseKey := List}} -> length(List);
        Error -> throw(Error)
    end;

get_members_count({rpc, Auth}, UsersOrGroups, DirectOrEffective) ->
    Function = case {UsersOrGroups, DirectOrEffective} of
        {users, direct} -> cluster_logic_get_users;
        {users, effective} -> cluster_logic_get_eff_users;
        {groups, direct} -> cluster_logic_get_groups;
        {groups, effective} -> cluster_logic_get_eff_groups
    end,
    case oz_worker_rpc:Function(Auth, get_id()) of
        {ok, List} -> length(List);
        Error -> throw(Error)
    end.


%% @private
-spec create_invite_token_for_admin(rest_handler:zone_credentials()) ->
    {ok, tokens:serialized()} | {error, _}.
create_invite_token_for_admin({rpc, Auth}) ->
    case oz_worker_rpc:cluster_logic_create_invite_token_for_admin(Auth, get_id()) of
        {ok, Token} -> tokens:serialize(Token);
        Error -> Error
    end;

create_invite_token_for_admin({rest, Auth}) ->
    TokenName = <<
        "admin invite to cluster ",
        (binary:part(time:seconds_to_iso8601(global_clock:timestamp_seconds()), 0, 10))/binary, " ",
        (str_utils:rand_hex(3))/binary
    >>,
    Body = json_utils:encode(#{
        <<"name">> => TokenName,
        <<"type">> => token_type:to_json(?INVITE_TOKEN(?USER_JOIN_CLUSTER, get_id())),
        <<"usageLimit">> => 1,
        <<"privileges">> => privileges:cluster_admin()
    }),
    case zone_rest(post, Auth, "/provider/tokens/named", [], Body) of
        {ok, #{token := Token}} -> {ok, Token};
        Error -> Error
    end.


%% @private
-spec format_provider_info(OzResponse :: #{binary() => term()}) ->
    #{binary() => term()}.
format_provider_info(OzResponse) ->
    kv_utils:copy_found([
        {<<"providerId">>, <<"id">>},
        {<<"name">>, <<"name">>},
        {<<"domain">>, <<"domain">>},
        {<<"longitude">>, <<"geoLongitude">>},
        {<<"latitude">>, <<"geoLatitude">>},
        {<<"cluster">>, <<"cluster">>},
        {<<"online">>, <<"online">>}
    ], OzResponse).

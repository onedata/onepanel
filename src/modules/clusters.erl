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
-export([create_invite_token_for_admin/0]).

-define(PRIVILEGES_CACHE_KEY(OnezoneUserId), {privileges, OnezoneUserId}).
-define(PRIVILEGES_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl, ?APP_NAME, 0)).


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
    #{atom() := term()} | #error{}.
get_current_cluster() ->
    try
        Auth = onezone_client:root_auth(),
        {ok, Details} = get_details(Auth, get_id()),
        store_in_cache(cluster, Details),
        Details
    catch _Type:Error ->
        try_cached(cluster, ?make_stacktrace(Error))
    end.


%%--------------------------------------------------------------------
%% @doc Returns summary with counts of users and groups belonging
%% to the current cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_members_summary(rest_handler:zone_auth()) ->
    #{atom() := non_neg_integer()} | no_return().
get_members_summary(Auth) ->
    Users = get_members_count(Auth, users, direct),
    EffUsers = get_members_count(Auth, users, effective),
    Groups = get_members_count(Auth, groups, direct),
    EffGroups = get_members_count(Auth, groups, effective),
    #{
        usersCount => Users, groupsCount => Groups,
        effectiveUsersCount => EffUsers, effectiveGroupsCount => EffGroups
    }.


%%--------------------------------------------------------------------
%% @doc Returns user privileges in the current cluster by UserId.
%% Throws if connection to Onezone could not be established.
%% Retrieves the credentials using root client authorization
%% as the user might not have enough privileges to view his own privileges.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(OnezoneUserId :: binary()) ->
    {ok, [privileges:cluster_privilege()]} | #error{} | no_return().
get_user_privileges(OnezoneUserId) ->
    RootAuth = onezone_client:root_auth(),
    get_user_privileges(RootAuth, OnezoneUserId).


%%--------------------------------------------------------------------
%% @private
%% @doc Returns user privileges in the current cluster by UserId.
%% Uses specified authentication for the request.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(rest_handler:zone_auth(), OnezoneUserId :: binary()) ->
    {ok, [privileges:cluster_privilege()]} | #error{} | no_return().
get_user_privileges({rest, RestAuth}, OnezoneUserId) ->
    simple_cache:get(?PRIVILEGES_CACHE_KEY(OnezoneUserId), fun() ->
        case zone_rest(RestAuth, "/clusters/~s/effective_users/~s/privileges",
            [get_id(), OnezoneUserId]) of
            {ok, #{privileges := Privileges}} ->
                ListOfAtoms = onepanel_utils:convert(Privileges, {seq, atom}),
                {true, ListOfAtoms, ?PRIVILEGES_CACHE_TTL};
            #error{reason = {?HTTP_401_UNAUTHORIZED, _, _}} ->
                ?make_error(?ERR_UNAUTHORIZED);
            #error{reason = {?HTTP_404_NOT_FOUND, _, _}} ->
                ?make_error(?ERR_USER_NOT_IN_CLUSTER);
            #error{} = Error -> throw(Error)
        end
    end);

get_user_privileges({rpc, Auth}, OnezoneUserId) ->
    case oz_worker_rpc:cluster_get_eff_user_privileges(
        Auth, get_id(), OnezoneUserId
    ) of
        ?ERROR_NOT_FOUND -> ?make_error(?ERR_USER_NOT_IN_CLUSTER);
        {ok, Privileges} -> {ok, Privileges}
    end.


%%--------------------------------------------------------------------
%% @doc Returns protected details of a cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: rest_handler:zone_auth(), ClusterId :: id()) ->
    {ok, #{atom() := term()}} | #error{}.
get_details({rpc, Auth}, ClusterId) ->
    case oz_worker_rpc:get_protected_cluster_data(Auth, ClusterId) of
        {ok, ClusterData} ->
            {ok, kv_utils:copy_found([
                {<<"onepanelVersion">>, onepanelVersion},
                {<<"workerVersion">>, workerVersion},
                {<<"onepanelProxy">>, onepanelProxy},
                {<<"type">>, type}
            ], ClusterData, #{id => ClusterId, serviceId => ClusterId})};
        Error -> ?make_error(Error)
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
-spec list_user_clusters(rest_handler:zone_auth()) ->
    {ok, [id()]} | #error{}.
list_user_clusters({rpc, Auth}) ->
    case oz_worker_rpc:get_clusters_by_user_auth(Auth) of
        {ok, Ids} -> {ok, Ids};
        Error -> ?make_error(Error)
    end;

list_user_clusters({rest, Auth}) ->
    case zone_rest(Auth, "/user/effective_clusters/", []) of
        {ok, #{clusters := Ids}} -> {ok, Ids};
        #error{} = Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Fetches information about a remote provider.
%% User must belong to its cluster.
%% @end
%%--------------------------------------------------------------------
-spec fetch_remote_provider_info(Auth :: rest_handler:zone_auth(), ProviderId :: binary()) ->
    #{binary() := term()}.
fetch_remote_provider_info({rpc, Client}, ProviderId) ->
    case oz_worker_rpc:get_protected_provider_data(Client, ProviderId) of
        {ok, ProviderData} -> format_provider_info(ProviderData);
        {error, not_found} -> ?throw_error(?ERR_NOT_FOUND)
    end;

fetch_remote_provider_info({rest, RestAuth}, ProviderId) ->
    URN = "/providers/" ++ binary_to_list(ProviderId),
    case oz_endpoint:request(RestAuth, URN, get) of
        {ok, ?HTTP_200_OK, _, BodyJson} ->
            format_provider_info(json_utils:decode(BodyJson));
        {ok, ?HTTP_404_NOT_FOUND, _, _} ->
            ?throw_error(?ERR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Obtains token which enables a Onezone user to join current cluster.
%% @end
%%--------------------------------------------------------------------
-spec create_invite_token_for_admin() -> {ok, tokens:serialized()} | #error{}.
create_invite_token_for_admin() ->
    create_invite_token_for_admin(onezone_client:root_auth()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec zone_rest(Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()]) ->
    {ok, #{atom() => term()}} | #error{}.
zone_rest(Auth, URNFormat, FormatArgs) ->
    zone_rest(get, Auth, URNFormat, FormatArgs).

%% @private
-spec zone_rest(Method :: http_client:method(), Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()]) ->
    {ok, #{atom() => term()}} | #error{}.
zone_rest(Method, Auth, URNFormat, FormatArgs) ->
    zone_rest(Method, Auth, URNFormat, FormatArgs, <<"">>).

%% @private
-spec zone_rest(Method :: http_client:method(), Auth :: oz_plugin:auth(),
    URNFormat :: string(), FormatArgs :: [term()], Body :: http_client:request_body()) ->
    {ok, #{atom() => term()}} | #error{}.
zone_rest(Method, Auth, URNFormat, FormatArgs, Body) ->
    URN = str_utils:format(URNFormat, FormatArgs),
    case oz_endpoint:request(Auth, URN, Method, Body) of
        {ok, C, _, BodyJson} when C == ?HTTP_200_OK orelse C == ?HTTP_201_CREATED ->
            Parsed = onepanel_utils:convert(json_utils:decode(BodyJson), {keys, atom}),
            {ok, Parsed};
        {ok, _, _, Body} ->
            #{<<"error">> := Error} = json_utils:decode(Body),
            ?make_error(errors:from_json(Error));
        {error, econnrefused} ->
            ?make_error(?ERR_ONEZONE_NOT_AVAILABLE);
        {error, Reason} ->
            ?make_error(Reason)
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
    service:update_ctx(Service, #{Key => Value}).


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves given key from service ctx.
%% Throws given Error on failure.
%% @end
%%--------------------------------------------------------------------
-spec try_cached(Key :: term(), ErrorResult :: term()) ->
    FoundValue :: term().
try_cached(Key, Error) ->
    Service = onepanel_env:get_cluster_type(),
    case service:get_ctx(Service) of
        #{Key := Value} -> Value;
        _ -> throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Returns number of given entities (users or groups)
%% belonging to the cluster - either directly or effectively.
%% @end
%%--------------------------------------------------------------------
-spec get_members_count(Auth :: rest_handler:zone_auth(),
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
        Error -> ?throw_error(Error)
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
        Error -> ?throw_error(Error)
    end.


%% @private
-spec create_invite_token_for_admin(rest_handler:zone_auth()) ->
    {ok, tokens:serialized()} | #error{}.
create_invite_token_for_admin({rpc, Auth}) ->
    case oz_worker_rpc:cluster_logic_create_invite_token_for_admin(Auth, get_id()) of
        {ok, Token} -> tokens:serialize(Token);
        Error -> ?make_error(Error)
    end;

create_invite_token_for_admin({rest, Auth}) ->
    TokenName = <<
        "admin invite to cluster ",
        (binary:part(time_utils:epoch_to_iso8601(time_utils:system_time_seconds()), 0, 10))/binary, " ",
        (str_utils:rand_hex(3))/binary
    >>,
    Body = json_utils:encode(#{
        <<"name">> => TokenName,
        <<"type">> => tokens:type_to_json(?INVITE_TOKEN(?USER_JOIN_CLUSTER, get_id())),
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
    ], OzResponse, #{}).

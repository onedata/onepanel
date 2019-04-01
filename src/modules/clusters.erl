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
-include_lib("ctool/include/api_errors.hrl").
-include_lib("ctool/include/logging.hrl").

-type id() :: binary().

-export_type([id/0]).

%% API
-export([get_id/0]).
-export([get_user_privileges/2]).
-export([get_current_cluster/0, get_details/2, list_user_clusters/1]).
-export([fetch_remote_provider_info/2]).

-define(PRIVILEGES_CACHE_KEY(OnezoneUserId), {privileges, OnezoneUserId}).
-define(PRIVILEGES_CACHE_TTL, onepanel_env:get(onezone_auth_cache_ttl, ?APP_NAME, 0)).

%%--------------------------------------------------------------------
%% @doc Returns Id of this cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_id() -> id().
get_id() ->
    get_id(onepanel_env:get_cluster_type()).


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
        Auth = zone_client:root_auth(),
        {ok, Details} = get_details(Auth, ?MODULE:get_id()),
        store_in_cache(cluster, Details),
        Details
    catch _Type:Error ->
        try_cached(cluster, ?make_stacktrace(Error))
    end.


%%--------------------------------------------------------------------
%% @doc Given user credentials, returns his privileges in the current cluster.
%% Throws if connection could not be established.
%% Retrieves the credentials using root client authorization
%% as the user might not have enough privileges to view his own privileges.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(rest_handler:zone_auth(), OnezoneUserId :: binary()) ->
    {ok, [privileges:cluster_privilege()]} | #error{} | no_return().
get_user_privileges({rest, _}, OnezoneUserId) ->
    simple_cache:get(?PRIVILEGES_CACHE_KEY(OnezoneUserId), fun() ->
        {rest, RestAuth} = zone_client:root_auth(),
        case zone_rest(RestAuth, "/clusters/~s/effective_users/~s/privileges",
            [?MODULE:get_id(), OnezoneUserId]) of
            {ok, #{privileges := Privileges}} ->
                ListOfAtoms = onepanel_utils:convert(Privileges, {seq, atom}),
                {true, ListOfAtoms, ?PRIVILEGES_CACHE_TTL};
            #error{reason = {401, _, _}} ->
                ?make_error(?ERR_UNAUTHORIZED);
            #error{reason = {404, _, _}} ->
                ?make_error(?ERR_USER_NOT_IN_CLUSTER);
            #error{} = Error -> throw(Error)
        end
    end);

get_user_privileges({rpc, _}, OnezoneUserId) ->
    {ok, LogicClient} = service_oz_worker:get_logic_client(root),
    case zone_rpc(cluster_logic, get_eff_user_privileges,
        [LogicClient, ?MODULE:get_id(), OnezoneUserId]) of
        #error{reason = ?ERR_NOT_FOUND} -> ?make_error(?ERR_USER_NOT_IN_CLUSTER);
        {ok, Privileges} -> {ok, Privileges}
    end.


%%--------------------------------------------------------------------
%% @doc Returns protected details of a cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: rest_handler:zone_auth(), ClusterId :: id()) ->
    {ok, #{atom() := term()}} | #error{}.
get_details({rpc, Auth}, ClusterId) ->
    case zone_rpc(cluster_logic, get_protected_data, [Auth, ClusterId]) of
        {ok, ClusterData} ->
            {ok, onepanel_maps:get_store_multiple([
                {<<"onepanelVersion">>, onepanelVersion},
                {<<"workerVersion">>, workerVersion},
                {<<"onepanelProxy">>, onepanelProxy},
                {<<"serviceId">>, serviceId},
                {<<"type">>, type}
            ], ClusterData, #{id => ClusterId})};
        Error -> Error
    end;

get_details({rest, Auth}, ClusterId) ->
    case zone_rest(Auth, "/clusters/~s", [ClusterId]) of
        {ok, Map} ->
            Map2 = maps:without([clusterId], Map),
            {ok, Map2#{id => ClusterId}};
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Returns ids of clusters belonging to the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec list_user_clusters(rest_handler:zone_auth()) ->
    {ok, [id()]} | #error{}.
list_user_clusters({rpc, Auth}) ->
    zone_rpc(user_logic, get_clusters, [Auth]);

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
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    {ok, ProviderData} = rpc:call(
        OzNode, provider_logic, get_protected_data, [Client, ProviderId]
    ),
    format_provider_info(ProviderData);

fetch_remote_provider_info({rest, RestAuth}, ProviderId) ->
    URN = "/providers/" ++ binary_to_list(ProviderId),
    {ok, 200, _, BodyJson} = oz_endpoint:request(RestAuth, URN, get),
    format_provider_info(json_utils:decode(BodyJson)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_id(onedata:cluster_type()) -> id() | no_return().
get_id(onezone) ->
    case zone_rpc(cluster_logic, get_onezone_cluster_id, []) of
        <<Id/binary>> ->
            store_in_cache(cluster_id, Id),
            Id;
        Error ->
            try_cached(cluster_id, Error)
    end;

get_id(oneprovider) ->
    try service_oneprovider:get_details() of
        #{cluster := <<Id/binary>>} ->
            store_in_cache(cluster_id, Id),
            Id
    catch _Type:Error ->
        try_cached(cluster_id, ?make_stacktrace(Error))
    end.


%% @private
-spec zone_rpc(Module :: module(), Function :: atom(), Args :: [term()]) ->
    term() | #error{}.
zone_rpc(Module, Function, Args) ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    case rpc:call(OzNode, Module, Function, Args) of
        {badrpc, _} = Error -> ?make_error(Error);
        {error, Reason} -> ?make_error(Reason);
        Result -> Result
    end.


%% @private
-spec zone_rest(Auth :: oz_plugin:auth(), URNFormat :: string(),
    FormatArgs :: [term()]) -> {ok, #{atom() => term()}} | #error{}.
zone_rest(Auth, URNFormat, FormatArgs) ->
    URN = str_utils:format(URNFormat, FormatArgs),
    case oz_endpoint:request(Auth, URN, get) of
        {ok, 200, _, BodyJson} ->
            Parsed = onepanel_utils:convert(json_utils:decode(BodyJson), {keys, atom}),
            {ok, Parsed};
        {ok, Code, Error, Description} ->
            ?make_error({Code, Error, Description});
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


%% @private
-spec format_provider_info(OzResponse :: #{binary() => term()}) ->
    #{binary() => term()}.
format_provider_info(OzResponse) ->
    onepanel_maps:get_store_multiple([
        {<<"providerId">>, <<"id">>},
        {<<"name">>, <<"name">>},
        {<<"domain">>, <<"domain">>},
        {<<"longitude">>, <<"geoLongitude">>},
        {<<"latitude">>, <<"geoLatitude">>},
        {<<"cluster">>, <<"cluster">>},
        {<<"online">>, <<"online">>}
    ], OzResponse).

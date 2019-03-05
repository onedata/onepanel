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

-type cluster_id() :: binary().

-export_type([cluster_id/0]).

%% API
-export([get_id/0]).
-export([get_user_privileges/2]).
-export([get_current_cluster/0, get_details/2, list_user_clusters/1]).

-define(PRIVILEGE_CACHE(Auth), {privileges, Auth}).
-define(PRIVILEGE_CACHE_TTL, onepanel_env:get(token_cache_ttl, ?APP_NAME, 0)).

%%--------------------------------------------------------------------
%% @doc Returns Id of this cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_id() -> cluster_id().
get_id() ->
    get_id(onepanel_env:get_release_type()).


%%--------------------------------------------------------------------
%% @doc Returns details of current cluster.
%% Uses the root authorization, since all users with onepanel access
%% should have access to the cluster details.
%% @end
%%--------------------------------------------------------------------
-spec get_current_cluster() ->
    #{atom() := term()} | #error{}.
get_current_cluster() ->
    Service = onepanel_env:get_release_type(),
    try
        Auth = remotes:root_auth(),
        {ok, Details} = get_details(Auth, get_id()),
        store(cluster, Details, Service)
    catch _Type:Error ->
        fetch(cluster, Service, ?make_stacktrace(Error))
    end.


%%--------------------------------------------------------------------
%% @doc Given user credentials, returns his privileges in the current cluster.
%% Throws if connection could not be established.
%% @end
%%--------------------------------------------------------------------
-spec get_user_privileges(rest_handler:zone_auth(), onepanel_user:onezone_id()) ->
    {ok, [privileges:cluser_privilege()]} | #error{} | no_return().
get_user_privileges({rest, RestAuth} = Auth, OnezoneUserId) ->
    simple_cache:get(?PRIVILEGE_CACHE(Auth), fun() ->
        URN = str_utils:format("/clusters/~s/effective_users/~s/privileges",
            [get_id(), OnezoneUserId]),
        case oz_endpoint:request(RestAuth, URN, get) of
            {ok, 401, _, _} ->
                ?make_error(?ERR_UNAUTHORIZED);
            {ok, 404, _, _} ->
                ?make_error(?ERR_USER_NOT_IN_CLUSTER);
            {ok, 200, _, Body} ->
                #{<<"privileges">> := Privileges} = json_utils:decode(Body),
                ListOfAtoms = onepanel_utils:convert(Privileges, {seq, atom}),
                {true, ListOfAtoms, ?PRIVILEGE_CACHE_TTL};
            {error, Error} -> ?throw_error(Error)
        end
    end);

get_user_privileges({rpc, LogicClient}, OnezoneUserId) ->
    case zone_rpc(cluster_logic, get_eff_user_privileges,
        [LogicClient, get_id(), OnezoneUserId]) of
        #error{reason = ?ERR_NOT_FOUND} -> ?make_error(?ERR_USER_NOT_IN_CLUSTER);
        {ok, Privileges} -> {ok, Privileges}
    end.


%%--------------------------------------------------------------------
%% @doc Returns protected details of a cluster.
%% @end
%%--------------------------------------------------------------------
-spec get_details(Auth :: rest_handler:zone_auth(), ClusterId :: cluster_id()) ->
    {ok, #{atom() := term()}} | #error{}.
get_details({rpc, Auth}, ClusterId) ->
    {ok, OzNode} = nodes:any(?SERVICE_OZW),
    case rpc:call(OzNode, cluster_logic, get_protected_data, [Auth, ClusterId]) of
        {ok, ClusterData} ->
            {ok, onepanel_maps:get_store_multiple([
                {<<"onepanelVersion">>, onepanelVersion},
                {<<"workerVersion">>, workerVersion},
                {<<"onepanelProxy">>, onepanelProxy},
                {<<"serviceId">>, serviceId},
                {<<"type">>, type}
            ], ClusterData, #{id => ClusterId})};
        {error, Reason} ->
            ?make_error(Reason)
    end;

get_details({rest, Auth}, ClusterId) ->
    URN = str_utils:format("/clusters/~s", [ClusterId]),
    case zone_rest(URN, Auth) of
        {ok, Map} ->
            Map2 = maps:without([cluster_id], Map),
            {ok, Map2#{id => ClusterId}};
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Returns ids of clusters belonging to the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec list_user_clusters(rest_handler:zone_auth()) ->
    {ok, [cluster_id()]} | #error{}.
list_user_clusters({rpc, Auth}) ->
    zone_rpc(user_logic, get_clusters, [Auth]);

list_user_clusters({rest, Auth}) ->
    case zone_rest("/user/clusters/", Auth) of
        {ok, #{clusters := Ids}} -> {ok, Ids};
        #error{} = Error -> Error
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_id(Release :: onezone | oneprovider) -> cluster_id() | no_return().
get_id(Service = onezone) ->
    case zone_rpc(cluster_logic, get_onezone_cluster_id, []) of
        <<Id/binary>> ->
            store(cluster_id, Id, Service);
        Error ->
            fetch(cluster_id, Service, Error)
    end;

get_id(Service = oneprovider) ->
    try service_oneprovider:get_details() of
        #{cluster := <<Id/binary>>} ->
            store(cluster_id, Id, Service)
    catch _Type:Error ->
        fetch(cluster_id, Service, ?make_stacktrace(Error))
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
-spec zone_rest(URN :: string(), Auth :: oz_plugin:auth()) ->
    {ok, #{atom() => term()}} | #error{}.
zone_rest(URN, Auth) ->
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
-spec store(Key :: term(), Value :: term(), Service :: service:name()) ->
    Value :: term().
store(Key, Value, Service) ->
    service:update_ctx(Service, #{Key => Value}),
    Value.


%%--------------------------------------------------------------------
%% @private
%% @doc Retrieves given key from service ctx.
%% Throws given Error on failure.
%% @end
%%--------------------------------------------------------------------
-spec fetch(Key :: term(), Service :: service:name(), ErrorResult :: term()) ->
    FoundValue :: term().
fetch(Key, Service, Error) ->
    case service:get_ctx(Service) of
        #{Key := Value} -> Value;
        _ -> throw(Error)
    end.

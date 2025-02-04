%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_cluster type.
%%% Handled requests concern the od_cluster entities, either
%%% the local cluster or other clusters of the authenticated user.
%%% @end
%%%-------------------------------------------------------------------
-module(cluster_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
operation_supported(create, invite_user_token, private) -> true;

operation_supported(get, instance, private) -> true;
operation_supported(get, current_cluster, private) -> true;
operation_supported(get, current_cluster_members_summary, private) -> true;

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(get, current_cluster, private) ->
    % always available even in oz_panel because of caching
    [];

required_availability(Op, Aspect, private) when
    Op == get, Aspect == instance;
    Op == get, Aspect == current_cluster_members_summary;
    Op == create, Aspect == invite_user_token
->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER -> []; % fetches from ozw, local services may be down
        ?ONEZONE -> [?SERVICE_OZW, all_healthy]
    end.


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = invite_user_token}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_ADD_USER);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == current_cluster
->
    true;

authorize(#onp_req{operation = get, client = Client,
    gri = #gri{aspect = current_cluster_members_summary}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_VIEW).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{
    operation = get, gri = #gri{aspect = instance}, client = #client{role = Role}
}, _) ->
    case Role of
        root ->
            % clusters must be fetched with Onezone user authorization
            throw(?ERROR_NOT_FOUND);
        member ->
            ok
    end;

validate(#onp_req{operation = Op, gri = #gri{aspect = Aspect}}, _) when
    Op == get andalso Aspect == current_cluster;
    Op == get andalso Aspect == current_cluster_members_summary;
    Op == create andalso Aspect == invite_user_token
->
    case onepanel_env:get_cluster_type() of
        ?ONEPROVIDER ->
            service_oneprovider:is_registered()
                orelse throw(?ERROR_NOT_FOUND);
        ?ONEZONE ->
            onepanel_deployment:is_set(?PROGRESS_CLUSTER)
                orelse throw(?ERROR_NOT_FOUND)
    end,
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = invite_user_token}, data = _Data}) ->
    case clusters:create_invite_token_for_admin() of
        {ok, Token} -> {ok, value, Token};
        {error, _} = Error -> Error
    end.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance, id = Id}, client = Client}, _) ->
    clusters:get_details(Client#client.zone_credentials, Id);

get(#onp_req{gri = #gri{aspect = current_cluster}}, _) ->
    case clusters:get_current_cluster() of
        {error, _} = Error -> Error;
        Result -> {ok, value, Result}
    end;

get(#onp_req{gri = #gri{aspect = current_cluster_members_summary}, client = #client{zone_credentials = Auth}}, _) ->
    {ok, value, clusters:get_members_summary(Auth)}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{}, data = _Data}) ->
    ?ERROR_NOT_SUPPORTED.


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% internal functions
%%%===================================================================


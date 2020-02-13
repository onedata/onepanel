%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_user type.
%%% Handles operations concerning Onezone users, which means:
%%% - in oz_panel, management of basic auth oz_worker users
%%% - in both oz/op_panek, information about the currently authenticated user
%%% @end
%%%-------------------------------------------------------------------
-module(user_middleware).
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
operation_supported(create, instance, private) -> onepanel:is_oz_panel();

operation_supported(get, instance, private) -> onepanel:is_oz_panel();
operation_supported(get, current_user, private) -> true;
operation_supported(get, current_user_clusters, private) -> true;
operation_supported(get, list, private) -> onepanel:is_oz_panel();

operation_supported(update, instance, private) -> onepanel:is_oz_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(_, _, _) ->
    case onepanel_env:get_cluster_type() of
        oneprovider -> [];
        onezone -> [?SERVICE_OZW, all_healthy]
    end.


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{gri = #gri{id = Id}}) ->
    UserData = middleware_utils:result_from_service_action(
        ?SERVICE_OZ, get_user, #{user_id => Id},
        onezone_users, get_user
    ),
    {ok, {UserData, 1}}.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == current_user;
    As == current_user_clusters;
    As == list
->
    true;

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = instance}}, _) ->
    ok;

validate(#onp_req{
    operation = get, gri = #gri{aspect = Aspect}, client = Client
}, _) when
    Aspect == current_user;
    Aspect == current_user_clusters
->
    % root does not have Onezone user parameters
    Client#client.role == member
        orelse throw(?ERROR_NOT_FOUND),
    ok;

validate(#onp_req{operation = get, gri = #gri{aspect = Aspect}}, _) when
    Aspect == instance;
    Aspect == list
->
    ok;

validate(#onp_req{operation = update, gri = #gri{aspect = instance}}, _) ->
    ok.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = GRI = #gri{aspect = instance}, data = Data}) ->
    case middleware_utils:result_from_service_action(
        ?SERVICE_OZ, add_user, Data,
        onezone_users, add_user
    ) of
        {ok, NewUserId} ->
            {ok, resource, {GRI#gri{id = NewUserId}, {undefined, 1}}};
        Error ->
            Error
    end.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance}}, UserData) ->
    {ok, UserData};

get(#onp_req{gri = #gri{aspect = current_user}, client = Client}, _) ->
    #client{privileges = Privileges, user = User} = Client,
    #user_details{full_name = FullName, id = Id} = User,
    {ok, value, #{
        <<"username">> => FullName, <<"userId">> => Id,
        <<"clusterPrivileges">> => Privileges
    }};

get(#onp_req{gri = #gri{aspect = current_user_clusters}, client = Client}, _) ->
    case clusters:list_user_clusters(Client#client.zone_credentials) of
        {ok, Ids} -> {ok, value, Ids};
        Error -> throw(Error)
    end;

get(#onp_req{gri = #gri{aspect = list}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OZ, list_users, #{},
        onezone_users, list_users
    )}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = instance, id = Id}, data = Data}) ->
    middleware_utils:execute_service_action(?SERVICE_OZ, set_user_password, #{
        user_id => Id, new_password => maps:get(newPassword, Data)
    }).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


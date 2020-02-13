%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_storage type.
%%% All operations handled by this module are available only in op_panel.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
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
% plural 'instances', since many storages are created with one request
operation_supported(create, instances, private) -> onepanel:is_op_panel();

operation_supported(get, list, private) -> onepanel:is_op_panel();
operation_supported(get, instance, private) -> onepanel:is_op_panel();

operation_supported(update, instance, private) -> onepanel:is_op_panel();
operation_supported(update, invalidate_luma_cache, private) -> onepanel:is_op_panel();

operation_supported(delete, instance, private) -> onepanel:is_op_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(get, list, private) -> [?SERVICE_OPW, all_healthy];
required_availability(get, instance, private) -> [?SERVICE_OPW, all_healthy];

required_availability(create, instances, private) -> [?SERVICE_OPW, all_healthy];

required_availability(update, instance, private) -> [?SERVICE_OPW, all_healthy];
required_availability(update, invalidate_luma_cache, private) -> [?SERVICE_OPW, all_healthy];

required_availability(delete, instance, private) -> [?SERVICE_OPW, all_healthy].


-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{gri = #gri{id = StorageId}}) ->
    case op_worker_storage:exists(StorageId) of
        true ->
            Storage = middleware_utils:result_from_service_action(
                ?SERVICE_OPW, get_storages, #{id => StorageId}
            ),
            {ok, {Storage, 1}};
        false ->
            throw(?ERROR_NOT_FOUND)
    end.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = instances}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member},
    gri = #gri{aspect = As}
}, _) when
    As == list;
    As == instance
->
    true;

authorize(#onp_req{
    operation = Op, client = Client, gri = #gri{aspect = As}
}, _) when
    Op == update, As == instance;
    Op == update, As == invalidate_luma_cache;
    Op == delete, As == instance
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = instances}}, _) ->
    ensure_registered();

validate(#onp_req{operation = get, gri = #gri{aspect = As}}, _) when
    As == list;
    As == instance
->
    ensure_registered();

validate(#onp_req{
    operation = update, gri = #gri{aspect = instance}, data = Data
}, CurrentDetails) ->
    ensure_registered(),

    % Swagger spec defines an object to allow for polymorphic storage type.
    % As a result, it is ensured here that only the storage with
    % id specified in path is modified.

    [{OldName, #{type := Type}}] = maps:to_list(Data),

    case CurrentDetails of
        #{name := OldName, type := Type} -> ok;
        #{name := ActualName, type := _} when ActualName /= OldName ->
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(OldName, [ActualName]));
        #{name := OldName, type := ActualType} ->
            Key = str_utils:join_as_binaries([OldName, type], <<".">>),
            throw(?ERROR_BAD_VALUE_NOT_ALLOWED(Key, [ActualType]))
    end;

validate(#onp_req{
    operation = update, gri = #gri{aspect = invalidate_luma_cache}
}, _) ->
    ensure_registered();

validate(#onp_req{operation = delete, gri = #gri{aspect = instance, id = Id}}, _) ->
    ensure_registered(),
    case op_worker_storage:can_be_removed(Id) of
        true -> ok;
        false -> throw(?ERROR_STORAGE_IN_USE)
    end.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = instances}, data = Data}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_storages, #{
        storages => Data
    }).


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = list}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_storages, #{}
    )};

get(#onp_req{gri = #gri{aspect = instance, id = _Id}}, Storage) ->
    {ok, Storage}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = instance, id = Id}, data = Data}) ->
    [{_OldName, Params}] = maps:to_list(Data),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, update_storage, #{id => Id, storage => Params}
    )};

update(#onp_req{gri = #gri{aspect = invalidate_luma_cache, id = Id}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, invalidate_luma_cache, #{id => Id}
    ).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{gri = #gri{aspect = instance, id = Id}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_storage, #{id => Id}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_registered() -> ok | no_return().
ensure_registered() ->
    case service_oneprovider:is_registered() of
        false -> throw(?ERROR_UNREGISTERED_ONEPROVIDER);
        true -> ok
    end.


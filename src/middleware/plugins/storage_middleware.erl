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
-include_lib("ctool/include/logging.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).

-define(DEFAULT_STORAGE_TIMEOUT, 5000).
-define(MIN_STORAGE_TIMEOUT, 1).

-define(DEFAULT_S3_SIGNATURE_VERSION, 4).
-define(ALLOWED_S3_SIGNATURE_VERSIONS, [4]).

-define(DEFAULT_S3_BLOCK_SIZE, 10485760).
-define(MIN_S3_BLOCK_SIZE, 0).

-define(DEFAULT_S3_MAX_CANONICAL_OBJECT_SIZE, 67108864).
-define(MIN_S3_MAX_CANONICAL_OBJECT_SIZE, 1).

-define(STORAGE_KEY(StorageName, Key), str_utils:join_as_binaries([StorageName, Key], <<".">>)).

%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
% plural 'instances', since many storages are created with one request
operation_supported(create, As, private) when
    As == instances;
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    onepanel:is_op_panel();
operation_supported(create, {As, _Id}, private) when
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping
->
    onepanel:is_op_panel();

operation_supported(get, As, private) when
    As == list;
    As == instance;
    As == luma_configuration
->
    onepanel:is_op_panel();
operation_supported(get, {As, _Id}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    onepanel:is_op_panel();

operation_supported(update, instance, private) ->
    onepanel:is_op_panel();
operation_supported(update, {As, _Id}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    onepanel:is_op_panel();

operation_supported(delete, As, private) when
    As == luma_db;
    As == instance
->
    onepanel:is_op_panel();
operation_supported(delete, {As, _Id}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    onepanel:is_op_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(get, As, private) when
    As == list;
    As == instance;
    As == luma_configuration
->
    [?SERVICE_OPW, all_healthy];
required_availability(get, {As, _Id}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    [?SERVICE_OPW, all_healthy];

required_availability(create, As, private) when
    As == instances;
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    [?SERVICE_OPW, all_healthy];
required_availability(create, {As, _Id}, private) when
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping
->
    [?SERVICE_OPW, all_healthy];

required_availability(update, instance, private) ->
    [?SERVICE_OPW, all_healthy];
required_availability(update, {As, _}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    [?SERVICE_OPW, all_healthy];

required_availability(delete, As, private) when
    As == instance;
    As == luma_db
->
    [?SERVICE_OPW, all_healthy];
required_availability(delete, {As, _Id}, private) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    [?SERVICE_OPW, all_healthy].


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
    operation = create, client = Client, gri = #gri{aspect = As}
}, _)  when
    As == instances;
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);
authorize(#onp_req{
    operation = create, client = Client, gri = #gri{aspect = {As, _}}
}, _) when
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == list;
    As == instance;
    As == luma_configuration
->
    true;
authorize(#onp_req{
    operation = get, client = Client, gri = #gri{aspect = {As, _}}
}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    middleware_utils:has_privilege(Client, ?CLUSTER_VIEW);

authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = instance}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);
authorize(#onp_req{
    operation = update, client = Client, gri = #gri{aspect = {As, _Id}}
}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = delete, client = Client, gri = #gri{aspect = As}
}, _) when
    As == luma_db;
    As == instance
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);
authorize(#onp_req{
    operation = delete, client = Client, gri = #gri{aspect = {As, _Id}}
}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).




-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = As}, data = Data}, _) when
    As == instances
->
    ensure_registered(),
    lists:foreach(fun(StorageName) ->
        validate_storage_common_args(StorageName, maps:get(StorageName, Data)),
        validate_storage_custom_args(StorageName, maps:get(StorageName, Data))
    end, maps:keys(Data));

validate(#onp_req{operation = create, gri = #gri{aspect = As}}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    ensure_registered();

validate(#onp_req{operation = create, gri = #gri{
    aspect = {As, _}
}}, _) when
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping
->
    ensure_registered();

validate(#onp_req{operation = get, gri = #gri{aspect = As}}, _) when
    As == list;
    As == instance;
    As == luma_configuration
->
    ensure_registered();
validate(#onp_req{operation = get, gri = #gri{aspect = {As, _Id}}}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    ensure_registered();

validate(#onp_req{
    operation = update, gri = #gri{aspect = instance}, data = Data
}, CurrentDetails) ->
    ensure_registered(),

    lists:foreach(fun(StorageName) ->
        validate_storage_custom_args(StorageName, maps:get(StorageName, Data))
    end, maps:keys(Data)),

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
    operation = update, gri = #gri{aspect = {As, _Id}}}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping
->
    ensure_registered();

validate(#onp_req{
    operation = delete, gri = #gri{aspect = luma_db}
}, _) ->
    ensure_registered();
validate(#onp_req{operation = delete, gri = #gri{aspect = instance, id = Id}}, _) ->
    ensure_registered(),
    case op_worker_storage:can_be_removed(Id) of
        true -> ok;
        false -> throw(?ERROR_STORAGE_IN_USE)
    end;
validate(#onp_req{
    operation = delete, gri = #gri{aspect = {As, _Id}}
}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == local_feed_luma_default_posix_credentials;
    As == local_feed_luma_display_credentials;
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_onedata_user_to_credentials_mapping;
    As == luma_default_posix_credentials;
    As == luma_display_credentials;
    As == luma_uid_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    ensure_registered().


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = instances}, data = Data}) ->
    ActionResults = service:apply_sync(?SERVICE_OPW, add_storages, #{storages => Data}),
    ResponseMap = parse_add_storages_results(ActionResults),
    {ok, value, ResponseMap};

create(#onp_req{
    gri = #gri{
        aspect = local_feed_luma_onedata_user_to_credentials_mapping,
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_onedata_user_to_credentials_mapping, Data#{
        id => StorageId,
        isLocalFeedLumaRequest => true
    });
create(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_default_posix_credentials, SpaceId},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_default_posix_credentials, #{
        id => StorageId,
        spaceId => SpaceId,
        credentials => Data,
        isLocalFeedLumaRequest => true
    });
create(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_display_credentials, SpaceId},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_display_credentials, #{
        id => StorageId,
        spaceId => SpaceId,
        credentials => Data,
        isLocalFeedLumaRequest => true
    });
create(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_uid_to_onedata_user_mapping, Uid},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_uid_to_onedata_user_mapping,
        #{
        id => StorageId,
        uid => convert_uid_to_integer(Uid),
        onedataUser => Data,
        isLocalFeedLumaRequest => true
    });
create(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_acl_user_to_onedata_user_mapping, AclUser},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_acl_user_to_onedata_user_mapping, #{
        id => StorageId,
        aclUser => AclUser,
        onedataUser => Data,
        isLocalFeedLumaRequest => true
    });
create(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_acl_group_to_onedata_group_mapping, AclGroup},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_acl_group_to_onedata_group_mapping, #{
        id => StorageId,
        aclGroup => AclGroup,
        onedataGroup => Data,
        isLocalFeedLumaRequest => true
    }).


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = list}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_storages, #{}
    )};
get(#onp_req{gri = #gri{aspect = instance, id = _Id}}, Storage) ->
    {ok, Storage};
get(#onp_req{gri = #gri{aspect = luma_configuration}}, Storage) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_luma_configuration, #{
            storage => Storage
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, OnedataUserId}, id = StorageId}}, _) when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == luma_onedata_user_to_credentials_mapping
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_onedata_user_to_credentials_mapping, #{
            id => StorageId,
            onedataUserId => OnedataUserId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, SpaceId}, id = StorageId}}, _) when
    As == local_feed_luma_default_posix_credentials;
    As == luma_default_posix_credentials
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_default_posix_credentials, #{
            id => StorageId,
            spaceId => SpaceId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, SpaceId}, id = StorageId}}, _) when
    As == local_feed_luma_display_credentials;
    As == luma_display_credentials
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_display_credentials, #{
            id => StorageId,
            spaceId => SpaceId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, Uid}, id = StorageId}}, _) when
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == luma_uid_to_onedata_user_mapping
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_uid_to_onedata_user_mapping, #{
            id => StorageId,
            uid => convert_uid_to_integer(Uid),
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, AclUser}, id = StorageId}}, _) when
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_acl_user_to_onedata_user_mapping, #{
            id => StorageId,
            aclUser => AclUser,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )};
get(#onp_req{gri = #gri{aspect = {As, AclGroup}, id = StorageId}}, _) when
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, get_acl_group_to_onedata_group_mapping, #{
            id => StorageId,
            aclGroup => AclGroup,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
        }
    )}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = instance, id = Id}, data = Data}) ->
    [{_OldName, Params}] = maps:to_list(Data),
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OPW, update_storage, #{id => Id, storage => Params}
    )};
update(#onp_req{
    gri = #gri{
        aspect = {local_feed_luma_onedata_user_to_credentials_mapping, OnedataUserId},
        id = StorageId
    },
    data = Data
}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, update_user_mapping, #{
            id => StorageId,
            onedataUserId => OnedataUserId,
            storageUser => Data,
            isLocalFeedLumaRequest => true
        }
    ).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{gri = #gri{aspect = luma_db, id = Id}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, clear_luma_db, #{id => Id}
    );
delete(#onp_req{gri = #gri{aspect = instance, id = Id}}) ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_storage, #{id => Id}
    );
delete(#onp_req{gri = #gri{aspect = {As, OnedataUserId}, id = StorageId}})  when
    As == local_feed_luma_onedata_user_to_credentials_mapping;
    As == luma_onedata_user_to_credentials_mapping
->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_onedata_user_to_credentials_mapping, #{
            id => StorageId,
            onedataUserId => OnedataUserId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    });
delete(#onp_req{gri = #gri{aspect = {As, SpaceId}, id = StorageId}}) when
    As == local_feed_luma_default_posix_credentials;
    As == luma_default_posix_credentials
->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_default_posix_credentials, #{
            id => StorageId,
            spaceId => SpaceId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    });
delete(#onp_req{gri = #gri{aspect = {As, SpaceId}, id = StorageId}}) when
    As == local_feed_luma_display_credentials;
    As == luma_display_credentials
->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_display_credentials, #{
            id => StorageId,
            spaceId => SpaceId,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    });
delete(#onp_req{gri = #gri{aspect = {As, Uid}, id = StorageId}}) when
    As == local_feed_luma_uid_to_onedata_user_mapping;
    As == luma_uid_to_onedata_user_mapping
    ->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_uid_to_onedata_user_mapping, #{
            id => StorageId,
            uid => convert_uid_to_integer(Uid),
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    });
delete(#onp_req{gri = #gri{aspect = {As, AclUser}, id = StorageId}}) when
    As == local_feed_luma_acl_user_to_onedata_user_mapping;
    As == luma_acl_user_to_onedata_user_mapping
->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_acl_user_to_onedata_user_mapping, #{
            id => StorageId,
            aclUser => AclUser,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    });
delete(#onp_req{gri = #gri{aspect = {As, AclGroup}, id = StorageId}}) when
    As == local_feed_luma_acl_group_to_onedata_group_mapping;
    As == luma_acl_group_to_onedata_group_mapping
->
    middleware_utils:execute_service_action(
        ?SERVICE_OPW, remove_acl_group_to_onedata_group_mapping, #{
            id => StorageId,
            aclGroup => AclGroup,
            isLocalFeedLumaRequest => is_local_feed_luma_request(As)
    }).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_registered() -> ok | no_return().
ensure_registered() ->
    case service_oneprovider:is_registered() of
        false -> throw(?ERROR_UNREGISTERED_ONEPROVIDER);
        true -> ok
    end.

-spec convert_uid_to_integer(binary()) -> integer().
convert_uid_to_integer(Value) ->
    try
        binary_to_integer(Value)
    catch
        error:badarg ->
            throw(?ERROR_BAD_VALUE_INTEGER(uid))
    end.

-spec is_local_feed_luma_request(atom()) -> boolean().
is_local_feed_luma_request(Aspect) ->
    case atom_to_binary(Aspect, utf8) of
        <<"local_feed_", _/binary>> -> true;
        _ -> false
    end.

-spec parse_add_storages_results(list()) -> map().
parse_add_storages_results(ActionResults) ->
    lists:foldl(fun(StepResult, AccMap) ->
        case StepResult of
            {step_end, _, add_storage, {[{_, {StorageName, {error, Reason}}}], []}} ->
                AccMap#{StorageName => #{<<"error">> => errors:to_json({error, Reason})}};
            {step_end, _, add_storage, {[{_, {StorageName, {ok, StorageId}}}], []}} ->
                AccMap#{StorageName => #{<<"id">> => StorageId}};
            _ ->
                AccMap
        end
    end, #{}, ActionResults).


-spec validate_storage_common_args(binary(), map()) -> ok.
validate_storage_common_args(StorageName, StorageArgs) ->
    Timeout =  maps:get(timeout, StorageArgs, ?DEFAULT_STORAGE_TIMEOUT),
    case Timeout < ?MIN_STORAGE_TIMEOUT of
        true ->
            throw(?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_KEY(StorageName, timeout), ?MIN_STORAGE_TIMEOUT));
        false ->
            ok
    end.


% used during create or update, so the data may include arbitrary parameters
-spec validate_storage_custom_args(binary(), map()) -> ok.
validate_storage_custom_args(StorageName, Data = #{type := <<"s3">>}) ->
    try
        case maps:find(hostname, Data) of
            error -> ok;
            {ok, Hostname} -> url_utils:infer_components(Hostname)
        end
    catch
        _:_ -> throw(?ERROR_BAD_DATA(?STORAGE_KEY(StorageName, <<"hostname">>)))
    end,

    SignatureVersion = maps:get(signatureVersion, Data, ?DEFAULT_S3_SIGNATURE_VERSION),
    case lists:member(SignatureVersion, ?ALLOWED_S3_SIGNATURE_VERSIONS) of
        true -> ok;
        false -> throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(?STORAGE_KEY(StorageName, signatureVersion), ?ALLOWED_S3_SIGNATURE_VERSIONS))
    end,

    BlockSize = maps:get(blockSize, Data, ?DEFAULT_S3_BLOCK_SIZE),
    case BlockSize < ?MIN_S3_BLOCK_SIZE of
        true -> throw(?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_KEY(StorageName, blockSize), ?MIN_S3_BLOCK_SIZE));
        false -> ok
    end,

    MaxCanonicalObjectSize = maps:get(maximumCanonicalObjectSize, Data, ?DEFAULT_S3_MAX_CANONICAL_OBJECT_SIZE),
    case MaxCanonicalObjectSize < ?MIN_S3_MAX_CANONICAL_OBJECT_SIZE of
        true -> throw(?ERROR_BAD_VALUE_TOO_LOW(?STORAGE_KEY(StorageName, maximumCanonicalObjectSize), ?MIN_S3_MAX_CANONICAL_OBJECT_SIZE));
        false -> ok
    end;

validate_storage_custom_args(_StorageName, _Data) ->
    ok.
%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Bartosz Walkowicz
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% TODO VFS-5621
%%% This module handles a requests processing layer common for
%%% graph sync (websocket) and REST interfaces.
%%%
%%% All this operations are carried out by middleware plugins (modules
%%% implementing `middleware_plugin` behaviour). Each such module is responsible
%%% for handling all request pointing to the same entity type (#gri.type field).
%%% @end
%%%-------------------------------------------------------------------
-module(middleware).
-author("Lukasz Opiola").
-author("Bartosz Walkowicz").
-author("Wojciech Geisler").

-include("middleware/middleware.hrl").
-include("authentication.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/logging.hrl").

-type req() :: #onp_req{}.
-type operation() :: gs_protocol:operation().
% The resource the request operates on (creates, gets, updates or deletes).
-type entity() :: undefined | map().
-type versioned_entity() :: gs_protocol:versioned_entity().
-type scope() :: gs_protocol:scope().

-type data() :: gs_protocol:data() | #{atom() => json_utils:json_term()}.
-type data_format() :: gs_protocol:data_format().

-type create_result() :: gs_protocol:graph_create_result().
-type get_result() :: gs_protocol:graph_get_result() | {ok, term()} | {ok, gri:gri(), term()}.
-type delete_result() :: gs_protocol:graph_delete_result().
-type update_result() :: gs_protocol:graph_update_result() | {ok, value, term()}.
-type result() :: create_result() | get_result() | update_result() | delete_result().

-type client() :: #client{}.

% Conditions used to describe when a request can be processed by the cluster.
% - Specifying service name indicates that there must exist a node
%   with the service deployed and the service must have status 'healthy'.
% - 'all_healthy' means that all deployed service nodes must have status 'healthy',
%   does not enforce presence of all services.
-type availability_level() :: all_healthy | service:name().

-export_type([
    client/0,
    req/0,
    operation/0,
    entity/0,
    versioned_entity/0,
    scope/0,
    data/0,
    data_format/0,
    create_result/0,
    get_result/0,
    update_result/0,
    delete_result/0,
    result/0,
    availability_level/0
]).

% Internal record containing the request data and state.
-record(req_ctx, {
    req = #onp_req{} :: req(),
    plugin = undefined :: module(),
    versioned_entity = {undefined, 1} :: versioned_entity()
}).
-type req_ctx() :: #req_ctx{}.

%% API
-export([handle/1, handle/2]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @equiv handle(OnpReq, undefined).
%% @end
%%--------------------------------------------------------------------
-spec handle(req()) -> result().
handle(OnpReq) ->
    handle(OnpReq, {undefined, 1}).


%%--------------------------------------------------------------------
%% @doc
%% Handles an middleware request expressed by a #onp_req{} record.
%% Entity can be provided if it was prefetched.
%% @end
%%--------------------------------------------------------------------
-spec handle(req(), versioned_entity()) -> result().
handle(#onp_req{gri = #gri{type = EntityType}} = OnpReq, VersionedEntity) ->
    try
        ReqCtx0 = #req_ctx{
            req = OnpReq,
            plugin = get_plugin(EntityType),
            versioned_entity = VersionedEntity
        },
        ensure_operation_supported(ReqCtx0),
        ReqCtx1 = sanitize_request(ReqCtx0),
        ensure_availability(ReqCtx1),
        ReqCtx2 = maybe_fetch_entity(ReqCtx1),
        ensure_authorized(ReqCtx2),

        validate_request(ReqCtx2),
        process_request(ReqCtx2)
    catch
        % Intentional errors (throws) are be returned to client as is
        % (e.g. unauthorized, forbidden, space not supported, etc.)
        throw:Error ->
            Error;
        % Unexpected errors are logged and internal server error is returned
        % to client instead
        Type:Reason:Stacktrace ->
            ?error_stacktrace("Unexpected error in ~p - ~p:~p", [
                ?MODULE, Type, Reason
            ], Stacktrace),
            ?ERROR_INTERNAL_SERVER_ERROR
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec get_plugin(gri:entity_type()) -> module() | no_return().
get_plugin(onp_cluster) -> cluster_middleware;
get_plugin(onp_host) -> host_middleware;
get_plugin(onp_panel) -> panel_middleware;
get_plugin(onp_provider) -> provider_middleware;
get_plugin(onp_service) -> service_middleware;
get_plugin(onp_space) -> space_middleware;
get_plugin(onp_storage) -> storage_middleware;
get_plugin(onp_user) -> user_middleware;
get_plugin(onp_zone) -> zone_middleware;
get_plugin(_) -> throw(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures requested operation is supported by calling back
%% proper middleware plugin, throws a proper error if not.
%% @end
%%--------------------------------------------------------------------
-spec ensure_operation_supported(req_ctx()) -> ok | no_return().
ensure_operation_supported(#req_ctx{plugin = Plugin, req = #onp_req{
    operation = Op,
    gri = #gri{aspect = Asp, scope = Scp}
}}) ->
    try Plugin:operation_supported(Op, Asp, Scp) of
        true -> ok;
        false -> throw(?ERROR_NOT_SUPPORTED)
    catch
        error:_ ->
            % No need for log here, 'operation_supported' may crash depending on
            % what the request contains and this is expected.
            throw(?ERROR_NOT_SUPPORTED)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures services required for processing given request are
%% present and healthy.
%% @end
%%--------------------------------------------------------------------
-spec ensure_availability(req_ctx()) -> ok | no_return().
ensure_availability(#req_ctx{plugin = Plugin, req = #onp_req{
    operation = Op,
    gri = #gri{aspect = Asp, scope = Scp}
}}) ->
    Requirements = Plugin:required_availability(Op, Asp, Scp),
    case lists:all(fun is_availability_satisfied/1, Requirements) of
        true -> ok;
        false -> throw(?ERROR_SERVICE_UNAVAILABLE)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sanitizes data specified in request, throws on errors.
%% @end
%%--------------------------------------------------------------------
-spec sanitize_request(req_ctx()) -> req_ctx().
sanitize_request(#req_ctx{req = #onp_req{data_spec = undefined} = OnpReq} = ReqCtx) ->
    ReqCtx#req_ctx{req = OnpReq#onp_req{data = #{}}};

sanitize_request(#req_ctx{req = #onp_req{
    data = Data,
    data_spec = DataSpec
} = Req} = ReqCtx) ->
    ReqCtx#req_ctx{req = Req#onp_req{data = parse_body(Data, DataSpec)}}.



%%--------------------------------------------------------------------
%% @private
%% @doc Parses request body according to provided specification.
%% @end
%%--------------------------------------------------------------------
-spec parse_body(Data :: map(), ArgsSpec :: onepanel_parser:object_spec()) ->
    middleware:data() | no_return().
parse_body(Data, ArgsSpec) ->
    onepanel_parser:parse(Data, ArgsSpec).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves the entity specified in request by calling back proper
%% middleware plugin. Does nothing if the entity is prefetched, or GRI of the
%% request is not related to a specific entity id.
%% @end
%%--------------------------------------------------------------------
-spec maybe_fetch_entity(req_ctx()) -> req_ctx().
maybe_fetch_entity(#req_ctx{versioned_entity = {Entity, _}} = ReqCtx) when Entity /= undefined ->
    ReqCtx;

maybe_fetch_entity(#req_ctx{req = #onp_req{gri = #gri{id = undefined}}} = ReqCtx) ->
    % Skip when creating an instance with predefined Id, set revision to 1
    ReqCtx#req_ctx{versioned_entity = {undefined, 1}};

maybe_fetch_entity(#req_ctx{plugin = Plugin, req = Req} = ReqCtx) ->
    case Plugin:fetch_entity(Req) of
        {ok, {_Entity, _Revision} = VersionedEntity} ->
            ReqCtx#req_ctx{versioned_entity = VersionedEntity};
        undefined ->
            ReqCtx#req_ctx{versioned_entity = {undefined, 1}};
        {error, _} = Error ->
            throw(Error)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensures client specified in request is authorized to perform the request,
%% throws on error.
%% @end
%%--------------------------------------------------------------------
-spec ensure_authorized(req_ctx()) -> ok | no_return().
ensure_authorized(#req_ctx{req = #onp_req{client = #client{role = root}}}) ->
    % Root (emergency passphrase) client is authorized to do everything
    ok;
ensure_authorized(#req_ctx{
    plugin = Plugin,
    versioned_entity = {Entity, _},
    req = #onp_req{operation = Operation, client = Client, gri = GRI} = OnpReq
}) ->
    Service = case onepanel_env:get_cluster_type() of
        ?ONEZONE -> ?OZ_PANEL;
        ?ONEPROVIDER -> ?OP_PANEL
    end,
    #client{auth = Auth} = Client,
    case api_auth:check_authorization(Auth, Service, Operation, GRI) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end,

    Result = try
        Plugin:authorize(OnpReq, Entity)
    catch _:_ ->
        % No need for log here, 'authorize' may crash depending on what the
        % request contains and this is expected.
        false
    end,
    case Result of
        true ->
            ok;
        false ->
            case Client of
                #client{role = guest} ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERROR_UNAUTHORIZED);
                #client{} ->
                    % The client was authenticated but cannot access the
                    % aspect -> forbidden
                    throw(?ERROR_FORBIDDEN)
            end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines if given request can be further processed.
%% @end
%%--------------------------------------------------------------------
-spec validate_request(req_ctx()) -> ok | no_return().
validate_request(#req_ctx{plugin = Plugin, versioned_entity = {Entity, _}, req = Req}) ->
    ok = Plugin:validate(Req, Entity).


%%--------------------------------------------------------------------
%% @doc
%% Handles an middleware request based on operation,
%% should be wrapped in a try-catch.
%% @end
%%--------------------------------------------------------------------
-spec process_request(req_ctx()) -> result().
process_request(#req_ctx{
    plugin = Plugin,
    req = #onp_req{operation = create} = Req
}) ->
    Plugin:create(Req);

process_request(#req_ctx{
    plugin = Plugin,
    req = #onp_req{operation = get} = Req,
    versioned_entity = {Entity, _}
}) ->
    Plugin:get(Req, Entity);

process_request(#req_ctx{
    plugin = Plugin, 
    req = #onp_req{operation = update} = Req
}) ->
    Plugin:update(Req);

process_request(#req_ctx{
    plugin = Plugin, 
    req = #onp_req{operation = delete, client = Client, gri = GRI} = Req
}) ->
    case {Plugin:delete(Req), GRI} of
        {ok, #gri{type = Type, id = Id, aspect = instance}} ->
            % If an entity instance is deleted, log an information about it
            % (it's a significant operation and this information might be useful).
            ?info("~s(~p) has been deleted by client: ~s",
                [Type, Id, client_to_string(Client)]),
            ok;
        {Result, _} ->
            Result
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a readable string representing provided client.
%% @end
%%--------------------------------------------------------------------
-spec client_to_string(middleware:client() | aai:auth()) -> string().
client_to_string(#client{auth = Auth}) -> client_to_string(Auth);
client_to_string(?NOBODY) -> "nobody (unauthenticated client)";
client_to_string(?ROOT) -> "root";
client_to_string(?USER(Id)) -> str_utils:format("user:~s", [Id]).


%% @private
-spec is_availability_satisfied(availability_level()) -> boolean().
is_availability_satisfied(all_healthy) ->
    service:all_healthy();
is_availability_satisfied(Service) ->
    service:is_healthy(Service).

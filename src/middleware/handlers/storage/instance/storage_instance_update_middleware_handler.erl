%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Modifies storage (OP only).
%%% @end
%%%-------------------------------------------------------------------
-module(storage_instance_update_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    init_state/2,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: map().

-record(state, {
    ctx :: middleware_handler:req_ctx(),
    input :: input(),
    storage_details :: op_worker_storage:storage_details()
}).
-type state() :: state().

-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> false | {true, [rest]}.
supported_interfaces() ->
    storage_middleware_handler_utils:supported_interfaces_op().


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    storage_middleware_handler_utils:common_availability().


-callback init_state(middleware_handler:req_ctx(), input()) ->
    {ok, state()} | errors:error().
init_state(OnpReqCtx = #onp_req_ctx{gri = #gri{id = StorageId}}, Input) ->
    case storage_middleware_handler_utils:get_storage(StorageId) of
        {ok, StorageDetails} ->
            #state{ctx = OnpReqCtx, input = Input, storage_details = StorageDetails};
        {error, _} = Error ->
            Error
    end.


-spec preauthorize(state()) -> boolean().
preauthorize(#state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(#state{storage_details = CurrentDetails, input = Data}) ->
    middleware_handler_utils:validate_op_registered(),

    % Validate only storage-specific args; common S3 constraints handled in create
    lists:foreach(fun(StorageName) ->
        storage_middleware_handler_utils:validate_storage_custom_args(
            StorageName, maps:get(StorageName, Data)
        )
    end, maps:keys(Data)),

    % Swagger spec defines an object to allow for polymorphic storage type.
    % As a result, it is ensured here that only the storage with
    % id specified in path is modified.

    [{OldName, #{type := Type}}] = maps:to_list(Data),

    case CurrentDetails of
        #{name := OldName, type := Type} ->
            ok;
        #{name := ActualName, type := _} when ActualName /= OldName ->
            throw(?ERR_BAD_VALUE_NOT_ALLOWED(?err_ctx(), OldName, [ActualName]));
        #{name := OldName, type := ActualType} ->
            Key = str_utils:join_as_binaries([OldName, type], <<".">>),
            throw(?ERR_BAD_VALUE_NOT_ALLOWED(?err_ctx(), Key, [ActualType]))
    end.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = Id}}, input = Data}) ->
    [{_OldName, Params}] = maps:to_list(Data),
    middleware_handler_utils:ok_result(middleware_utils:result_from_service_action(
        ?SERVICE_OPW, update_storage, #{id => Id, storage => Params}
    )).


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Data) ->
    {ok, ?OK_REPLY(Data)}.

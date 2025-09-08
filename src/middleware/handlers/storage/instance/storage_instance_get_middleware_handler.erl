%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns storage details (OP only).
%%% @end
%%%-------------------------------------------------------------------
-module(storage_instance_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: op_worker_storage:storage_details().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    storage_middleware_handler_utils:supported_interfaces_op().


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    storage_middleware_handler_utils:common_availability().


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{id = StorageId}}}) ->
    case op_worker_storage:exists(StorageId) of
        true -> storage_middleware_handler_utils:get_storage(StorageId);
        false -> ?ERROR_NOT_FOUND
    end.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, StorageDetails) ->
    TypeAdjusted = format_storage_details(StorageDetails),
    {ok, ?OK_REPLY(onepanel_utils:convert(TypeAdjusted, {keys, binary}))}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc All storage helper parameters are stored as binaries in op_worker.
%% This function attempts to find their desired type in API model
%% used for creating them and convert accordingly.
%% @end
%%--------------------------------------------------------------------
-spec format_storage_details(#{Key => term()}) ->
    #{Key => term()}.
format_storage_details(StorageDetails) ->
    StorageType = onepanel_utils:get_converted(type, StorageDetails, atom),
    Model = get_storage_model(StorageType),
    TypeMmap = model_to_type_map(Model),
    maps:map(fun
        (Key, Value) when is_binary(Value) ->
            KeyAtom = onepanel_utils:convert(Key, atom),
            case maps:find(KeyAtom, TypeMmap) of
                {ok, string} -> Value;
                {ok, Type} -> onepanel_utils:convert(Value, Type);
                error -> Value
            end;
        (_Key, Value) -> Value
    end, StorageDetails).


%%--------------------------------------------------------------------
%% @private
%% @doc Simplifies API model by stripping optionality modifiers
%% and filtering out more complex specs such as "anyOf".
%% Transforms only top level fields.
%% @end
%%--------------------------------------------------------------------
-spec model_to_type_map(Model :: #{atom() := atom() | tuple()}) ->
    #{Key :: atom() := Type :: atom()}.
model_to_type_map(Model) ->
    maps:fold(fun
        (Key, Type, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, optional}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, required}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (Key, {Type, {optional, _}}, Acc) when is_atom(Type) -> Acc#{Key => Type};
        (_Key, _ValueSpec, Acc) -> Acc
    end, #{}, Model).


%%--------------------------------------------------------------------
%% @private
%% @doc Finds REST model used for creating storage of given type.
%% @end
%%--------------------------------------------------------------------
-spec get_storage_model(StorageType :: atom()) -> onepanel_parser:object_spec().
get_storage_model(posix) -> rest_model:posix_model();
get_storage_model(s3) -> rest_model:s3_model();
get_storage_model(ceph) -> rest_model:ceph_model();
get_storage_model(cephrados) -> rest_model:cephrados_model();
get_storage_model(swift) -> rest_model:swift_model();
get_storage_model(glusterfs) -> rest_model:glusterfs_model();
get_storage_model(nulldevice) -> rest_model:nulldevice_model();
get_storage_model(webdav) -> rest_model:webdav_model();
get_storage_model(http) -> rest_model:http_model();
get_storage_model(xrootd) -> rest_model:xrootd_model();
get_storage_model(nfs) -> rest_model:nfs_model().

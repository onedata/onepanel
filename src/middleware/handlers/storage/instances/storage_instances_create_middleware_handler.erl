%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Adds storages (OP only).
%%% @end
%%%-------------------------------------------------------------------
-module(storage_instances_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").
-include("middleware/middleware.hrl").
-include_lib("ctool/include/http/headers.hrl").

% middleware_handler callbacks
-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    storage_middleware_handler_utils:supported_op_interfaces().


-spec service_availability_requirements(middleware_handler:req_ctx()) ->
    false | {true, [middleware_handler:availability_level()]}.
service_availability_requirements(_) ->
    storage_middleware_handler_utils:common_availability().


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{input = Data}) ->
    middleware_handler_utils:validate_op_registered(),

    lists:foreach(fun(StorageName) ->
        StorageArgs = maps:get(StorageName, Data),
        storage_middleware_handler_utils:validate_storage_common_args(StorageName, StorageArgs),
        storage_middleware_handler_utils:validate_storage_custom_args(StorageName, StorageArgs)
    end, maps:keys(Data)).


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    ActionResults = service:apply_sync(?SERVICE_OPW, add_storages, #{storages => Data}),
    {ok, parse_add_storages_results(ActionResults)}.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, StoragesMap) ->
    case adding_storages_caused_error(StoragesMap) of
        true ->
            {ok, #rest_resp{
                code = ?HTTP_400_BAD_REQUEST,
                headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                body = StoragesMap
            }};
        false ->
            {ok, ?OK_REPLY(StoragesMap)}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


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


%%--------------------------------------------------------------------
%% @private
%% @doc Finds, if any error occurred during adding storages, based on response storages map.
%% @end
%%--------------------------------------------------------------------
-spec adding_storages_caused_error(map()) -> boolean().
adding_storages_caused_error(StoragesMap) ->
    lists:any(fun(StorageStatus) ->
        maps:is_key(<<"error">>, StorageStatus)
    end, maps:values(StoragesMap)).

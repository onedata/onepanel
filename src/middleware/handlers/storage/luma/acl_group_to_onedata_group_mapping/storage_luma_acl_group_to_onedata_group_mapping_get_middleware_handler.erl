%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Returns mapping acl_group -> onedata_group (local or non-local depending on aspect).
%%% TODO VFS-13068 test endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(storage_luma_acl_group_to_onedata_group_mapping_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

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
-type input() :: undefined.
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
    middleware_utils:has_privilege(Client, ?CLUSTER_VIEW).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {Aspect, AclGroup}, id = StorageId}}}) ->
    middleware_handler_utils:service_call(
        ?SERVICE_OPW, get_acl_group_to_onedata_group_mapping, #{
            id => StorageId,
            aclGroup => AclGroup,
            isLocalFeedLumaRequest => case Aspect of
                local_feed_luma_acl_group_to_onedata_group_mapping -> true;
                luma_acl_group_to_onedata_group_mapping -> false
            end
        }
    ).


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Data) ->
    {ok, ?OK_REPLY(Data)}.

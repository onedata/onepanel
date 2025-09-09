%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Deletes mapping acl_user -> onedata_user (local or non-local depending on aspect).
%%% @end
%%%-------------------------------------------------------------------
-module(storage_luma_acl_user_to_onedata_user_mapping_delete_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

% middleware_handler callbacks
-export([
    supported_interfaces/1,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1
]).

-type t() :: ?MODULE.
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: undefined.

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
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {Aspect, AclUser}, id = StorageId}}}) ->
    middleware_handler_utils:service_exec(
        ?SERVICE_OPW, remove_acl_user_to_onedata_user_mapping, #{
            id => StorageId,
            aclUser => AclUser,
            isLocalFeedLumaRequest => case Aspect of
                local_feed_luma_acl_user_to_onedata_user_mapping -> true;
                luma_acl_user_to_onedata_user_mapping -> false
            end
        }
    ).

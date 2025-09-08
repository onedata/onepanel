%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Creates local-feed mapping uid -> onedata_user.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_luma_uid_to_onedata_user_mapping_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").

-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1
]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: undefined.

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


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_op_registered().


-spec process(state()) -> ok | errors:error().
process(#onp_req_state{
    ctx = #onp_req_ctx{gri = #gri{
        aspect = {local_feed_luma_uid_to_onedata_user_mapping, Uid},
        id = StorageId
    }},
    input = Data
}) ->
    middleware_utils:execute_service_action(?SERVICE_OPW, add_uid_to_onedata_user_mapping, #{
        id => StorageId,
        uid => storage_middleware_handler_utils:convert_uid_to_integer(Uid),
        onedataUser => Data,
        isLocalFeedLumaRequest => true
    }).

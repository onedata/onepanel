%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Deploys couchbase instances on new hosts.
%%% @end
%%%-------------------------------------------------------------------
-module(service_couchbase_instances_create_middleware_handler).
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
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: service_executor:task_id().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> {true, [rest]}.
supported_interfaces(_) ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | errors:error().
validate(#onp_req_state{input = Data}) ->
    Hosts = service_middleware_handler_utils:extract_new_hosts(Data),
    service_middleware_handler_utils:validate_hosts_not_existing(?SERVICE_CB, Hosts).


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    Hosts = service_middleware_handler_utils:extract_new_hosts(Data),
    Ctx = kv_utils:copy_found([
        {serverQuota, couchbase_server_quota},
        {bucketQuota, couchbase_bucket_quota}
    ], Data, #{hosts => Hosts}),
    {ok, service:apply_async(?SERVICE_CB, deploy, Ctx)}.


-spec translate_output(state(), output()) -> {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, TaskId) ->
    {ok, ?ASYNC_TASK_REPLY(TaskId)}.

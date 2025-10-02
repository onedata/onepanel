%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for deploying Onezone cluster (batch configuration).
%%% Available on Onezone panels only.
%%% TODO VFS-13075 test endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(zone_cluster_create_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("deployment_progress.hrl").
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

-onp_handles([{create, onp_zone}]).

-type t() :: ?MODULE.
-type input() :: map().
-type state() :: #onp_req_state{input :: input()}.
-type output() :: service_executor:task_id().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces(middleware_handler:req_ctx()) -> false | {true, [rest]}.
supported_interfaces(_) ->
    middleware_handler_utils:if_oz_then([rest]).


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(state()) -> ok | no_return().
validate(#onp_req_state{input = Data}) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> throw(?ERROR_ALREADY_EXISTS);
        false -> ok
    end,
    % This check should be done by the data spec, but swagger's erlang
    % generator is buggy and does not enforce presence of the "onezone" key.
    case maps:find(onezone, Data) of
        {ok, Map} when is_map(Map) -> ok;
        _ -> throw(?ERR_MISSING_REQUIRED_VALUE(?err_ctx(), <<"onezone">>))
    end.


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Data}) ->
    ?notice("Received cluster configuration request with the following batch config:~n~tp", [
        Data
    ]),
    DbHosts = middleware_utils:get_hosts([cluster, databases, nodes], Data),
    CmHosts = middleware_utils:get_hosts([cluster, managers, nodes], Data),
    [MainCmHost] = middleware_utils:get_hosts([cluster, managers, mainNode], Data),
    OzwHosts = middleware_utils:get_hosts([cluster, workers, nodes], Data),
    AllHosts = lists:usort(DbHosts ++ CmHosts ++ OzwHosts),
    ClusterIPs = middleware_utils:get_cluster_ips(Data),

    DbCtx = kv_utils:copy_found([
        {[cluster, databases, serverQuota], couchbase_server_quota},
        {[cluster, databases, bucketQuota], couchbase_bucket_quota}
    ], Data, #{hosts => DbHosts}),

    OpaCtx = maps:get(onepanel, Data, #{}),
    OpaCtx2 = OpaCtx#{
        hosts => AllHosts
    },
    OpaCtx3 = kv_utils:copy_found([
        {[onepanel, interactiveDeployment], interactive_deployment, true},
        {[onepanel, guiDebugMode], gui_debug_mode}
    ], Data, OpaCtx2),

    LeCtx = kv_utils:copy_found([
        {[onezone, letsEncryptEnabled], letsencrypt_enabled}
    ], Data, #{hosts => AllHosts}),

    OzCtx = kv_utils:copy_found([
        {[onezone, name], name},
        {[onezone, domainName], domain},
        {[onezone, builtInDnsServer], [dns_check_config, built_in_dns_server]}
    ], Data),

    OzwCtx = #{
        hosts => OzwHosts, db_hosts => DbHosts, cm_hosts => CmHosts,
        main_cm_host => MainCmHost,
        cluster_ips => ClusterIPs
    },

    OzwCtx2 = kv_utils:copy_found([
        {[onezone, name], onezone_name},
        {[onezone, domainName], onezone_domain},
        {[onezone, users], onezone_users},
        {[onepanel, guiDebugMode], gui_debug_mode}
    ], Data, OzwCtx),

    OzwCtx3 = case Data of
        #{onezone := #{policies := Policies}} ->
            OzwCtx2#{policies => make_policies_ctx(Policies)};
        _ -> OzwCtx2
    end,

    ClusterCtx = #{
        ?SERVICE_PANEL => OpaCtx3,
        ?SERVICE_CB => DbCtx,
        ?SERVICE_CM => #{main_host => MainCmHost,
            hosts => CmHosts, worker_num => length(OzwHosts)},
        ?SERVICE_OZW => OzwCtx3,
        ?SERVICE_LE => LeCtx
    },

    Ctx = #{cluster => ClusterCtx, onezone => OzCtx},
    {ok, service:apply_async(?SERVICE_OZ, deploy, Ctx)}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, TaskId) ->
    {ok, ?ASYNC_TASK_REPLY(TaskId)}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


-spec make_policies_ctx(input()) -> #{atom() => term()}.
make_policies_ctx(Data) ->
    kv_utils:copy_found([
        {oneproviderRegistration, oneprovider_registration},
        {subdomainDelegation, subdomain_delegation},
        {guiPackageVerification, gui_package_verification},
        {harvesterGuiPackageVerification, harvester_gui_package_verification}
    ], Data).

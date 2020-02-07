%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Middleware plugin for the onp_zone type.
%%% Mostly handles operations which are specific to oz_panel and configuring
%%% the deployed Onezone.
%%% An exception is the instance:private resource which is used in op_panel
%%% to fetch information about a remote Onezone.
%%% @end
%%%-------------------------------------------------------------------
-module(zone_middleware).
-author("Wojciech Geisler").

-behaviour(middleware_plugin).

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

%% API
-export([operation_supported/3, required_availability/3, fetch_entity/1,
    authorize/2, validate/2]).
-export([create/1, get/2, update/1, delete/1]).


%%%===================================================================
%%% middleware_plugin callbacks
%%%===================================================================

-spec operation_supported(middleware:operation(), gri:aspect(),
    middleware:scope()) -> boolean().
operation_supported(create, cluster, private) -> onepanel:is_oz_panel();

% get information about a remote onezone - either the issuer of given
% registration token, or the one to which this Oneprovider is registered.
operation_supported(get, instance, private) -> onepanel:is_op_panel();

operation_supported(get, cluster, private) ->  onepanel:is_oz_panel();
operation_supported(get, policies, private) -> onepanel:is_oz_panel();
operation_supported(get, {gui_message, _Id}, private) -> onepanel:is_oz_panel();

operation_supported(update, policies, private) -> onepanel:is_oz_panel();
operation_supported(update, {gui_message, _Id}, private) -> onepanel:is_oz_panel();

operation_supported(_, _, _) -> false.


-spec required_availability(middleware:operation(), gri:aspect(),
    middleware:scope()) -> [middleware:availability_level()].
required_availability(create, cluster, private) -> [];

% get information about a remote onezone
required_availability(get, instance, private) -> [];
required_availability(get, cluster, private) -> [];
required_availability(get, policies, private) ->
    [?SERVICE_OZW, all_healthy];
required_availability(get, {gui_message, _Id}, private) ->
    [?SERVICE_OZW, all_healthy];

required_availability(update, policies, private) ->
    [?SERVICE_OZW, all_healthy];
required_availability(update, {gui_message, _Id}, private) ->
    [?SERVICE_OZW, all_healthy].



-spec fetch_entity(middleware:req()) ->
    {ok, middleware:versioned_entity()} | undefined | errors:error().
fetch_entity(#onp_req{}) ->
    undefined.


-spec authorize(middleware:req(), middleware:entity()) -> boolean().
authorize(#onp_req{client = Client,
    operation = create, gri = #gri{aspect = cluster}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{
    operation = get, client = #client{role = member}, gri = #gri{aspect = As}
}, _) when
    As == instance;
    As == cluster;
    As == policies
->
    true;

authorize(#onp_req{
    operation = get, client = #client{role = member},
    gri = #gri{aspect = {gui_message, _}}
}, _) ->
    true;

authorize(#onp_req{client = Client,
    operation = update, gri = #gri{aspect = policies}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE);

authorize(#onp_req{client = Client,
    operation = update, gri = #gri{aspect = {gui_message, _}}
}, _) ->
    middleware_utils:has_privilege(Client, ?CLUSTER_UPDATE).


-spec validate(middleware:req(), middleware:entity()) -> ok | no_return().
validate(#onp_req{operation = create, gri = #gri{aspect = cluster}}, _) ->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> throw(?ERROR_ALREADY_EXISTS);
        false -> ok
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = instance}, data = Data}, _) ->
    case {service_oneprovider:is_registered(), Data} of
        {true, _} -> ok;
        {false, #{token := _}} -> ok;
        {false, _} -> throw(?ERROR_MISSING_REQUIRED_VALUE(<<"token">>))
    end;

validate(#onp_req{operation = get, gri = #gri{aspect = cluster}, data = Data}, _) ->
    case onepanel_deployment:is_set(?PROGRESS_CLUSTER) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end,
    % This check should be done by the data spec, but swagger's cowboy
    % generator is buggy and does not enforce presence of the "onezone" key.
    case maps:find(onezone, Data) of
        {ok, Map} when is_map(Map) -> ok;
        _ -> throw(?ERROR_MISSING_REQUIRED_VALUE(<<"onezone">>))
    end;

validate(#onp_req{operation = Op, gri = #gri{aspect = policies}}, _) when
    Op == get;
    Op == update
->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end;

validate(#onp_req{
    operation = Op, gri = #gri{aspect = {gui_message, Id}}
}, _) when
    Op == get;
    Op == update
->
    case onepanel_deployment:is_set(?PROGRESS_READY) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end,
    case service:get_hosts(?SERVICE_OZW) /= []
        andalso oz_worker_rpc:gui_message_exists(Id) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec create(middleware:req()) -> middleware:create_result().
create(#onp_req{gri = #gri{aspect = cluster}, data = Data}) ->
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
    {ok, value, _TaskId = service:apply_async(?SERVICE_OZ, deploy, Ctx)}.


-spec get(middleware:req(), middleware:entity()) -> middleware:get_result().
get(#onp_req{gri = #gri{aspect = instance}, data = Data}, _) ->
    Domain = case Data of
        #{token := Token} -> onezone_tokens:read_domain(Token);
        _ -> list_to_binary(service_oneprovider:get_oz_domain())
    end,
    {ok, value, onezone_client:fetch_zone_info(Domain)};

get(#onp_req{gri = #gri{aspect = cluster}}, _) ->
    {ok, value, middleware_utils:format_service_configuration(service_onezone)};

get(#onp_req{gri = #gri{aspect = policies}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OZW, get_policies
    )};

get(#onp_req{gri = #gri{aspect = {gui_message, Id}}}, _) ->
    {ok, value, middleware_utils:result_from_service_action(
        ?SERVICE_OZ, get_gui_message, #{message_id => Id}
    )}.


-spec update(middleware:req()) -> middleware:update_result().
update(#onp_req{gri = #gri{aspect = policies}, data = Data}) ->
    Ctx = make_policies_ctx(Data),
    middleware_utils:execute_service_action(?SERVICE_OZW, set_policies, Ctx);

update(#onp_req{gri = #gri{aspect = {gui_message, Id}}, data = Data}) ->
    Ctx = maps:put(message_id, Id, maps:remove(id, Data)),
    middleware_utils:execute_service_action(
        ?SERVICE_OZ, update_gui_message, Ctx
    ).


-spec delete(middleware:req()) -> middleware:delete_result().
delete(#onp_req{}) ->
    ?ERROR_NOT_SUPPORTED.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Gathers arguments related to Onezone policies.
%% @end
%%--------------------------------------------------------------------
-spec make_policies_ctx(Args :: middleware:data()) -> #{atom() => term()}.
make_policies_ctx(Data) ->
    kv_utils:copy_found([
        {oneproviderRegistration, oneprovider_registration},
        {subdomainDelegation, subdomain_delegation},
        {guiPackageVerification, gui_package_verification},
        {harvesterGuiPackageVerification, harvester_gui_package_verification}
    ], Data).

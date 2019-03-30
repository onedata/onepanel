%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /zone REST resources.
%%%-------------------------------------------------------------------
-module(rest_onezone).
-author("Krzysztof Trzepla").

-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_possible/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

-export([make_policies_ctx/1]).

-define(SERVICE, ?SERVICE_OZ).
-define(WORKER, ?SERVICE_OZW).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = root}}) ->
    {true, Req};

is_authorized(Req, 'GET', #rstate{client = #client{role = member}}) ->
    {true, Req};
is_authorized(Req, _Method, #rstate{client = #client{role = member} = Client}) ->
    {rest_utils:has_privileges(Client, ?CLUSTER_UPDATE), Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = policies}) ->
    {model:exists(onepanel_deployment) andalso
        onepanel_deployment:is_set(?PROGRESS_READY), Req};
exists_resource(Req, _State) ->
    case service:get(?SERVICE) of
        {ok, #service{}} -> {true, Req};
        #error{reason = ?ERR_NOT_FOUND} -> {false, Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_possible/4}
%% @end
%%--------------------------------------------------------------------
accept_possible(Req, _Method, _Args, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, 'GET', #rstate{resource = cluster_ips}) ->
    {true, Req};

is_available(Req, _Method, _State) ->
    {service:all_healthy(), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'PATCH', Args, #rstate{resource = policies}) ->
    Ctx = make_policies_ctx(Args),
    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?WORKER, set_policies, Ctx
    ))};

accept_resource(Req, 'PATCH', Args, #rstate{resource = cluster_ips}) ->
    {ok, ClusterIps} = onepanel_maps:get(hosts, Args),
    Ctx = #{cluster_ips => onepanel_utils:convert(ClusterIps, {keys, list})},

    {true, rest_replier:throw_on_service_error(Req, service:apply_sync(
        ?SERVICE, set_cluster_ips, Ctx
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = policies}) ->
    {rest_replier:format_service_step(service_oz_worker, get_policies,
        service_utils:throw_on_error(service:apply_sync(
            ?WORKER, get_policies, #{}
        ))
    ), Req};

provide_resource(Req, #rstate{resource = cluster_ips}) ->
    {rest_replier:format_service_step(service_onezone, format_cluster_ips,
        service_utils:throw_on_error(service:apply_sync(
            ?SERVICE, format_cluster_ips, #{}
        ))
    ), Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    no_return().
delete_resource(_Req, #rstate{}) ->
    ?throw_error(?ERR_NOT_FOUND).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Gathers arguments related to Onezone policies.
%% @end
%%--------------------------------------------------------------------
-spec make_policies_ctx(Args :: rest_handler:args()) -> #{atom() => term()}.
make_policies_ctx(Args) ->
    onepanel_maps:get_store_multiple([
        {subdomainDelegation, subdomain_delegation}
    ], Args).

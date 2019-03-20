%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling REST resources related to remote
%%% clusters information.
%%%-------------------------------------------------------------------
-module(rest_clusters).
-author("Wojciech Geisler").

-include("names.hrl").
-include("authentication.hrl").
-include("deployment_progress.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_possible/4, is_available/3,
    accept_resource/4, provide_resource/2, delete_resource/2]).

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
is_authorized(Req, _Method, #rstate{resource = Resource, client = Client}) when
    Resource == clusters;
    Resource == cluster ->
    {Client#client.role == user, Req};

is_authorized(Req, _Method, #rstate{client = #client{role = Role}})
    when Role == root; Role == admin; Role == user ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = current_cluster}) ->
    case onepanel_env:get_cluster_type() of
        onezone -> {onepanel_deployment:is_completed(?PROGRESS_CLUSTER), Req};
        oneprovider -> {service_oneprovider:is_registered(#{}), Req}
    end;

exists_resource(Req, #rstate{resource = cluster, client = Client,
    bindings = #{id := ClusterId}}) ->
    {ok, Ids} = clusters:list_user_clusters(Client#client.zone_auth),
    {lists:member(ClusterId, Ids), Req};

exists_resource(Req, _State) ->
    {true, Req}.


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
is_available(Req, _Method, #rstate{resource = Resource}) when
    Resource == current_cluster; Resource == remote_provider ->
    {true, Req};

is_available(Req, _Method, _State) ->
    case onepanel_env:get_cluster_type() of
        oneprovider -> {true, Req};
        onezone -> {service:all_healthy(), Req}
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, _, _, _) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req(), State :: rest_handler:state()}.
provide_resource(Req, #rstate{resource = current_cluster}) ->
    {clusters:get_current_cluster(), Req};

provide_resource(Req, #rstate{resource = remote_provider, bindings = #{id := ProviderId},
    client = #client{zone_auth = Auth}}) ->
    {clusters:fetch_provider_info(Auth, ProviderId), Req};

provide_resource(Req, #rstate{resource = clusters, client = Client}) ->
    {ok, Ids} = clusters:list_user_clusters(Client#client.zone_auth),
    {#{ids => Ids}, Req};

provide_resource(Req, #rstate{resource = cluster, client = Client,
    bindings = #{id := ClusterId}}) ->
    {ok, Result} = clusters:get_details(Client#client.zone_auth, ClusterId),
    {Result, Req}.

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, _) ->
    {false, Req}.

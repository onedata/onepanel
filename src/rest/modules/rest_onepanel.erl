%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /hosts REST resources.
%%%-------------------------------------------------------------------
-module(rest_onepanel).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/logger.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

-define(SERVICE, service_onepanel:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, #rstate{resource = hosts, client = #client{role = undefined}}) ->
    {onepanel_user:count() == 0, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = host, bindings = #{host := Host}}) ->
    {lists:member(Host, service_onepanel:get_hosts()), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'PUT', _Args, #rstate{resource = hosts, params = #{discovered := true}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, deploy, #{hosts => onepanel_discovery:get_hosts()}
    ))};

accept_resource(Req, 'PUT', #{hosts := Hosts}, #rstate{resource = hosts}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, deploy, #{hosts => onepanel_utils:convert(Hosts, {seq, list})}
    ))};

accept_resource(Req, 'PUT', _Args, #rstate{resource = host, bindings = #{host := Host}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, join_cluster, #{hosts => [Host]}
    ))}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = hosts, params = #{discovered := true}}) ->
    Hosts = onepanel_discovery:get_hosts(),
    {[{hosts, lists:sort(onepanel_utils:convert(Hosts, {seq, binary}))}], Req};

provide_resource(Req, #rstate{resource = hosts}) ->
    Hosts = onepanel_cluster:nodes_to_hosts(),
    {[{hosts, lists:sort(onepanel_utils:convert(Hosts, {seq, binary}))}], Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{bindings = #{host := Host}}) ->
    {true, rest_utils:handle_service_action(Req, service_executor:apply_sync(
        ?SERVICE, leave_cluster, #{hosts => [Host]}
    ))}.

%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(rest_cluster_manager).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/logger.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

-define(SERVICE, service_cluster_manager:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:is_authorized/3
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:exists_resource/2
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{bindings = #{host := Host}}) ->
    {service:is_member(?SERVICE, Host), Req};

exists_resource(Req, _State) ->
    {service:exists(?SERVICE), Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:accept_resource/4
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
%%accept_resource(Req, 'PATCH', _Args, #rstate{params = #{<<"started">> := <<"true">>},
%%    bindings = #{host := Host}}) ->
%%    {true, rest_utils:set_results(service_executor:apply_sync(
%%        ?SERVICE, start, #{hosts => [Host]}
%%    ), Req)};
%%
%%accept_resource(Req, 'PATCH', _Args, #rstate{params = #{<<"started">> := <<"false">>},
%%    bindings = #{host := Host}}) ->
%%    {true, rest_utils:set_results(service_executor:apply_sync(
%%        ?SERVICE, stop, #{hosts => [Host]}
%%    ), Req)};
%%
%%accept_resource(Req, 'PATCH', _Args, #rstate{params = #{<<"started">> := <<"true">>}}) ->
%%    {true, rest_utils:set_results(service_executor:apply_sync(
%%        ?SERVICE, stop, #{}
%%    ), Req)};
%%
%%accept_resource(Req, 'PATCH', _Args, #rstate{params = #{<<"started">> := <<"false">>}}) ->
%%    {true, rest_utils:set_results(service_executor:apply_sync(
%%        ?SERVICE, stop, #{}
%%    ), Req)};

accept_resource(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:provide_resource/2
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
%%provide_resource(Req, #rstate{bindings = #{host := Host}}) ->
%%    {rest_utils:format_results(service_executor:apply_sync(
%%        ?SERVICE, status, #{hosts => [Host]}
%%    )), Req};
%%
%%provide_resource(Req, _State) ->
%%    {rest_utils:format_results(service_executor:apply_sync(
%%        ?SERVICE, status, #{}
%%    )), Req};

provide_resource(Req, _State) ->
    {[], Req}.


%%--------------------------------------------------------------------
%% @doc @see rest_behaviour:delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, _State) ->
    {false, Req}.

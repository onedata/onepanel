%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /cluster/workers REST resources.
%%%-------------------------------------------------------------------
-module(rest_op_worker).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([routes/0, is_authorized/4, resource_exists/2, accept_resource/6,
    provide_resource/3, delete_resource/2]).

-define(SERVICE, service_op_worker:name()).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback routes/0
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), rest_handler, State :: rest_handler:rstate()}].
routes() ->
    lists:map(fun({Path, Module, #rstate{} = State}) ->
        {Path, Module, State#rstate{module = ?MODULE}}
    end, rest_cluster_worker:routes()).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback is_authorized/4
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: rest_handler:resource(),
    Method :: rest_handler:method(), Ctx :: rest_handler:ctx(),
    Client :: rest_handler:client()) -> boolean().
is_authorized(Resource, Method, Ctx, Client) ->
    rest_cluster_worker:is_authorized(Resource, Method,
        Ctx#{service => ?SERVICE}, Client).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback resource_exists/2
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> boolean().
resource_exists(Resource, Ctx) ->
    rest_cluster_worker:resource_exists(Resource, Ctx#{service => ?SERVICE}).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback accept_resource/5
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: rest_handler:resource(),
    Method :: rest_handler:accept_method(), Ctx :: rest_handler:ctx(),
    Data :: rest_handler:data(), Client :: rest_handler:client(),
    Req :: cowboy_req:req()) -> {boolean(), cowboy_req:req()} | no_return().
accept_resource(Resource, Method, Ctx, Data, Client, Req) ->
    rest_cluster_worker:accept_resource(Resource, Method,
        Ctx#{service => ?SERVICE}, Data, Client, Req).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback provide_resource/3
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) ->
    Data :: rest_handler:data().
provide_resource(Resource, Ctx, Client) ->
    rest_cluster_worker:provide_resource(Resource, Ctx#{service => ?SERVICE},
        Client).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> no_return().
delete_resource(Resource, Ctx) ->
    rest_cluster_worker:delete_resource(Resource, Ctx#{service => ?SERVICE}).
%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The behavior implemented by different logic handlers behind the REST API.
%%%-------------------------------------------------------------------
-module(rest_behaviour).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").

%%--------------------------------------------------------------------
%% Returns a structure containing routes supported by a module implementing
%% this behavior.
%%--------------------------------------------------------------------
-callback routes() ->
    [{Path :: binary(), rest_handler, State :: rest_handler:rstate()}].


%%--------------------------------------------------------------------
%% Returns a boolean determining if the client is authorized to carry the
%% request on the resource.
%%--------------------------------------------------------------------
-callback is_authorized(Resource :: rest_handler:resource(), 
    Method :: rest_handler:method(), Ctx :: rest_handler:ctx(),
    Client :: rest_handler:client()) -> boolean().


%%--------------------------------------------------------------------
%% Returns whether a resource exists.
%%--------------------------------------------------------------------
-callback resource_exists(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> boolean().


%%--------------------------------------------------------------------
%% Processes data submitted by a client through POST, PUT, PATCH on a REST
%% resource. The callback shall return whether the operation was performed
%% successfully.
%%--------------------------------------------------------------------
-callback accept_resource(Resource :: rest_handler:resource(),
    Method :: rest_handler:accept_method(), Ctx :: rest_handler:ctx(),
    Data :: rest_handler:data(), Client :: rest_handler:client(),
    Req :: cowboy_req:req()) -> {boolean(), cowboy_req:req()} | no_return().


%%--------------------------------------------------------------------
%% Returns data requested by a client through GET request on a REST
%% resource.
%%--------------------------------------------------------------------
-callback provide_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) ->
    Data :: rest_handler:data().


%%--------------------------------------------------------------------
%% Deletes the resource. Returns whether the deletion was successful.
%%--------------------------------------------------------------------
-callback delete_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> boolean().

%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The behavior implemented by different logic handlers behind the REST API.
%%%-------------------------------------------------------------------
-module(rest_behaviour).
-author("Krzysztof Trzepla").

%%--------------------------------------------------------------------
%% Returns a boolean determining if the client is authorized to carry the
%% request on the resource.
%%--------------------------------------------------------------------
-callback is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) -> {Authorized :: boolean(), Req :: cowboy_req:req()}.


%%--------------------------------------------------------------------
%% Returns whether a resource exists.
%%--------------------------------------------------------------------
-callback exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.


%%--------------------------------------------------------------------
%% Checks if current state makes request impossible to fulfill.
%% Called on methods POST, PUT, PATCH and DELETE.
%% Negative response triggers the 409 Conflict http code.
%%--------------------------------------------------------------------
-callback is_conflict(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Possible :: boolean(), Req :: cowboy_req:req()}
    | {stop, Req :: cowboy_req:req()}.


%%--------------------------------------------------------------------
%% Checks if services needed to fulfill the request are healthy.
%% Negative response triggers the 503 Service Unavailable http code.
%%--------------------------------------------------------------------
-callback is_available(Req :: cowboy_req:req(),
    Method :: rest_handler:method_type(), State :: rest_handler:state()) ->
    {Available :: boolean(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req()}.


%%--------------------------------------------------------------------
%% Processes data submitted by a client through POST, PUT, PATCH on a REST
%% resource. The callback shall return whether the operation was performed
%% successfully.
%%--------------------------------------------------------------------
-callback accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req()}.


%%--------------------------------------------------------------------
%% Returns data requested by a client through GET request on a REST
%% resource.
%%--------------------------------------------------------------------
-callback provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()} |
    {stop, Req :: cowboy_req:req(), State :: rest_handler:state()}.


%%--------------------------------------------------------------------
%% Deletes the resource. Returns whether the deletion was successful.
%%--------------------------------------------------------------------
-callback delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
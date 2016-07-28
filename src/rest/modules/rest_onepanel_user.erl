%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(rest_onepanel_user).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("modules/models.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([is_authorized/3, exists_resource/2, accept_resource/4,
    provide_resource/2, delete_resource/2]).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_authorized/3}
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    State :: rest_handler:state()) ->
    {Authorized :: boolean(), Req :: cowboy_req:req()}.
is_authorized(Req, 'DELETE', #rstate{bindings = #{username := Username},
    client = #client{name = Username, role = admin}}) ->
    {false, Req};

is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, 'PUT', #rstate{resource = users, client = #client{role = undefined}}) ->
    {onepanel_user:count() == 0, Req};

is_authorized(Req, _Method, #rstate{resource = user, bindings = #{username := Username},
    client = #client{name = Username}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = user, bindings = #{username := Username}}) ->
    {onepanel_user:exists(Username), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'PUT', #{username := Username, password := Password,
    userRole := Role}, #rstate{resource = users, client = #client{role = ClientRole}}) ->
    NewRole = case ClientRole of
        undefined -> admin;
        _ -> Role
    end,
    onepanel_user:new(Username, Password, NewRole),
    {true, Req};

accept_resource(Req, 'PUT', #{password := Password}, #rstate{resource = user,
    bindings = #{username := Username}}) ->
    onepanel_user:change_password(Username, Password),
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = user, bindings = #{username := Username}}) ->
    {ok, #onepanel_user{uuid = UserId, role = Role}} = onepanel_user:get(Username),
    {[{userId, UserId}, {userRole, Role}], Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{bindings = #{username := Username}}) ->
    onepanel_user:delete(Username),
    {true, Req}.
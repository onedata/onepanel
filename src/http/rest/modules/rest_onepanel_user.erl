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
-export([is_authorized/3, exists_resource/2, is_conflict/4, is_available/3,
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
is_authorized(Req, _Method, #rstate{client = #client{role = admin}}) ->
    {true, Req};

is_authorized(Req, _Method, #rstate{resource = users}) ->
    {onepanel_user:get_by_role(admin) == [], Req};

is_authorized(Req, _Method, #rstate{resource = user, bindings = #{username := Username},
    client = #client{name = Username}}) ->
    {true, Req};

is_authorized(Req, _Method, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:exists_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec exists_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Exists :: boolean(), Req :: cowboy_req:req()}.
exists_resource(Req, #rstate{resource = user, bindings = #{username := Username}}) ->
    {onepanel_user:exists(Username), Req};

exists_resource(Req, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_conflict/4}
%% @end
%%--------------------------------------------------------------------
is_conflict(Req, _Method, _Args, _State) ->
    {false, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:is_available/3}
%% @end
%%--------------------------------------------------------------------
is_available(Req, _Method, _State) ->
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:accept_resource/4}
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Method :: rest_handler:method_type(),
    Args :: rest_handler:args(), State :: rest_handler:state()) ->
    {Accepted :: boolean(), Req :: cowboy_req:req()}.
accept_resource(Req, 'POST', #{username := Username, password := Password,
    userRole := Role}, #rstate{resource = users}) ->
    onepanel_user:create(Username, Password, Role),
    {true, Req};

accept_resource(Req, 'PATCH', Args, #rstate{resource = user,
    client = #client{name = ClientName}, bindings = #{username := Username}}) ->
    #{currentPassword := CurrentPassword, newPassword := NewPassword} = Args,
    {ok, _} = onepanel_user:authenticate(ClientName, CurrentPassword),
    onepanel_user:change_password(Username, NewPassword),
    {true, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:provide_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Data :: rest_handler:data(), Req :: cowboy_req:req()}.
provide_resource(Req, #rstate{resource = users, params = #{role := Role}}) ->
    Users = onepanel_user:get_by_role(onepanel_utils:convert(Role, atom)),
    {format_usernames(Users), Req};
provide_resource(Req, #rstate{resource = users}) ->
    Users = onepanel_user:list(),
    {format_usernames(Users), Req};
provide_resource(Req, #rstate{resource = user, bindings = #{username := Username}}) ->
    {ok, #onepanel_user{uuid = UserId, role = Role}} = onepanel_user:get(Username),
    {#{userId => UserId, userRole => Role}, Req}.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{bindings = #{username := Username}}) ->
    onepanel_user:delete(Username),
    {true, Req}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc Extracts usernames from a list of users.
%% @end
%%--------------------------------------------------------------------
-spec format_usernames(Users :: [#onepanel_user{}]) -> #{usernames := [binary()]}.
format_usernames(Users) ->
    Usernames = lists:map(fun(#onepanel_user{username = Username}) -> Username end, Users),
    #{
        usernames => lists:sort(Usernames)
    }.

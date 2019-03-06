%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(rest_onepanel_user).
-author("Krzysztof Trzepla").

-include("http/rest.hrl").
-include("authentication.hrl").
-include("modules/models.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

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
is_authorized(Req, _Method, #rstate{client = #client{role = Role}})
    when Role == root; Role == admin; Role == user ->
    {true, Req};

is_authorized(Req, _Method, #rstate{resource = users}) ->
    {onepanel_user:get_by_role(admin) == [], Req};

is_authorized(Req, _Method, #rstate{resource = user, bindings = #{username := Username},
    client = #client{user = #user_details{name = Username}}}) ->
    {true, Req};

% resource defaulting to current user
is_authorized(Req, Method, #rstate{resource = current_user, client = #client{role = Role}} = State)
    when Role == user; Role == regular; Role == admin ->
    is_authorized(Req, Method, expand_current_user(State));

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

exists_resource(Req, #rstate{resource = current_user} = State) ->
    exists_resource(Req, expand_current_user(State));

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
    client = #client{user = #user_details{name = ClientName}},
    bindings = #{username := Username}}) ->

    #{currentPassword := CurrentPassword, newPassword := NewPassword} = Args,
    {ok, _} = onepanel_user:authenticate_by_basic_auth(ClientName, CurrentPassword),
    onepanel_user:change_password(Username, NewPassword),
    {true, Req};

accept_resource(Req, 'PATCH', Args, #rstate{resource = current_user} = State) ->
    accept_resource(Req, 'PATCH', Args, expand_current_user(State)).


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
    {#{userId => UserId, userRole => Role, username => Username}, Req};

provide_resource(Req, #rstate{resource = current_user} = State) ->
    provide_resource(Req, expand_current_user(State)).


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour:delete_resource/2}
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rest_handler:state()) ->
    {Deleted :: boolean(), Req :: cowboy_req:req()}.
delete_resource(Req, #rstate{bindings = #{username := Username}}) ->
    onepanel_user:delete(Username),
    {true, Req};

delete_resource(Req, #rstate{resource = current_user} = State) ->
    delete_resource(Req, expand_current_user(State)).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc Extracts usernames from a list of users.
%% @end
%%--------------------------------------------------------------------
-spec format_usernames(Users :: [#onepanel_user{}]) ->
    #{usernames := [onepanel_user:name()]}.
format_usernames(Users) ->
    Names = [Username || #onepanel_user{username = Username} <- Users],
    #{usernames => lists:sort(Names)}.


%%--------------------------------------------------------------------
%% @private
%% @doc Resolves resource "current_user" to the "user" resource
%% adding 'username' binding with the name of the authenticated user.
%% @end
%%--------------------------------------------------------------------
-spec expand_current_user(rest_handler:state()) -> rest_handler:state().
expand_current_user(#rstate{resource = current_user} = State) ->
    Username = get_username(State#rstate.client),
    Bindings = State#rstate.bindings,
    NewBindings = Bindings#{username => Username},
    State#rstate{resource = user, bindings = NewBindings}.


%% @private
-spec get_username(#client{}) -> onepanel_user:name().
get_username(#client{user = #user_details{name = Username}}) -> Username.

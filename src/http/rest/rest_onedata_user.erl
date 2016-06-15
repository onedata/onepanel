%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling logic behind /user REST resources.
%%%-------------------------------------------------------------------
-module(rest_onedata_user).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").
-include("onepanel_modules/logic/db_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% REST behaviour callbacks
-export([routes/0, is_authorized/4, resource_exists/2, accept_resource/6,
    provide_resource/3, delete_resource/3]).

%%%===================================================================
%%% REST behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback routes/0
%%--------------------------------------------------------------------
-spec routes() ->
    [{Path :: binary(), rest_handler, State :: rest_handler:rstate()}].
routes() ->
    State = #rstate{module = ?MODULE},
    Module = rest_handler,
    [
        {<<"/api/v3/onepanel/user">>, Module, State#rstate{resource = user,
            methods = [get, post, put, delete], noauth = [post]}},
        {<<"/api/v3/onepanel/user/:name">>, Module, State#rstate{resource = users,
            methods = [delete]}}
    ].


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback is_authorized/4
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: rest_handler:resource(),
    Method :: rest_handler:method(), Ctx :: rest_handler:ctx(),
    Client :: rest_handler:client()) -> boolean().
is_authorized(user, post, _Ctx, Client) ->
    case {mnesia:table_info(?USER_TABLE, size), Client} of
        {0, _} -> true;
        {_, #client{role = admin}} -> true;
        {_, _} -> false
    end;
is_authorized(user, _Method, _Ctx, _Client) ->
    true;
is_authorized(_Resource, _Method, _Ctx, #client{role = admin}) ->
    true;
is_authorized(_Resource, _Method, _Ctx, _Client) ->
    false.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback resource_exists/2
%%--------------------------------------------------------------------
-spec resource_exists(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx()) -> boolean().
resource_exists(_Resource, _Ctx) ->
    true.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback accept_resource/5
%%--------------------------------------------------------------------
-spec accept_resource(Resource :: rest_handler:resource(),
    Method :: rest_handler:accept_method(), Ctx :: rest_handler:ctx(),
    Data :: rest_handler:data(), Client :: rest_handler:client(),
    Req :: cowboy_req:req()) -> {boolean(), cowboy_req:req()} | no_return().
accept_resource(user, post, _Ctx, Data, #client{role = ClientRole}, Req) ->
    Username = rest_utils:assert_key_type(<<"username">>, Data, binary),
    Password = rest_utils:assert_key_type(<<"password">>, Data, binary),
    Role = case ClientRole of
        undefined -> admin;
        admin -> rest_utils:assert_key_value(<<"userRole">>, Data, atom,
            [admin, regular])
    end,
    case user_logic:create_user(Username, Password, Role) of
        ok -> {true, Req};
        {error, Reason} -> rest_utils:report_error(invalid_request, Reason)
    end;

accept_resource(user, put, _Ctx, Data, #client{name = Name}, Req) ->
    Password = rest_utils:assert_key_type(<<"password">>, Data, binary),
    case user_logic:change_password(Name, Password) of
        ok -> {true, Req};
        {error, Reason} -> rest_utils:report_error(invalid_request, Reason)
    end.


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback provide_resource/3
%%--------------------------------------------------------------------
-spec provide_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) ->
    Data :: rest_handler:data().
provide_resource(user, _Ctx, #client{id = Id, role = Role}) ->
    [{userId, Id}, {userRole, Role}].


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback delete_resource/2
%%--------------------------------------------------------------------
-spec delete_resource(Resource :: rest_handler:resource(),
    Ctx :: rest_handler:ctx(), Client :: rest_handler:client()) -> true.
delete_resource(user, _Ctx, #client{name = Username}) ->
    ok = dao:delete_record(?USER_TABLE, Username),
    true;

delete_resource(users, #{bindings := #{name := Username}}, _Client) ->
    ok = dao:delete_record(?USER_TABLE, Username),
    true.
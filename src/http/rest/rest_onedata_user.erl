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
-include_lib("ctool/include/logging.hrl").

-behavior(rest_behaviour).

%% REST behaviour callbacks
-export([routes/0, is_authorized/4, resource_exists/2, accept_resource/6,
    provide_resource/3, delete_resource/2]).

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
        {<<"/user">>, Module, State#rstate{resource = user,
            methods = [get, post, put], noauth = [post]}}
    ].


%%--------------------------------------------------------------------
%% @doc {@link rest_behaviour} callback is_authorized/4
%%--------------------------------------------------------------------
-spec is_authorized(Resource :: rest_handler:resource(),
    Method :: rest_handler:method(), Ctx :: rest_handler:ctx(),
    Client :: rest_handler:client()) -> boolean().
is_authorized(_Resource, post, _Ctx, #client{role = admin}) ->
    true;
is_authorized(_Resource, post, _Ctx, #client{role = undefined}) ->
    db_manager:is_table_empty(model_logic:table_name(onedata_user));
is_authorized(_Resource, post, _Ctx, _Client) ->
    false;
is_authorized(_Resource, _Method, _Ctx, _Client) ->
    true.


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
    Password = rest_utils:assert_key_type(<<"password">>, Data, base64),
    Role = case ClientRole of
        undefined -> admin;
        admin -> rest_utils:assert_key_value(<<"userRole">>, Data, atom,
            [admin, regular])
    end,
    case onedata_user:new(Username, Password, Role) of
        ok -> {true, Req};
        {error, already_exists} ->
            rest_utils:report_error(invalid_request,
                <<"user '", Username/binary, "' already exists">>)
    end;

accept_resource(user, put, _Ctx, Data, #client{name = Name}, Req) ->
    Password = rest_utils:assert_key_type(<<"password">>, Data, base64),
    case onedata_user:change_password(Name, Password) of
        ok -> {true, Req};
        {error, {password, {too_short, MinLength}}} ->
            rest_utils:report_error(invalid_value,
                <<"new password should be at least ",
                    (integer_to_binary(MinLength))/binary, " characters long">>)
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
    Ctx :: rest_handler:ctx()) -> no_return().
delete_resource(_Resource, _Ctx) ->
    rest_utils:report_error(invalid_request).
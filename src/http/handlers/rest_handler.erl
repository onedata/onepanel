%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc The module handling the common RESTful logic. It implements
%%% Cowboy's rest pseudo-behavior, delegating specifics to submodules.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("Krzysztof Trzepla").

-include("http/handlers/rest.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/3, rest_init/2, allowed_methods/2, content_types_accepted/2,
    content_types_provided/2, is_authorized/2, forbidden/2, resource_exists/2,
    delete_resource/2, accept_resource_json/2, provide_resource/2]).

-type accept_method() :: post | patch | put.
-type method() :: accept_method() | get | delete.
-type resource() :: atom().
-type ctx() :: #{}.
-type data() :: proplists:proplist().
-type client() :: #client{}.
-type rstate() :: #rstate{}.

-export_type([accept_method/0, method/0, resource/0, ctx/0, data/0, client/0,
    rstate/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Upgrade the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
-spec init({TransportName :: atom(), ProtocolName :: http},
    Req :: cowboy_req:req(), Opts :: any()) -> {upgrade, protocol, cowboy_rest}.
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Initialize the state for this request.
%% @end
%%--------------------------------------------------------------------
-spec rest_init(Req :: cowboy_req:req(), Opts :: rstate()) ->
    {ok, cowboy_req:req(), rstate()}.
rest_init(Req, #rstate{} = Opts) ->
    {ok, Req, Opts}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: rstate()) ->
    {[binary()], cowboy_req:req(), rstate()}.
allowed_methods(Req, #rstate{methods = Methods} = State) ->
    AllowedMethods = lists:map(fun
        (post) -> <<"POST">>;
        (patch) -> <<"PATCH">>;
        (get) -> <<"GET">>;
        (put) -> <<"PUT">>;
        (delete) -> <<"DELETE">>
    end, Methods),
    {AllowedMethods, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether POST is allowed when the resource doesn't exist.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: rstate()) ->
    {[{binary(), atom()}], cowboy_req:req(), rstate()}.
content_types_accepted(Req, #rstate{} = State) ->
    {[{<<"application/json">>, accept_resource_json}], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of content-types the resource provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: rstate()) ->
    {[{binary(), atom()}], cowboy_req:req(), rstate()}.
content_types_provided(Req, #rstate{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the user is authorized to perform the action.
%% NOTE: The name and description of this function is actually misleading;
%% 401 Unauthorized is returned when there's been an *authentication* error,
%% and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: rstate()) ->
    {true | {false, binary()}, cowboy_req:req(), rstate()}.
is_authorized(Req, #rstate{noauth = Methods} = State) ->
    case authorize_by_basic_auth(Req, State) of
        {true, NewState} -> {true, Req, NewState};
        false ->
            Method = get_method(Req),
            case lists:member(Method, Methods) of
                true ->
                    {true, Req, State#rstate{client = #client{}}};
                false ->
                    {{false, <<"">>}, Req, State}
            end
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether access to the resource is forbidden.
%% @see is_authorized/2
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
forbidden(Req, #rstate{module = Module, resource = Resource, client = Client} =
    State) ->
    Ctx = get_ctx(Req),
    Method = get_method(Req),
    Forbidden = not Module:is_authorized(Resource, Method, Ctx, Client),
    {Forbidden, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return whether the resource exists.
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
resource_exists(Req, #rstate{module = Module, resource = Resource} = State) ->
    Ctx = get_ctx(Req),
    Exists = Module:resource_exists(Resource, Ctx),
    {Exists, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Delete the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
delete_resource(Req, #rstate{module = Mod, resource = Resource, client = Client}
    = State) ->
    Ctx = get_ctx(Req),
    Deleted = Mod:delete_resource(Resource, Ctx, Client),
    {Deleted, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body of application/json content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_json(Req :: cowboy_req:req(), State :: rstate()) ->
    {boolean(), cowboy_req:req(), rstate()}.
accept_resource_json(Req, #rstate{} = State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    Result = try
        {ok, json_utils:decode(Body)}
    catch
        _:Reason ->
            ?debug_stacktrace("Cannot decode request body due to: ~p", [Reason]),
            {error, {invalid_request, <<"malformed JSON data">>}}
    end,

    case Result of
        {ok, Data} ->
            accept_resource(Data, Req2, State);
        {error, Reason2} ->
            {false, rest_utils:set_error_response(Reason2, Req2), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: rstate()) ->
    {iodata(), cowboy_req:req(), rstate()}.
provide_resource(Req, #rstate{module = Module, resource = Resource,
    client = Client} = State) ->
    Ctx = get_ctx(Req),
    Data = Module:provide_resource(Resource, Ctx, Client),
    Json = json_utils:encode(Data),
    {Json, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts REST method from binary to an atom representation.
%%--------------------------------------------------------------------
-spec get_method(Req :: cowboy_req:req()) -> Method :: method().
get_method(Req) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, _} -> post;
        {<<"PATCH">>, _} -> patch;
        {<<"GET">>, _} -> get;
        {<<"PUT">>, _} -> put;
        {<<"DELETE">>, _} -> delete
    end.


%%--------------------------------------------------------------------
%% @doc Returns request bindings.
%%--------------------------------------------------------------------
-spec get_ctx(Req :: cowboy_req:req()) -> Ctx :: ctx().
get_ctx(Req) ->
    {Bindings, _} = cowboy_req:bindings(Req),
    NewBindings = case lists:keyfind(host, 1, Bindings) of
        {_, Host} ->
            lists:keyreplace(host, 1, Bindings, {host, erlang:binary_to_list(Host)});
        false ->
            Bindings
    end,
    {QsVals, _} = cowboy_req:qs_vals(Req),
    #{bindings => maps:from_list(NewBindings), qs_vals => maps:from_list(QsVals)}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Process the request body.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Data :: data(), Req :: cowboy_req:req(), State :: rstate()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), rstate()}.
accept_resource(Data, Req, #rstate{module = Module, resource = Resource,
    client = Client} = State) ->
    Ctx = get_ctx(Req),
    Method = get_method(Req),

    try
        {Result, Req2} = Module:accept_resource(Resource, Method, Ctx, Data,
            Client, Req),
        {Result, Req2, State}
    catch
        {rest_error, Reason} ->
            {false, rest_utils:set_error_response(Reason, Req), State}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Authorizes user by basic auth (username + password).
%% @end
%%--------------------------------------------------------------------
-spec authorize_by_basic_auth(Req :: cowboy_req:req(), State :: rstate()) ->
    {true, NewState :: rstate()} | false.
authorize_by_basic_auth(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        {<<"Basic ", Hash/binary>>, _} ->
            [Username, Password] = binary:split(base64:decode(Hash), <<":">>),
            case user_logic:authenticate(Username, Password) of
                {ok, #?USER_RECORD{uuid = Uuid, role = Role}} ->
                    Client = #client{name = Username, id = Uuid, role = Role},
                    {true, State#rstate{client = Client}};
                _ ->
                    false
            end;
        _ ->
            false
    end.

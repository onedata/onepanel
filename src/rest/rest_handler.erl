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

-include("http/rest.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

%% API
-export([init/3, rest_init/2, allowed_methods/2, content_types_accepted/2,
    content_types_provided/2, is_authorized/2, forbidden/2, resource_exists/2,
    delete_resource/2, accept_resource_json/2, accept_resource_yaml/2,
    provide_resource/2]).

-type version() :: non_neg_integer().
-type accept_method_type() :: 'POST' | 'PATCH' | 'PUT'.
-type method_type() :: accept_method_type() | 'GET' | 'DELETE'.
-type resource() :: atom().
-type data() :: onepanel_parser:data().
-type bindings() :: #{Key :: atom() => Value :: term()}.
-type params() :: #{Key :: atom() => Value :: term()}.
-type args() :: onepanel_parser:args().
-type spec() :: onepanel_parser:spec().
-type client() :: #client{}.
-type state() :: #rstate{}.
-type method() :: #rmethod{}.

-export_type([version/0, accept_method_type/0, method_type/0, resource/0,
    data/0, bindings/0, params/0, args/0, spec/0, client/0, state/0,
    method/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Upgrades the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
-spec init(Type :: {any(), http}, Req :: cowboy_req:req(), Opts :: any()) ->
    {upgrade, protocol, cowboy_rest}.
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Initializes the state for this request.
%% @end
%%--------------------------------------------------------------------
-spec rest_init(Req :: cowboy_req:req(), State :: state()) ->
    {ok, cowboy_req:req(), state()}.
rest_init(Req, #rstate{} = State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, #rstate{methods = Methods} = State) ->
    AllowedMethods = lists:map(fun(#rmethod{type = Type}) ->
        onepanel_utils:convert(Type, binary)
    end, Methods),
    {AllowedMethods, Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of content-types the resource
%% accepts.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #rstate{} = State) ->
    {[
        {<<"application/json">>, accept_resource_json},
        {<<"application/x-yaml">>, accept_resource_yaml}
    ], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of content-types the resource
%% provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #rstate{} = State) ->
    {[{<<"application/json">>, provide_resource}], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns whether the user is authorized to
%% perform the action. The name and description of this function is actually
%% misleading; 401 Unauthorized is returned when there's been an *authentication*
%% error, and 403 Forbidden is returned when the already-authenticated client
%% is unauthorized to perform an operation.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {true | {false, binary()}, cowboy_req:req(), state()}.
is_authorized(Req, #rstate{methods = Methods} = State) ->
    AuthMethods = [
        fun authenticate_by_basic_auth/1,
        fun authenticate_by_cookie/1
    ],
    case authenticate(Req, AuthMethods) of
        {{true, User, SessionId}, Req3} ->
            #onepanel_user{username = Username, role = Role, uuid = Uuid} = User,
            Client = #client{
                name = Username, id = Uuid, role = Role, session_id = SessionId
            },
            {true, Req3, State#rstate{client = Client}};
        {false, Req3} ->
            {Method, Req4} = rest_utils:get_method(Req3),
            case lists:keyfind(Method, 2, Methods) of
                #rmethod{noauth = true} ->
                    Req5 = cowboy_req:set_resp_body(<<>>, Req4),
                    {true, Req5, State#rstate{client = #client{}}};
                _ ->
                    {{false, <<"">>}, Req4, State}
            end
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns whether access to the resource is
%% forbidden ({@link  is_authorized/2}).
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
forbidden(Req, #rstate{module = Module, methods = Methods} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        #rmethod{params_spec = Spec} = lists:keyfind(Method, 2, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        {Authorized, Req5} = Module:is_authorized(Req4, Method, State#rstate{
            bindings = Bindings,
            params = Params
        }),
        {not Authorized, Req5, State}
    catch
        Type:Reason ->
            {true, rest_replier:handle_error(Req, Type, ?make_error(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns whether the resource exists.
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, #rstate{module = Module, methods = Methods} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        #rmethod{params_spec = Spec} = lists:keyfind(Method, 2, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        {Exists, Req5} = Module:exists_resource(Req4, State#rstate{
            bindings = Bindings,
            params = Params
        }),
        {Exists, Req5, State}
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_error(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Processes the request body of application/json
%% content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_json(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
accept_resource_json(Req, #rstate{} = State) ->
    try
        {ok, Body, Req2} = cowboy_req:body(Req),
        Data = json_utils:decode(Body),
        accept_resource(Req2, Data, State)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_error(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Processes the request body of application/x-yaml
%% content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_yaml(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
accept_resource_yaml(Req, #rstate{} = State) ->
    try
        {ok, Body, Req2} = cowboy_req:body(Req),
        [Data] = yamerl_constr:string(Body),
        Data2 = adjust_yaml_data(Data),
        accept_resource(Req2, Data2, State)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_error(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Provides the resource.
%% @end
%%--------------------------------------------------------------------
-spec provide_resource(Req :: cowboy_req:req(), State :: state()) ->
    {iodata(), cowboy_req:req(), state()}.
provide_resource(Req, #rstate{module = Module, methods = Methods} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        #rmethod{params_spec = Spec} = lists:keyfind(Method, 2, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        case Module:provide_resource(Req4, State#rstate{
            bindings = Bindings,
            params = Params
        }) of
            {Data, Req5} ->
                Json = json_utils:encode(Data),
                {Json, Req5, State};
            {halt, Req5, State} ->
                {halt, Req5, State}
        end
    catch
        Type:Reason ->
            {halt, rest_replier:reply_with_error(Req, Type, ?make_error(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Deletes the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, #rstate{module = Module, methods = Methods} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        #rmethod{params_spec = Spec} = lists:keyfind(Method, 2, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        {Deleted, Req5} = Module:delete_resource(Req4, State#rstate{
            bindings = Bindings,
            params = Params
        }),
        {Deleted, Req5, State}
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_error(Reason)), State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Cowboy callback function. Processes the request body.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Data :: data(), State :: state()) ->
    {{true, URL :: binary()} | boolean(), cowboy_req:req(), state()}.
accept_resource(Req, Data, #rstate{module = Module, methods = Methods} =
    State) ->
    {Method, Req2} = rest_utils:get_method(Req),
    {Bindings, Req3} = rest_utils:get_bindings(Req2),
    #rmethod{params_spec = ParamSpec, args_spec = ArgsSpec} =
        lists:keyfind(Method, 2, Methods),
    {Params, Req4} = rest_utils:get_params(Req3, ParamSpec),
    Args = rest_utils:get_args(Data, ArgsSpec),
    {Accepted, Req5} = Module:accept_resource(Req4, Method, Args, State#rstate{
        bindings = Bindings,
        params = Params
    }),
    {Accepted, Req5, State}.


%%--------------------------------------------------------------------
%% @private @doc Authenticates user using provided authorization methods.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Req :: cowboy_req:req(), Methods :: [fun()]) ->
    {{true, User :: #onepanel_user{}, SessionId ::
    undefined | onepanel_session:id()} | false, Req :: cowboy_req:req()}.
authenticate(Req, []) ->
    {false, Req};
authenticate(Req, [AuthMethod | AuthMethods]) ->
    try AuthMethod(Req) of
        {{ok, User}, SessionId, Req2} ->
            {{true, User, SessionId}, Req2};
        {#error{} = Error, _SessionId, Req2} ->
            {false, rest_replier:handle_error(Req2, error, Error)};
        {ignore, _SessionId, Req2} ->
            authenticate(Req2, AuthMethods)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_error(Reason))}
    end.

%%--------------------------------------------------------------------
%% @private @doc Authenticates user using basic authorization method.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Req :: cowboy_req:req()) ->
    {{ok, User :: #onepanel_user{}} | #error{} | ignore, SessionId ::
    undefined | onepanel_session:id(), Req :: cowboy_req:req()}.
authenticate_by_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        {<<"Basic ", Hash/binary>>, Req2} ->
            [Username, Password] = binary:split(base64:decode(Hash), <<":">>),
            {onepanel_user:authenticate(Username, Password), undefined, Req2};
        {_, Req2} ->
            {ignore, undefined, Req2}
    end.


%%--------------------------------------------------------------------
%% @private @doc Authenticates user using session cookie.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_cookie(Req :: cowboy_req:req()) ->
    {{ok, User :: #onepanel_user{}} | #error{} | ignore, SessionId ::
    undefined | onepanel_session:id(), Req :: cowboy_req:req()}.
authenticate_by_cookie(Req) ->
    case cowboy_req:cookie(<<"sessionId">>, Req) of
        {undefined, Req2} ->
            {ignore, undefined, Req2};
        {SessionId, Req2} ->
            {onepanel_user:authenticate(SessionId), SessionId, Req2}
    end.


%%--------------------------------------------------------------------
%% @private @doc Adjust data, that has been returned by YAML parser, by
%% converting each parameter into binary.
%% @end
%%--------------------------------------------------------------------
-spec adjust_yaml_data(Data :: data()) -> NewData :: data().
adjust_yaml_data(Data) ->
    case {io_lib:printable_unicode_list(Data), erlang:is_list(Data)} of
        {false, true} -> lists:map(fun
            ({Key, Value}) ->
                {onepanel_utils:convert(Key, binary), adjust_yaml_data(Value)};
            (Value) ->
                onepanel_utils:convert(Value, binary)
        end, Data);
        {_, _} -> onepanel_utils:convert(Data, binary)
    end.

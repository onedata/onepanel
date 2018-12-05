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
-export([init/2, allowed_methods/2, content_types_accepted/2,
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
-spec init(Req :: cowboy_req:req(), State :: state()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.


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
            {true, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason)), State}
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
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Processes the request body of application/json
%% content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_json(Req :: cowboy_req:req(), State :: state()) ->
    {boolean() | stop, cowboy_req:req(), state()}.
accept_resource_json(Req, #rstate{} = State) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        Data = json_utils:decode(Body),
        accept_resource(Req2, Data, State)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason)), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Processes the request body of application/x-yaml
%% content type.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource_yaml(Req :: cowboy_req:req(), State :: state()) ->
    {boolean() | stop, cowboy_req:req(), state()}.
accept_resource_yaml(Req, #rstate{} = State) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        [Data] = yamerl_constr:string(Body),
        Data2 = json_utils:list_to_map(adjust_yaml_data(Data)),
        accept_resource(Req2, Data2, State)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason)), State}
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
            {stop, Req5, State} ->
                {stop, Req5, State}
        end
    catch
        Type:Reason ->
            {stop, rest_replier:reply_with_error(Req, Type, ?make_stacktrace(Reason)), State}
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
        case Module:is_conflict(Req4, Method, #{}, State#rstate{
            bindings = Bindings,
            params = Params
        }) of
            {false, Req5} ->
                {Deleted, Req6} = Module:delete_resource(Req4, State#rstate{
                    bindings = Bindings,
                    params = Params
                }),
                {Deleted, Req6, State};
            {true, Req5} ->
                {stop, cowboy_req:reply(409, Req5), State}
        end
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason)), State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Cowboy callback function. Processes the request body.
%% @end
%%--------------------------------------------------------------------
-spec accept_resource(Req :: cowboy_req:req(), Data :: data(), State :: state()) ->
    {{true, URL :: binary()} | boolean() | stop, cowboy_req:req(), state()}.
accept_resource(Req, Data, #rstate{module = Module, methods = Methods} =
    State) ->
    {Method, Req2} = rest_utils:get_method(Req),
    {Bindings, Req3} = rest_utils:get_bindings(Req2),
    #rmethod{params_spec = ParamSpec, args_spec = ArgsSpec} =
        lists:keyfind(Method, 2, Methods),
    {Params, Req4} = rest_utils:get_params(Req3, ParamSpec),
    Args = rest_utils:get_args(Data, ArgsSpec),

    case Module:is_conflict(Req4, Method, Args, State#rstate{
        bindings = Bindings,
        params = Params
    }) of
        {false, Req5} ->
            {Result, Req6} = Module:accept_resource(Req5, Method, Args, State#rstate{
                bindings = Bindings,
                params = Params
            }),
            {Result, Req6, State};
        {true, Req5} ->
            {stop, cowboy_req:reply(409, Req5), State}
    end.


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
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason))}
    end.

%%--------------------------------------------------------------------
%% @private @doc Authenticates user using basic authorization method.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Req :: cowboy_req:req()) ->
    {Result, SessionId :: undefined, Req :: cowboy_req:req()}
    when Result :: {ok, User :: #onepanel_user{}} | #error{} | ignore.
authenticate_by_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Basic ", Hash/binary>> ->
            [Username, Password] = binary:split(base64:decode(Hash), <<":">>),
            {onepanel_user:authenticate(Username, Password), undefined, Req};
        _ ->
            {ignore, undefined, Req}
    end.


%%--------------------------------------------------------------------
%% @private @doc Authenticates user using session cookie.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_cookie(Req :: cowboy_req:req()) ->
    {{ok, User :: #onepanel_user{}}, onepanel_session:id(), Req}
    | {#error{}, onepanel_session:id(), Req}
    | {ignore, undefined, Req}
    when Req :: onepanel_session:req().
authenticate_by_cookie(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case proplists:get_all_values(<<"sessionId">>, Cookies) of
        [] ->
            {ignore, undefined, Req};
        [SessionId] ->
            {onepanel_user:authenticate(SessionId), SessionId, Req};
        [SessionId1, SessionId2] ->
            % Since 18.02.0-beta2 up to 18.02.0-rc11 a bug existed causing
            % session cookie to be set with path "/api/v3/onepanel/" instead of "/".
            % After upgrading to fixed version using the "/" path the old cookie
            % persists and both are sent in each request.
            % Without the code below logging in would not work in such situation
            % without manually cleaning cookies.

            case onepanel_user:authenticate(SessionId1) of
                {ok, _} = Result -> {Result, SessionId1, Req};
                #error{} -> {onepanel_user:authenticate(SessionId2), SessionId2, Req}
            end
    end.


%%--------------------------------------------------------------------
%% @private @doc Adjust data, that has been returned by YAML parser, by
%% converting each parameter into binary.
%% @end
%%--------------------------------------------------------------------
-spec adjust_yaml_data(Data :: proplists:proplist()) ->
    NewData :: proplists:proplist().
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

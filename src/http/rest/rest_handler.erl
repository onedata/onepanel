%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
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

-include("authentication.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").

%% API
-export([init/2, allowed_methods/2, content_types_accepted/2,
    content_types_provided/2, is_authorized/2, forbidden/2, resource_exists/2,
    delete_resource/2, accept_resource_json/2, accept_resource_yaml/2,
    provide_resource/2]).
-export([service_available/2]).

-type version() :: non_neg_integer().
-type accept_method_type() :: 'POST' | 'PATCH' | 'PUT'.
-type method_type() :: accept_method_type() | 'GET' | 'DELETE'.
-type resource() :: atom().
-type data() :: onepanel_parser:data() | {binary, binary()}.
-type bindings() :: #{Key :: atom() => Value :: term()}.
-type params() :: #{Key :: atom() => Value :: term()}.
-type args() :: onepanel_parser:args().
-type spec() :: onepanel_parser:object_spec().
-type client() :: #client{}.
-type state() :: #rstate{}.
-type method() :: #rmethod{}.
-type privilege() :: privileges:cluster_privilege().

%% Objects used to authenticate request to Onezone
%% 'none' is used if proper authentication object could not be obtained
-type zone_auth() :: rpc_auth() | rest_auth() | none.
%% Used by oz_panel
-type rpc_auth() :: {rpc, aai:auth()}.
%% Used by op_panel
-type rest_auth() :: {rest, oz_plugin:auth()}.

-export_type([zone_auth/0, rpc_auth/0, rest_auth/0]).


-export_type([version/0, accept_method_type/0, method_type/0, resource/0,
    data/0, bindings/0, params/0, args/0, spec/0, client/0, state/0,
    method/0, privilege/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Upgrades the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), State :: state()) ->
    {cowboy_rest | ok, Req :: cowboy_req:req(), State :: state()}.
init(Req, Opts) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> handle_options(Req, Opts);
        _ -> {cowboy_rest, Req, Opts}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Returns whether services needed to fulfill the request are available.
%% Negative result of this function triggers 503 Service Unavailable error.
%% @end
%%--------------------------------------------------------------------
-spec service_available(Req :: cowboy_req:req(), State :: state()) ->
    {boolean(), cowboy_req:req(), state()}.
service_available(Req, #rstate{methods = Methods, module = Module} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        case lists:keyfind(Method, 2, Methods) of
            #rmethod{params_spec = Spec} ->
                {Params, Req4} = rest_utils:get_params(Req3, Spec),
                State2 = State#rstate{bindings = Bindings, params = Params},

                {Available, Req} = Module:is_available(Req4, Method, State2),
                {Available, Req, State};
            false ->
                % continue processing as it will fail anyway on allowed methods check
                % triggering more descriptive error
                {true, Req, State}
        end
    catch
        _Type:_Reason ->
            % exceptions in the is_available callbacks are usually caused
            % by incorrect request method or params which will be
            % handled with proper error code later on
            {true, Req, State}
    end.

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
    case cowboy_req:has_body(Req) of
        true -> {[
            {<<"application/json">>, accept_resource_json},
            {<<"application/x-yaml">>, accept_resource_yaml}
        ], Req, State};
        false ->
            {[{'*', accept_resource_json}], Req, State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of content-types the resource
%% provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #rstate{produces = Produces} = State) ->
    {lists:map(fun(ContentType) -> {ContentType, provide_resource} end, Produces), Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Handles authentication of the user.
%% Negative result of this function triggers 401 Unauthorized which
%% is http status used to describe authentication errors.
%%
%% Resource methods marked as "noauth" will never cause failure
%% of this function even if user does provide credentials in the request,
%% thus never triggering 401 code for such resources.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {true | {false, binary()} | stop, cowboy_req:req(), state()}.
is_authorized(Req, #rstate{methods = Methods} = State) ->
    AuthMethods = [
        fun rest_auth:authenticate_by_basic_auth/1,
        fun rest_auth:authenticate_by_onepanel_auth_token/1,
        fun rest_auth:authenticate_by_onezone_auth_token/1
    ],
    case rest_auth:authenticate(Req, AuthMethods) of
        {{true, Client}, Req2} ->
            {true, Req2, State#rstate{client = Client}};
        {{false, Error}, Req3} ->
            {Method, Req4} = rest_utils:get_method(Req3),
            case lists:keyfind(Method, #rmethod.type, Methods) of
                #rmethod{noauth = true} ->
                    Req5 = cowboy_req:set_resp_body(<<>>, Req4),
                    {true, Req5, State#rstate{client = #client{role = guest}}};
                _ ->
                    % @fixme maybe enforce 401
                    {stop, rest_replier:reply_with_error(Req, Error), State}
            end
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Returns whether the client is allowed to access given resource.
%% The client is an already authenticated user unless given endpoint has
%% the noauth flag permitting requests without authentication.
%% Negative result of this function triggers 403 Unauthorized status code.
%% @end
%%--------------------------------------------------------------------
-spec forbidden(Req :: cowboy_req:req(), State :: state()) ->
    {boolean() | stop, cowboy_req:req(), state()}.
forbidden(Req, #rstate{module = Module} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Req3, State2} = parse_query_string(Req2, State),
        {Authorized, Req4} = Module:is_authorized(Req3, Method, State2),
        {not Authorized, Req4, State2}
    catch
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error in authorization check: ~tp:~tp", [Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns whether the resource exists.
%% @end
%%--------------------------------------------------------------------
-spec resource_exists(Req :: cowboy_req:req(), State :: state()) ->
    {boolean() | stop, cowboy_req:req(), state()}.
resource_exists(Req, #rstate{module = Module, methods = Methods} = State) ->
    try
        {Method, Req2} = rest_utils:get_method(Req),
        {Bindings, Req3} = rest_utils:get_bindings(Req2),
        #rmethod{params_spec = Spec} = lists:keyfind(Method, #rmethod.type, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        {Exists, Req5} = Module:exists_resource(Req4, State#rstate{
            bindings = Bindings,
            params = Params
        }),
        {Exists, Req5, State}
    catch
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error in resource existence check: ~tp:~tp", [Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
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
        throw:invalid_json ->
            {stop, rest_replier:reply_with_error(Req, ?ERROR_MALFORMED_DATA), State};
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error handling ~s request: ~tp:~tp",
                [cowboy_req:method(Req), Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
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
        {Data2, Req3} = try
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            [Data] = yamerl_constr:string(Body, [str_node_as_binary]),
            {json_utils:list_to_map(Data), Req2}
        catch _:_ ->
            throw(?ERROR_MALFORMED_DATA)
        end,
        accept_resource(Req3, Data2, State)
    catch
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error handling ~s request: ~tp:~tp",
                [cowboy_req:method(Req), Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
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
        #rmethod{params_spec = Spec} = lists:keyfind(Method, #rmethod.type, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        case Module:provide_resource(Req4, State#rstate{
            bindings = Bindings,
            params = Params
        }) of
            {{binary, Data}, Req5} ->
                {Data, Req5, State};
            {Data, Req5} ->
                Json = json_utils:encode(Data),
                {Json, Req5, State};
            {stop, Req5, State} ->
                {stop, Req5, State}
        end
    catch
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error handling GET request: ~tp:~tp", [Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
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
        #rmethod{params_spec = Spec} = lists:keyfind(Method, #rmethod.type, Methods),
        {Params, Req4} = rest_utils:get_params(Req3, Spec),
        case Module:is_conflict(Req4, Method, #{}, State#rstate{
            bindings = Bindings,
            params = Params
        }) of
            {false, Req5} ->
                {Deleted, Req6} = Module:delete_resource(Req5, State#rstate{
                    bindings = Bindings,
                    params = Params
                }),
                {Deleted, Req6, State};
            {true, Req5} ->
                {stop, cowboy_req:reply(?HTTP_409_CONFLICT, Req5), State}
        end
    catch
        throw:Reason ->
            {stop, rest_replier:reply_with_error(Req, Reason), State};
        Type:Reason ->
            ?error_stacktrace("Error handling DELETE request: ~tp:~tp", [Type, Reason]),
            {stop, rest_replier:reply_with_error(Req, ?ERROR_INTERNAL_SERVER_ERROR), State}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sends reply for an OPTIONS request.
%% @end
%%--------------------------------------------------------------------
-spec handle_options(cowboy_req:req(), State :: state()) ->
    {ok, cowboy_req:req(), state()}.
handle_options(Req, State) ->
    case rest_utils:allowed_origin() of
        undefined ->
            % For unregistered provider or not deployed zone
            % there is no need for OPTIONS request
            {ok, cowboy_req:reply(?HTTP_405_METHOD_NOT_ALLOWED, Req), State};
        Origin ->
            {AllowedMethods, Req2, _} = rest_handler:allowed_methods(Req, State),

            AllowedHeaders = [?HDR_CONTENT_TYPE | tokens:supported_access_token_headers()],
            Req3 = gui_cors:options_response(Origin, AllowedMethods, AllowedHeaders, Req2),

            {ok, Req3, State}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Reads bindings and parameters from the query string
%% and inserts into state.
%% @end
%%--------------------------------------------------------------------
-spec parse_query_string(Req :: cowboy_req:req(), State :: state()) ->
    {Req :: cowboy_req:req(), NewState :: state()}.
parse_query_string(Req, #rstate{methods = Methods} = State) ->
    {Method, Req2} = rest_utils:get_method(Req),
    {Bindings, Req3} = rest_utils:get_bindings(Req2),
    #rmethod{params_spec = Spec} = lists:keyfind(Method, #rmethod.type, Methods),
    {Params, Req4} = rest_utils:get_params(Req3, Spec),
    {Req4, State#rstate{
        bindings = Bindings,
        params = Params
    }}.


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
        lists:keyfind(Method, #rmethod.type, Methods),
    {Params, Req4} = rest_utils:get_params(Req3, ParamSpec),
    Args = rest_utils:get_args(Data, ArgsSpec),
    State2 = State#rstate{bindings = Bindings, params = Params},

    case Module:is_conflict(Req4, Method, Args, State2) of
        {false, Req5} ->
            {Result, Req6} = Module:accept_resource(Req5, Method, Args, State2),
            {Result, Req6, State};
        {true, Req5} ->
            {stop, cowboy_req:reply(?HTTP_409_CONFLICT, Req5), State}
    end.

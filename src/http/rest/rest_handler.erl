%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Module implementing cowboy rest flow callbacks.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_handler).
-author("Wojciech Geisler").

-include("names.hrl").
-include("authentication.hrl").
-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([init/2, allowed_methods/2, content_types_accepted/2,
    content_types_provided/2, is_authorized/2, delete_resource/2]).
-export([process_request/2]).
-export([send_response/2]).

-type method() :: http_utils:method().
-type binding() :: {binding, atom()}.
-type qs_params() :: #{binary() => binary()}.
-type spec() :: onepanel_parser:object_spec().
-type bound_gri() :: #b_gri{}.

%% Objects used to authenticate request to Onezone
%% 'none' is used if proper authentication object could not be obtained
-type zone_credentials() :: rpc_auth() | rest_auth() | none.
%% Used by oz_panel
-type rpc_auth() :: {rpc, aai:auth()}.
%% Used by op_panel
-type rest_auth() :: {rest, oz_plugin:auth()}.

-record(state, {
    client = #client{} :: middleware:client(),
    rest_req = undefined :: #rest_req{} | undefined,
    allowed_methods :: [method()]
}).
-type state() :: #state{}.

-export_type([binding/0, bound_gri/0, method/0, qs_params/0, spec/0,
    zone_credentials/0]).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Upgrades the protocol to cowboy_rest.
%% @end
%%--------------------------------------------------------------------
-spec init(Req :: cowboy_req:req(), #{method() => #rest_req{}}) ->
    {cowboy_rest | ok, Req :: cowboy_req:req(), state()}.
init(InitialReq, Opts) ->
    % The REST API accepts only basic auth and tokens (rather than session cookies) so
    % it's safe to allow it to be called from other origins.
    Req = http_cors:allow_origin(<<"*">>, InitialReq),
    MethodBin = cowboy_req:method(Req),
    Method = http_utils:binary_to_method(MethodBin),
    % If given method is not allowed, it is not in the map.
    % Use 'undefined' here as the execution will stop on allowed_methods/2 anyway.
    State = #state{
        rest_req = maps:get(Method, Opts, undefined),
        allowed_methods = maps:keys(Opts)
    },
    case onepanel_env:get(service_circuit_breaker_state, ?APP_NAME, closed) of
        open ->
            Error = ?ERR_SERVICE_UNAVAILABLE(?err_ctx()),
            Req1 = cowboy_req:reply(
                errors:to_http_code(Error),
                #{?HDR_CONTENT_TYPE => <<"application/json">>},
                json_utils:encode(#{<<"error">> => errors:to_json(Error)}),
                Req
            ),
            {ok, Req1, State};
        closed when Method == 'OPTIONS' ->
            handle_options(Req, State);
        closed ->
            {cowboy_rest, Req, State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Return the list of allowed methods.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods(Req :: cowboy_req:req(), State :: #state{}) ->
    {[binary()], cowboy_req:req(), #state{}}.
allowed_methods(Req, #state{allowed_methods = AllowedMethods} = State) ->
    {lists:map(fun http_utils:method_to_binary/1, AllowedMethods), Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of content-types the resource
%% accepts.
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(Req :: cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, #state{rest_req = #rest_req{}} = State) ->
    case cowboy_req:has_body(Req) of
        true -> {[
            {<<"application/json">>, process_request},
            {<<"application/x-yaml">>, process_request}
        ], Req, State};
        false -> {[
            {'*', process_request}
        ], Req, State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Returns the list of content-types the resource
%% provides.
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(Req :: cowboy_req:req(), state()) ->
    {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, #state{rest_req = #rest_req{produces = Produces}} = State) ->
    {[{ContentType, process_request} || ContentType <- Produces], Req, State}.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function.
%% Handles authentication of the user.
%% Negative result of this function triggers 401 Unauthorized which
%% is HTTP status used to describe authentication errors.
%% However, even requests with no credentials pass this check, causing
%% #client.role = guest.
%% @end
%%--------------------------------------------------------------------
-spec is_authorized(Req :: cowboy_req:req(), state()) ->
    {true | {false, binary()} | stop, cowboy_req:req(), state()}.
is_authorized(Req, #state{} = State) ->
    case rest_auth:authenticate(Req) of
        {{true, Client}, Req2} ->
            {true, Req2, State#state{client = Client}};
        {{false, Error}, Req3} ->
            Response = rest_translator:error_response(Error),
            {stop, send_response(Response, Req3), State}
    end.


%%--------------------------------------------------------------------
%% @doc Cowboy callback function. Deletes the resource.
%% @end
%%--------------------------------------------------------------------
-spec delete_resource(Req :: cowboy_req:req(), state()) ->
    {stop, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
    process_request(Req, State).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Processes a REST request (of any type) by calling middleware.
%% Return new Req and State (after setting cowboy response).
%% @end
%%--------------------------------------------------------------------
-spec process_request(cowboy_req:req(), state()) ->
    {stop, cowboy_req:req(), state()}.
process_request(Req, State = #state{client = Client, rest_req = #rest_req{
    method = Method,
    b_gri = GriWithBindings,
    data_spec = InputSpec
}}) ->
    try
        OnpReqCtx = #onp_req_ctx{
            interface = rest,
            operation = method_to_operation(Method),
            client = Client,
            gri = resolve_gri_bindings(GriWithBindings, Req)
        },
        {Req2, Input} = get_input(Req),

        RestResp = case middleware:handle(OnpReqCtx, Input, InputSpec) of
            ok -> ?NO_CONTENT_REPLY;
            {ok, RestOutput} -> RestOutput;
            {error, _} = HandlerError -> rest_translator:error_response(HandlerError)
        end,
        {stop, send_response(RestResp, Req2), State}
    catch Class:Reason:Stacktrace ->
        Error = ?examine_exception(Class, Reason, Stacktrace),
        ErrorResp = rest_translator:error_response(Error),
        {stop, send_response(ErrorResp, Req), State}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Sends given response and returns modified cowboy_req record.
%% @end
%%--------------------------------------------------------------------
-spec send_response(RestResp :: #rest_resp{}, cowboy_req:req()) ->
    cowboy_req:req().
send_response(#rest_resp{code = Code, headers = Headers, body = Body}, Req) ->
    RespBody = case Body of
        {binary, Bin} ->
            Bin;
        JSONable ->
            json_utils:encode(JSONable)
    end,
    cowboy_req:reply(Code, Headers, RespBody, Req).


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec get_input(cowboy_req:req()) -> {cowboy_req:req(), middleware_handler:rest_input()}.
get_input(Req) ->
    Params = get_qs_params(Req),
    {Req2, Body} = get_data(Req),
    {Req2, maps:merge(Body, Params)}.


%% @private
-spec get_qs_params(cowboy_req:req()) -> qs_params().
get_qs_params(Req) ->
    Params = cowboy_req:parse_qs(Req),
    lists:foldl(fun
        ({Key, true}, Acc) -> maps:put(Key, <<"true">>, Acc);
        ({Key, Value}, Acc) -> maps:put(Key, Value, Acc)
    end, #{}, Params).


%%--------------------------------------------------------------------
%% @private
%% @doc Decodes request body. YAML format is used when indicated by
%% content-type header, otherwise JSON is assumed.
%% @end
%%--------------------------------------------------------------------
-spec get_data(cowboy_req:req()) -> {cowboy_req:req(), json_utils:json_term()}.
get_data(Req) ->
    try
        {ok, Body, Req2} = cowboy_req:read_body(Req),
        ContentType = try
            {Type, Subtype, _} = cowboy_req:parse_header(?HDR_CONTENT_TYPE, Req2),
            <<Type/binary, "/", Subtype/binary>>
        catch _:_ ->
            undefined
        end,
        case ContentType of
            <<"application/x-yaml">> ->
                [Data] = yamerl_constr:string(Body, [str_node_as_binary]),
                {Req2, json_utils:list_to_map(Data)};
            _ -> % includes <<"application/json">>
                {Req2, json_utils:decode(Body)}
        end
    catch
        _:_ ->
            throw(?ERR_MALFORMED_DATA(?err_ctx()))
    end.


%%--------------------------------------------------------------------
%% @doc Sends reply for an OPTIONS request.
%% @end
%%--------------------------------------------------------------------
-spec handle_options(cowboy_req:req(), state()) ->
    {ok, cowboy_req:req(), state()}.
handle_options(Req, State) ->
    {AllowedMethods, Req2, _} = rest_handler:allowed_methods(Req, State),

    AllowedHeaders = [?HDR_LOCATION, ?HDR_CONTENT_TYPE | tokens:supported_access_token_headers()],
    Req3 = http_cors:options_response(<<"*">>, AllowedMethods, AllowedHeaders, Req2),

    {ok, Req3, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings included in a #b_gri{} record into actual data
%% that was sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_gri_bindings(bound_gri(), cowboy_req:req()) ->
    gri:gri().
resolve_gri_bindings(#b_gri{type = Tp, id = Id, aspect = As, scope = Sc}, Req) ->
    IdBinding = resolve_bindings(Id, Req),
    AspectBinding = case As of
        {Atom, Asp} -> {Atom, resolve_bindings(Asp, Req)};
        Atom -> Atom
    end,
    #gri{type = Tp, id = IdBinding, aspect = AspectBinding, scope = Sc}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Transforms bindings as specified in rest routes into actual data that was
%% sent with the request.
%% @end
%%--------------------------------------------------------------------
-spec resolve_bindings(binding() | {atom(), binding()} | term(),
    cowboy_req:req()) -> binary() | {atom(), binary()}.
resolve_bindings(?BINDING(Key), Req) ->
    cowboy_req:binding(Key, Req);

resolve_bindings(Other, _Req) ->
    Other.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts an atom representing a REST method into operation
%% that should be called to handle it.
%% @end
%%--------------------------------------------------------------------
-spec method_to_operation(method()) -> middleware_handler:operation().
method_to_operation('POST') -> create;
method_to_operation('PUT') -> create;
method_to_operation('GET') -> get;
method_to_operation('PATCH') -> update;
method_to_operation('DELETE') -> delete.

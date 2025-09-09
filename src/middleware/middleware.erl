%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Bartosz Walkowicz
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% TODO VFS-5621
%%% This module handles a requests processing layer common for
%%% graph sync (websocket) and REST interfaces.
%%%
%%% All this operations are carried out by middleware handlers (modules
%%% implementing `middleware_handler` behaviour). Each such module is responsible
%%% for handling all request pointing to the same aspect of entity type.
%%% @end
%%%-------------------------------------------------------------------
-module(middleware).
-author("Lukasz Opiola").
-author("Bartosz Walkowicz").
-author("Wojciech Geisler").

-include("middleware/middleware.hrl").

%% API
-export([handle/3]).


-type client() :: #client{}.

-export_type([client/0]).

% Internal record containing the request state.
-record(state, {
    handler :: middleware_handler:t(),
    req_ctx :: middleware_handler:req_ctx(),
    handler_state :: middleware_handler:state()
}).
-type state() :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================


-spec handle(
    middleware_handler:req_ctx(),
    middleware_handler:interface_input(),
    undefined | onepanel_parser:object_spec()
) ->
    ok | {ok, middleware_handler:interface_output()} | errors:error().
handle(OnpReqCtx, Input, InputSpec) ->
    try
        Handler = find_handler(OnpReqCtx),
        assert_interface_supported(Handler, OnpReqCtx),

        HandlerInput = sanitize_input(Input, InputSpec),

        assert_required_services_available(Handler, OnpReqCtx),

        State = init_state(Handler, OnpReqCtx, HandlerInput),
        assert_preauthorized(State),
        validate_request(State),
        case process_request(State) of
            ok -> ok;
            {ok, HandlerOutput} -> translate_output(State, HandlerOutput)
        end
    catch Class:Reason:Stacktrace ->
        ?examine_exception(Class, Reason, Stacktrace)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec find_handler(middleware_handler:req_ctx()) -> middleware_handler:t() | no_return().
find_handler(#onp_req_ctx{operation = Operation, gri = Gri}) ->
    case middleware_router:resolve_handler(Operation, Gri) of
        {true, Handler} -> Handler;
        false -> throw(?ERROR_NOT_SUPPORTED)
    end.


%% @private
-spec assert_interface_supported(middleware_handler:t(), middleware_handler:req_ctx()) ->
    ok | no_return().
assert_interface_supported(Handler, OnpReqCtx) ->
    case is_interface_supported(Handler, OnpReqCtx) of
        true -> ok;
        false -> throw(?ERROR_NOT_SUPPORTED)
    end.


%% @private
-spec is_interface_supported(middleware_handler:t(), middleware_handler:req_ctx()) ->
    boolean().
is_interface_supported(Handler, OnpReqCtx = #onp_req_ctx{interface = Interface}) ->
    try Handler:supported_interfaces(OnpReqCtx) of
        {true, SupportedInterfaces} -> lists:member(Interface, SupportedInterfaces);
        false -> false
    catch _:_ ->
        false
    end.


%% @private
-spec sanitize_input(
    middleware_handler:interface_input(),
    undefined | onepanel_parser:object_spec()
) ->
    map() | middleware_handler:handler_input() | no_return().
sanitize_input(_, undefined) ->
    #{};
sanitize_input(Input, InputSpec) ->
    onepanel_parser:parse(Input, InputSpec).


%% @private
-spec assert_required_services_available(middleware_handler:t(), middleware_handler:req_ctx()) ->
    ok | no_return().
assert_required_services_available(Handler, OnpReqCtx) ->
    case Handler:service_availability_requirements(OnpReqCtx) of
        {true, Requirements} ->
            case lists:all(fun is_availability_satisfied/1, Requirements) of
                true -> ok;
                false -> throw(?ERR_SERVICE_UNAVAILABLE(?err_ctx()))
            end;
        false ->
            ok
    end.


%% @private
-spec is_availability_satisfied(middleware_handler:availability_level()) -> boolean().
is_availability_satisfied(all_healthy_ignoring_ones3) ->
    service:all_healthy_ignoring_ones3();
is_availability_satisfied(Service) ->
    service:is_healthy(Service).


%% @private
-spec init_state(
    middleware_handler:t(),
    middleware_handler:req_ctx(),
    middleware_handler:handler_input()
) ->
    state() | no_return().
init_state(Handler, OnpReqCtx, HandlerInput) ->
    HandlerState = try Handler:init_state(OnpReqCtx, HandlerInput) of
        {ok, HS} -> HS;
        {error, _} = Error -> throw(Error)
    catch error:undef ->
        #onp_req_state{ctx = OnpReqCtx, input = HandlerInput}
    end,

    #state{
        handler = Handler,
        req_ctx = OnpReqCtx,
        handler_state = HandlerState
    }.


%% @private
-spec assert_preauthorized(state()) -> ok | no_return().
assert_preauthorized(#state{req_ctx = #onp_req_ctx{client = #client{role = root}}}) ->
    % Root (emergency passphrase) client is authorized to do everything
    ok;

assert_preauthorized(#state{
    handler = Handler,
    req_ctx = #onp_req_ctx{
        operation = Operation,
        gri = Gri,
        client = Client = #client{auth = Auth}
    },
    handler_state = HandlerState
}) ->
    Service = case onepanel_env:get_cluster_type() of
        ?ONEZONE -> ?OZ_PANEL;
        ?ONEPROVIDER -> ?OP_PANEL
    end,
    case api_auth:check_authorization(Auth, Service, Operation, Gri) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end,

    Result = try
        Handler:preauthorize(HandlerState)
    catch _:_ ->
        % No need for log here, 'authorize' may crash depending on what the
        % request contains and this is expected.
        false
    end,
    case Result of
        true ->
            ok;
        false ->
            case Client of
                #client{role = guest} ->
                    % The client was not authenticated -> unauthorized
                    throw(?ERR_UNAUTHORIZED(?err_ctx(), undefined));
                #client{} ->
                    % The client was authenticated but cannot access the
                    % aspect -> forbidden
                    throw(?ERR_FORBIDDEN(?err_ctx()))
            end
    end.


%% @private
-spec validate_request(state()) -> ok | no_return().
validate_request(#state{handler = Handler, handler_state = HandlerState}) ->
    case Handler:validate(HandlerState) of
        ok -> ok;
        {error, _} = Error -> throw(Error)
    end.


%% @private
-spec process_request(state()) ->
    ok | {ok, middleware_handler:handler_output()} | no_return().
process_request(#state{
    handler = Handler,
    req_ctx = #onp_req_ctx{operation = Operation, gri = Gri, client = Client},
    handler_state = HandlerState
}) ->
    case {Handler:process(HandlerState), Operation, Gri} of
        {ok, delete, #gri{type = Type, id = Id, aspect = instance}} ->
            % If an entity instance is deleted, log an information about it
            % (it's a significant operation and this information might be useful).
            ?info("~ts(~tp) has been deleted by client: ~ts",
                [Type, Id, client_to_string(Client)]),
            ok;
        {Result, _, _} ->
            Result
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns a readable string representing provided client.
%% @end
%%--------------------------------------------------------------------
-spec client_to_string(middleware:client() | aai:auth()) -> string().
client_to_string(#client{auth = Auth}) -> client_to_string(Auth);
client_to_string(?NOBODY) -> "nobody (unauthenticated client)";
client_to_string(?ROOT) -> "root";
client_to_string(?USER(Id)) -> str_utils:format("user:~ts", [Id]).


%% @private
-spec translate_output(state(), middleware_handler:handler_output()) ->
    {ok, middleware_handler:interface_output()} | errors:error().
translate_output(State, HandlerOutput) ->
    Handler = State#state.handler,
    Handler:translate_output(State#state.handler_state, HandlerOutput).

%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains helper functions for modules implementing
%%% {@link rest_module_behavior}.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([get_method/1, get_bindings/1, get_params/2, get_args/2, get_hosts/2,
    verify_any/2]).
-export([handle_errors/3, handle_service_action/2, handle_service_steps/2,
    handle_service_step/4]).
-export([format_errors/2, format_service_action/1, handle_service_action_async/3,
    format_service_steps/1, format_service_step/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts REST method from binary to an atom representation.
%%--------------------------------------------------------------------
-spec get_method(Req :: cowboy_req:req()) ->
    {Method :: rest_handler:method_type(), Req :: cowboy_req:req()}.
get_method(Req) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} -> {'POST', Req2};
        {<<"PATCH">>, Req2} -> {'PATCH', Req2};
        {<<"GET">>, Req2} -> {'GET', Req2};
        {<<"PUT">>, Req2} -> {'PUT', Req2};
        {<<"DELETE">>, Req2} -> {'DELETE', Req2}
    end.


%%--------------------------------------------------------------------
%% @doc Returns a map of endpoint bindings, e.g. for endpoint /users/:username
%% and request /users/user1 returns ```#{'username' => <<"user1">>}'''
%% @end
%%--------------------------------------------------------------------
-spec get_bindings(Req :: cowboy_req:req()) ->
    {Bindings :: rest_handler:bindings(), Req :: cowboy_req:req()}.
get_bindings(Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    NewBindings = lists:map(fun
        ({host, Value}) -> {host, onepanel_utils:convert(Value, list)};
        ({Key, Value}) -> {Key, Value}
    end, Bindings),
    {maps:from_list(NewBindings), Req2}.


%%--------------------------------------------------------------------
%% @doc Returns a map of query string name and associated value.
%%--------------------------------------------------------------------
-spec get_params(Req :: cowboy_req:req(), ParamsSpec :: rest_handler:spec()) ->
    {Params :: rest_handler:params(), Req :: cowboy_req:req()}.
get_params(Req, ParamsSpec) ->
    {Params, Req2} = cowboy_req:qs_vals(Req),
    NewParams = lists:map(fun
        ({Key, true}) -> {Key, <<"true">>};
        ({Key, Value}) -> {Key, Value}
    end, Params),
    try
        {onepanel_parser:parse(NewParams, ParamsSpec), Req2}
    catch
        throw:#error{reason = {?ERR_MISSING_KEY, Keys}} = Error ->
            ?throw(Error#error{reason = {?ERR_MISSING_PARAM, Keys}})
    end.


%%--------------------------------------------------------------------
%% @doc Parses request body according to provided specification.
%%--------------------------------------------------------------------
-spec get_args(Data :: rest_handler:data(), ArgsSpec :: rest_handler:spec()) ->
    Args :: rest_handler:args() | no_return().
get_args(Data, ArgsSpec) ->
    onepanel_parser:parse(Data, ArgsSpec).


%%--------------------------------------------------------------------
%% @doc Returns lists of hosts for given service based on cluster description
%% format.
%% @end
%%--------------------------------------------------------------------
-spec get_hosts(Keys :: [atom()], Args :: rest_handler:args()) ->
    Hosts :: [service:host()] | no_return().
get_hosts(Keys, Args) ->
    DomainName = onepanel_maps:get([cluster, domainName], Args),
    HostsMap = maps:fold(fun(Alias, Props, Acc) ->
        Host = <<(maps:get(hostname, Props))/binary, ".", DomainName/binary>>,
        maps:put(Alias, Host, Acc)
    end, #{}, onepanel_maps:get([cluster, nodes], Args)),
    Aliases = onepanel_maps:get(Keys, Args),
    AliasesList = case erlang:is_list(Aliases) of
        true -> Aliases;
        false -> [Aliases]
    end,
    lists:map(fun(Alias) ->
        case maps:find(Alias, HostsMap) of
            {ok, Host} -> onepanel_utils:convert(Host, list);
            error -> ?throw({?ERR_HOST_NOT_FOUND_FOR_ALIAS, Alias})
        end
    end, AliasesList).


%%--------------------------------------------------------------------
%% @doc Checks whether at least one key from a provided list is found in a map.
%% Throws an exception if none of keys is found.
%% @end
%%--------------------------------------------------------------------
-spec verify_any(Keys :: [atom()], Args :: rest_handler:args()) -> ok | no_return().
verify_any(Keys, Args) ->
    case lists:any(fun(Key) -> maps:is_key(Key, Args) end, Keys) of
        true -> ok;
        _ -> ?throw({?ERR_MISSING_ANY_KEY, Keys})
    end.


%%--------------------------------------------------------------------
%% @doc Sets error description in a response body.
%%--------------------------------------------------------------------
-spec handle_errors(Req :: cowboy_req:req(), Type :: atom(), Reason :: term()) ->
    Req :: cowboy_req:req().
handle_errors(Req, Type, Reason) ->
    Body = json_utils:encode(format_errors(Type, Reason)),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @doc Sets service action description in a response body.
%%--------------------------------------------------------------------
-spec handle_service_action(Req :: cowboy_req:req(), Results :: list() | #error{}) ->
    Req :: cowboy_req:req().
handle_service_action(Req, Results) ->
    handle_service_response(Req, fun format_service_action/1, [Results]).


%%--------------------------------------------------------------------
%% @doc Sets task ID as a location in a response header.
%%--------------------------------------------------------------------
-spec handle_service_action_async(Req :: cowboy_req:req(),
    TaskId :: service_executor:task_id(), ApiVersion :: integer()) ->
    Req :: cowboy_req:req().
handle_service_action_async(Req, TaskId, ApiVersion) ->
    Template = onepanel_env:get(rest_api_prefix_template),
    Prefix = re:replace(Template, "{version_number}",
        onepanel_utils:convert(ApiVersion, binary), [{return, binary}]),
    Location = <<Prefix/binary, "/service/tasks/", TaskId/binary>>,
    cowboy_req:set_resp_header(<<"location">>, Location, Req).


%%--------------------------------------------------------------------
%% @doc Sets service steps description in a response body.
%%--------------------------------------------------------------------
-spec handle_service_steps(Req :: cowboy_req:req(), Results :: list() | #error{}) ->
    Req :: cowboy_req:req().
handle_service_steps(Req, Results) ->
    handle_service_response(Req, fun format_service_steps/1, [Results]).


%%--------------------------------------------------------------------
%% @doc Sets service step description in a response body.
%%--------------------------------------------------------------------
-spec handle_service_step(Req :: cowboy_req:req(), Module :: module(),
    Function :: atom(), Results :: list() | #error{}) -> Req :: cowboy_req:req().
handle_service_step(Req, Module, Function, Results) ->
    handle_service_response(Req, fun format_service_step/3, [Module, Function, Results]).


%%--------------------------------------------------------------------
%% @doc Returns formatted error description.
%%--------------------------------------------------------------------
-spec format_errors(Type :: atom(), Reason :: term()) -> Response :: list().
format_errors(Type, Reason) ->
    {Name, Description} = onepanel_errors:translate(Type, Reason),
    [{<<"error">>, Name}, {<<"description">>, Description}].


%%--------------------------------------------------------------------
%% @doc Returns formatted service action.
%%--------------------------------------------------------------------
-spec format_service_action(Results :: list()) -> Response :: list().
format_service_action(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{}}}, Step | _] ->
            format_service_step(Step);
        [{task_finished, {_, _, #error{} = Error}} | _] ->
            [{<<"status">>, <<"error">>}] ++ format_errors(error, Error);
        [{task_finished, {_, _, ok}} | _] ->
            [{<<"status">>, <<"ok">>}];
        _ ->
            [{<<"status">>, <<"running">>}]
    end.


%%--------------------------------------------------------------------
%% @doc Returns formatted service steps.
%%--------------------------------------------------------------------
-spec format_service_steps(Results :: list()) -> Response :: list().
format_service_steps(Results) ->
    Steps = lists:foldl(fun
        ({Module, Function, _Results}, Acc) ->
            [<<(onepanel_utils:convert(Module, binary))/binary, ":",
                (onepanel_utils:convert(Function, binary))/binary>> | Acc];
        (_, Acc) -> Acc
    end, [], Results),
    format_service_action(Results) ++ [{<<"steps">>, lists:reverse(Steps)}].


%%--------------------------------------------------------------------
%% @doc Selects and returns formatted service step.
%%--------------------------------------------------------------------
-spec format_service_step(Module :: module(), Function :: atom(), Results :: list()) ->
    Response :: list().
format_service_step(_Module, _Function, []) ->
    [];

format_service_step(Module, Function, [{Module, Function, _} = Step | _]) ->
    format_service_step(Step);

format_service_step(Module, Function, [_Result | Results]) ->
    format_service_step(Module, Function, Results).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Sets service action result in the response body.
%%--------------------------------------------------------------------
-spec handle_service_response(Req :: cowboy_req:req(), ResponseFormatter :: fun(),
    Args :: list()) -> Req :: cowboy_req:req().
handle_service_response(Req, _ResponseFormatter, [#error{} = Error]) ->
    handle_errors(Req, error, Error);

handle_service_response(Req, ResponseFormatter, Args) ->
    Response = erlang:apply(ResponseFormatter, Args),
    Body = json_utils:encode(Response),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @private @doc Returns formatted service step.
%%--------------------------------------------------------------------
-spec format_service_step({Module :: module(), Function :: atom(), Results :: term()}) ->
    Response :: list().
format_service_step({_Module, _Function, {GoodResults, []}}) ->
    [
        {<<"status">>, <<"ok">>},
        {<<"hosts">>, format_hosts_results(GoodResults)}
    ];
format_service_step({_Module, _Function, {_, BadResults}}) ->
    [
        {<<"status">>, <<"error">>},
        {<<"hosts">>, format_hosts_results(BadResults)}
    ].


%%--------------------------------------------------------------------
%% @private @doc Returns formatted service results for each host.
%%--------------------------------------------------------------------
-spec format_hosts_results(Results :: list()) -> Response :: list().
format_hosts_results(Results) ->
    lists:map(fun
        ({Node, #error{} = Error}) -> {
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            format_errors(error, Error)
        };
        ({Node, Result}) -> {
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            Result
        }
    end, Results).
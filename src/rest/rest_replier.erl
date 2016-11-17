%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains functions for handling and formatting REST API
%%% responses.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_replier).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").

%% API
-export([throw_on_service_error/2]).
-export([handle_error/3, handle_service_action_async/3, handle_service_step/4]).
-export([format_error/2, format_service_status/2, format_service_host_status/2,
    format_service_task_results/1, format_service_step/3, format_configuration/1]).

-type response() :: proplists:proplist() | term().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link service_utils:throw_on_error/1}
%% @end
%%--------------------------------------------------------------------
-spec throw_on_service_error(Req :: cowboy_req:req(),
    Results :: #error{} | service_executor:results()) ->
    Req :: cowboy_req:req() | no_return().
throw_on_service_error(Req, Results) ->
    service_utils:throw_on_error(Results),
    Req.


%%--------------------------------------------------------------------
%% @doc Sets an error description in the response body.
%% @end
%%--------------------------------------------------------------------
-spec handle_error(Req :: cowboy_req:req(), Type :: atom(), Reason :: term()) ->
    Req :: cowboy_req:req().
handle_error(Req, Type, Reason) ->
    Body = json_utils:encode(format_error(Type, Reason)),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @doc Sets the task ID as a location in the response header.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_action_async(Req :: cowboy_req:req(),
    TaskId :: service_executor:task_id(), ApiVersion :: integer()) ->
    Req :: cowboy_req:req().
handle_service_action_async(Req, TaskId, ApiVersion) ->
    Prefix = rest_listener:get_prefix(ApiVersion),
    Location = <<Prefix/binary, "/tasks/", TaskId/binary>>,
    cowboy_req:set_resp_header(<<"location">>, Location, Req).


%%--------------------------------------------------------------------
%% @doc Sets a formatted result of the single host service step in the response
%% body.
%% @end
%%--------------------------------------------------------------------
-spec handle_service_step(Req :: cowboy_req:req(), Module :: module(),
    Function :: atom(), Results :: service_executor:results()) ->
    Req :: cowboy_req:req().
handle_service_step(Req, Module, Function, Results) ->
    Body = json_utils:encode(format_service_step(Module, Function, Results)),
    cowboy_req:set_resp_body(Body, Req).


%%--------------------------------------------------------------------
%% @doc Returns a formatted error description.
%% @end
%%--------------------------------------------------------------------
-spec format_error(Type :: atom(), Reason :: term()) -> Response :: response().
format_error(Type, #error{reason = #service_error{} = Error}) ->
    format_error(Type, Error);

format_error(_Type, #service_error{service = Service, action = Action,
    module = Module, function = Function, bad_results = Results}) ->
    [
        {<<"error">>, <<"Service Error">>},
        {<<"description">>, onepanel_utils:join(["Action '", Action,
            "' for a service '", Service, "' terminated with an error."])},
        {<<"module">>, Module},
        {<<"function">>, Function},
        {<<"hosts">>, format_service_hosts_results(Results)}
    ];

format_error(Type, Reason) ->
    {Name, Description} = onepanel_errors:translate(Type, Reason),
    [{<<"error">>, Name}, {<<"description">>, Description}].


%%--------------------------------------------------------------------
%% @doc Returns a formatted service status.
%% @end
%%--------------------------------------------------------------------
-spec format_service_status(SModule :: module(),
    Results :: service_executor:results()) -> Response :: response().
format_service_status(SModule, Results) ->
    {HostsResults, []} = select_service_step(SModule, status, Results),
    lists:map(fun
        ({Node, Result}) -> {
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            Result
        }
    end, HostsResults).


%%--------------------------------------------------------------------
%% @doc @equiv format_service_step(SModule, status, Results)
%% @end
%%--------------------------------------------------------------------
-spec format_service_host_status(SModule :: module(),
    Results :: service_executor:results()) -> Response :: response().
format_service_host_status(SModule, Results) ->
    format_service_step(SModule, status, Results).


%%--------------------------------------------------------------------
%% @doc Returns a formatted service asynchronous task result.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_results(Results :: #error{} | service_executor:results()) ->
    Response :: response().
format_service_task_results(#error{} = Error) ->
    ?throw_error(Error);

format_service_task_results(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            [{<<"status">>, <<"error">>} | format_error(error, Error)];

        [{task_finished, {Service, Action, #error{}}}, Step | Steps] ->
            {Module, Function, {_, BadResults}} = Step,
            [
                {<<"status">>, <<"error">>},
                {<<"steps">>, format_service_task_steps(lists:reverse(Steps))} |
                format_error(error, #service_error{
                    service = Service, action = Action, module = Module,
                    function = Function, bad_results = BadResults
                })
            ];

        [{task_finished, {_, _, ok}} | Steps] ->
            [
                {<<"status">>, <<"ok">>},
                {<<"steps">>, format_service_task_steps(lists:reverse(Steps))}
            ];

        Steps ->
            [
                {<<"status">>, <<"running">>},
                {<<"steps">>, format_service_task_steps(lists:reverse(Steps))}
            ]
    end.


%%--------------------------------------------------------------------
%% @doc Returns a formatted result of the single host service step.
%% @end
%%--------------------------------------------------------------------
-spec format_service_step(Module :: module(), Function :: atom(),
    Results :: service_executor:results()) -> Response :: response().
format_service_step(Module, Function, Results) ->
    {[{_, HostResult}], []} = select_service_step(Module, Function, Results),
    HostResult.


%%--------------------------------------------------------------------
%% @doc Returns formatted cluster configuration.
%% @end
%%--------------------------------------------------------------------
-spec format_configuration(SModule :: module()) -> Response :: response().
format_configuration(SModule) ->
    DbHosts = service_couchbase:get_hosts(),
    {ok, #service{hosts = CmHosts, ctx = #{main_host := MainCmHost}}} =
        service:get(service_cluster_manager:name()),
    WrkHosts = SModule:get_hosts(),
    [
        {<<"cluster">>, [
            {<<"databases">>, onepanel_utils:convert(DbHosts, {seq, binary})},
            {<<"managers">>, [
                {<<"mainHost">>, onepanel_utils:convert(MainCmHost, binary)},
                {<<"hosts">>, onepanel_utils:convert(CmHosts, {seq, binary})}
            ]},
            {<<"workers">>, onepanel_utils:convert(WrkHosts, {seq, binary})}
        ]}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Returns hosts results of the selected service step.
%% Throws an exception if the step is not found.
%% @end
%%--------------------------------------------------------------------
-spec select_service_step(Module :: module(), Function :: atom(),
    Results :: service_executor:results()) ->
    HostsResults :: service_executor:hosts_results().
select_service_step(Module, Function, []) ->
    ?throw_error({?ERR_SERVICE_STEP_NOT_FOUND, Module, Function});

select_service_step(Module, Function, [{Module, Function, Results} | _]) ->
    Results;

select_service_step(Module, Function, [_ | Results]) ->
    select_service_step(Module, Function, Results).


%%--------------------------------------------------------------------
%% @doc Returns a list of formatted service steps.
%% @end
%%--------------------------------------------------------------------
-spec format_service_task_steps(Steps :: [service_executor:step_result()]) ->
    [StepName :: binary()].
format_service_task_steps(Steps) ->
    lists:filtermap(fun
        ({Module, Function}) ->
            {true, onepanel_utils:join([Module, Function], <<":">>)};
        (_) ->
            false
    end, Steps).


%%--------------------------------------------------------------------
%% @doc Returns a formatted service step hosts results.
%% @end
%%--------------------------------------------------------------------
-spec format_service_hosts_results(Results :: onepanel_rpc:results()) ->
    Response :: response().
format_service_hosts_results(Results) ->
    lists:map(fun
        ({Node, #error{} = Error}) -> {
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            format_error(error, Error)
        };
        ({Node, Result}) -> {
            onepanel_utils:convert(onepanel_cluster:node_to_host(Node), binary),
            Result
        }
    end, Results).
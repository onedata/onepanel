%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains service utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(service_utils).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include("modules/models.hrl").
-include("service.hrl").

%% API
-export([get_steps/3, format_steps/2, notify/2, partition_results/1]).
-export([results_contain_error/1, throw_on_error/1]).
-export([select_service_step/3]).
-export([for_each_ctx/2]).
-export([absolute_path/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of steps for the service action.
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Service :: service:name(), Action :: service:action(),
    Ctx :: service:step_ctx()) -> Steps :: [#step{}].
get_steps(Service, Action, Ctx) ->
    get_steps(#steps{service = Service, action = Action, ctx = Ctx,
        verify_hosts = true}).

%%--------------------------------------------------------------------
%% @doc Formats the service steps into a human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec format_steps(Steps :: [#step{}], Acc :: string()) -> Log :: string().
format_steps([], Log) ->
    Log;
format_steps([#step{hosts = Hosts, module = service_op_worker = Module,
    function = add_storages = Function, args = [Ctx]} | Steps], Log) ->
    Storages = maps:get(storages, Ctx, #{}),
    Storages2 = maps:fold(fun(Name, Params, Map) ->
        Params2 = lists:foldl(fun(Key, Map2) ->
            case maps:find(Key, Map2) of
                {ok, _} -> maps:put(Key, <<"__secret__">>, Map2);
                error -> Map2
            end
        end, Params, [key, secretKey, password]),
        maps:put(Name, Params2, Map)
    end, #{}, Storages),
    Ctx2 = maps:put(storages, Storages2, Ctx),
    Step = io_lib:format("Hosts: ~p~nFunction: ~p:~p~nArgs: ~p~n~n",
        [Hosts, Module, Function, [Ctx2]]),
    format_steps(Steps, Log ++ Step);
format_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args} | Steps], Log) ->
    Step = io_lib:format("Hosts: ~p~nFunction: ~p:~p~nArgs: ~p~n~n",
        [Hosts, Module, Function, Args]),
    format_steps(Steps, Log ++ Step).


%%--------------------------------------------------------------------
%% @doc Notifies about the service action progress.
%% @end
%%--------------------------------------------------------------------
-spec notify(Msg, service:notify()) -> ok when
    Msg :: service_executor:result() | #action_steps_count{} | #action_begin{}.
notify(Msg, Notify) when is_pid(Notify) ->
    log(Msg),
    Notify ! Msg,
    ok;

notify(Msg, _Notify) ->
    log(Msg),
    ok.


%%--------------------------------------------------------------------
%% @doc Divides the results into two groups. The first group represents
%% the results of step that completed successfully and the second group that
%% completed with an error.
%% @end
%%--------------------------------------------------------------------
-spec partition_results(Results :: onepanel_rpc:results()) ->
    service_executor:hosts_results().
partition_results(Results) ->
    lists:partition(fun
        ({_Node, {error, _}}) -> false;
        ({_Node, _Success}) -> true
    end, Results).


%%--------------------------------------------------------------------
%% @doc Checks if an error occured during service action execution
%% and returns it.
%% @end
%%--------------------------------------------------------------------
-spec results_contain_error(Results :: service_executor:results() | {error, _}) ->
    {true, errors:error() | {error, _}} | false.
results_contain_error({error, _} = Error) ->
    {true, Error};

results_contain_error(Results) ->
    case lists:reverse(Results) of
        [
            #action_end{result = {error, _} = Error},
            #step_end{good_bad_results = {_, [_ | _] = BadResults}}
            | _Steps
        ] ->
            {true, bad_results_to_error(BadResults)};

        [
            #action_end{result = {error, _} = Error}
            | _Steps
        ] ->
            % an action error may be without any erroneous #step_end,
            % for example if verify_hosts failed.
            {true, cast_to_serializable_error(Error)};

        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc Throws an exception if an error occurred during service action execution.
%% @end
%%--------------------------------------------------------------------
-spec throw_on_error(Results :: service_executor:results() | {error, term()}) ->
    service_executor:results() | no_return().
throw_on_error(Results) ->
    case results_contain_error(Results) of
        {true, Error} -> throw(Error);
        false -> Results
    end.


%%--------------------------------------------------------------------
%% @doc Returns hosts results of the selected service step.
%% Throws an exception if the step is not found.
%% @end
%%--------------------------------------------------------------------
-spec select_service_step(Module :: module(), Function :: atom(),
    Results :: service_executor:results()) ->
    HostsResults :: service_executor:hosts_results().
select_service_step(Module, Function, []) ->
    ?error("Service step ~p:~p not found", [Module, Function]),
    error({step_not_found, {Module, Function}});

select_service_step(Module, Function,
    [#step_end{module = Module, function = Function, good_bad_results = Results} | _]) ->
    Results;

select_service_step(Module, Function, [_ | Results]) ->
    select_service_step(Module, Function, Results).


%%--------------------------------------------------------------------
%% @doc Takes a list of Ctxs, each intended for one host, and a list of steps.
%% Duplicates the list of steps to be executed for each Ctx in sequence.
%% Host-specific ctx (from the Ctxs list) overrides
%% ctx values embedded in the Steps.
%% @end
%%--------------------------------------------------------------------
-spec for_each_ctx([HostCtx], Steps :: [service:step()]) -> [service:step()]
    when HostCtx :: #{host := service:host(), _ => _}.
for_each_ctx(Ctxs, Steps) ->
    lists:foldl(fun(#{host := Host} = HostCtx, StepsAcc) ->
        StepsAcc ++ lists:map(fun
            (#step{ctx = StepCtx} = S) ->
                StepCtxMap = utils:ensure_defined(StepCtx, undefined, #{}),
                S#step{ctx = maps:merge(StepCtxMap, HostCtx#{hosts => [Host]})};
            (#steps{ctx = StepsCtx} = S) ->
                StepsCtxMap = utils:ensure_defined(StepsCtx, undefined, #{}),
                S#steps{ctx = maps:merge(StepsCtxMap, HostCtx#{hosts => [Host]})}
        end, Steps)
    end, [], Ctxs).


%%--------------------------------------------------------------------
%% @doc If given Path is relative, converts it to absolute path
%% in relation to the cwd used by nodes of given service.
%% Requires the service nodes to be online.
%% @end
%%--------------------------------------------------------------------
-spec absolute_path(Service :: service:name(), Path :: file:filename_all()) ->
    AbsPath :: file:filename_all().
absolute_path(Service, Path) ->
    case filename:absname(Path) of
        Path ->
            Path;
        _ ->
            {ok, Node} = nodes:any(Service),
            case rpc:call(Node, filename, absname, [Path]) of
                {badrpc, _} = Error -> error(Error);
                AbsPath -> AbsPath
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Substitutes default steps values.
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Steps :: #steps{}) -> [#step{}].
get_steps(#steps{service = Service, action = Action, ctx = Ctx, verify_hosts = Verify}) ->
    Module = service:get_module(Service),
    Steps = Module:get_steps(Action, Ctx),
    lists:flatten(lists:map(fun
        (#step{service = Service2, ctx = Ctx2, verify_hosts = Verify2} = Step) ->
            get_step(Step#step{
                service = utils:ensure_defined(Service2, undefined, Service),
                ctx = utils:ensure_defined(Ctx2, undefined, Ctx),
                verify_hosts = utils:ensure_defined(Verify2, undefined, Verify)
            });
        (#steps{service = Service2, ctx = Ctx2, verify_hosts = Verify2} = NestedSteps) ->
            get_nested_steps(NestedSteps#steps{
                service = utils:ensure_defined(Service2, undefined, Service),
                ctx = utils:ensure_defined(Ctx2, undefined, Ctx),
                verify_hosts = utils:ensure_defined(Verify2, undefined, Verify)
            })
    end, Steps)).


%%--------------------------------------------------------------------
%% @private @doc Substitutes default step values.
%% @end
%%--------------------------------------------------------------------
-spec get_step(Step :: #step{}) -> Step :: [] | #step{}.
get_step(#step{service = Service, module = undefined} = Step) ->
    get_step(Step#step{module = service:get_module(Service)});

get_step(#step{ctx = Ctx, args = undefined} = Step) ->
    get_step(Step#step{args = [Ctx]});

get_step(#step{condition = Condition, ctx = Ctx} = Step) when
    is_function(Condition, 1)
->
    get_step(Step#step{condition = Condition(Ctx)});

get_step(#step{condition = true} = Step) ->
    Step;

get_step(#step{condition = false}) ->
    [].


%%--------------------------------------------------------------------
%% @private @doc Returns a list of steps for the nested service action.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_steps(Steps :: #steps{}) -> Steps :: [#step{}].
get_nested_steps(#steps{ctx = Ctx, condition = Condition} = Steps) when
    is_function(Condition, 1)
->
    get_nested_steps(Steps#steps{condition = Condition(Ctx)});

get_nested_steps(#steps{condition = true} = Steps) ->
    get_steps(Steps);

get_nested_steps(#steps{condition = false}) ->
    [].


%%--------------------------------------------------------------------
%% @private @doc Logs the service action progress.
%% @end
%%--------------------------------------------------------------------
%% @formatter:off
-spec log(Msg) -> ok when
    Msg :: service_executor:result() | #action_steps_count{} | #action_begin{}.
%% @formatter:on
log(#action_steps_count{service = Service, action = Action, count = StepsCount}) ->
    ?debug("Executing action ~p:~p requires ~b steps", [Service, Action, StepsCount]);
log(#action_begin{service = Service, action = Action}) ->
    ?debug("Executing action ~p:~p", [Service, Action]);
log(#action_end{service = Service, action = Action, result = ok}) ->
    ?debug("Action ~p:~p completed successfully", [Service, Action]);
log(#action_end{service = Service, action = Action, result = {error, Reason}}) ->
    ?error("Action ~p:~p failed due to: ~tp", [Service, Action, Reason]);
log(#step_begin{module = Module, function = Function}) ->
    ?debug("Executing step ~p:~p", [Module, Function]);
log(#step_end{module = Module, function = Function, good_bad_results = {_, []}}) ->
    ?debug("Step ~p:~p completed successfully", [Module, Function]);
log(#step_end{module = Module, function = Function, good_bad_results = {_, Errors}}) ->
    ?error("Step ~p:~p failed~n~ts", [Module, Function, format_errors(Errors, "")]).


%%--------------------------------------------------------------------
%% @private @doc Formats the service errors into a human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec format_errors(Errors :: [{node(), {error, _}}], Acc :: string()) ->
    Log :: string().
format_errors([], Log) ->
    Log;

format_errors([{Node, {error, _} = Error} | Errors], Log) ->
    ErrorStr = str_utils:format("Node: ~tp~nError: ~tp~n", [Node, Error]),
    format_errors(Errors, Log ++ ErrorStr).


%% @private
-spec bad_results_to_error([onepanel_rpc:result()]) -> ?ERROR_ON_NODES(_, _).
bad_results_to_error(BadResults) ->
    {Nodes, Errors} = lists:unzip(BadResults),

    % @TODO VFS-5838 Better separation of internal results processing
    % and external error reporting
    SerializableErrors = lists:map(fun cast_to_serializable_error/1, Errors),
    NodesErrors = lists:zip(Nodes, SerializableErrors),

    SelectedError = select_error(SerializableErrors),
    Hostnames = [list_to_binary(hosts:from_node(Node))
        || {Node, Err} <- NodesErrors, Err == SelectedError],
    ?ERROR_ON_NODES(SelectedError, Hostnames).


%%--------------------------------------------------------------------
%% @private
%% @doc Selects error most appropriate for request response in case
%% multiple hosts returned different results.
%% @end
%%--------------------------------------------------------------------
-spec select_error(nonempty_list(Error)) -> Error when Error :: errors:error().
select_error(Errors) ->
    % Use http code as a heuristic favoring user-caused errors (4xx) over server errors
    hd(lists:sort(fun(E1, E2) ->
        errors:to_http_code(E1) < errors:to_http_code(E2)
    end, Errors)).


%%--------------------------------------------------------------------
%% @private
%% @doc This function works on best-effort basis since there is
%% no way of accurate and silent errors:error() identification.
%% Guarantees that the result will be at least in {error, term()} format
%% and the reason will not be an #exception record.
%% @TODO VFS-5922 Make the function fully accurate
%% @end
%%--------------------------------------------------------------------
-spec cast_to_serializable_error(T | term()) -> T when T :: errors:error().
cast_to_serializable_error({error, #exception{}}) ->
    ?ERROR_INTERNAL_SERVER_ERROR;
cast_to_serializable_error({error, _} = Error) ->
    Error;
cast_to_serializable_error(_) ->
    ?ERROR_INTERNAL_SERVER_ERROR.

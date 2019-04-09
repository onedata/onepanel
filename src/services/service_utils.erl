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
-export([absolute_path/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a list of steps for the service action.
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Service :: service:name(), Action :: service:action(),
    Ctx :: service:ctx()) -> Steps :: [#step{}].
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
-spec notify(Msg :: {Stage :: service:stage(), Details},
    Notify :: service:notify()) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
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
    {GoodResults :: onepanel_rpc:results(), BadResults :: onepanel_rpc:results()}.
partition_results(Results) ->
    lists:partition(fun
        ({_, #error{}}) -> false;
        (_) -> true
    end, Results).


%%--------------------------------------------------------------------
%% @doc Checks if an error occured during service action execution
%% and returns it.
%% @end
%%--------------------------------------------------------------------
-spec results_contain_error(Results :: service_executor:results() | #error{}) ->
    {true, #error{}} | false.
results_contain_error(#error{} = Error) ->
    {true, Error};

results_contain_error(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, #error{} = Error}}] ->
            {true, Error};

        [{task_finished, {Service, Action, #error{}}}, Step | _Steps] ->
            {Module, Function, {_, BadResults}} = Step,

            ServiceError = #service_error{
                service = Service, action = Action, module = Module,
                function = Function, bad_results = BadResults
            },
            {true, ?make_error(ServiceError)};

        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @doc Throws an exception if an error occurred during service action execution.
%% @end
%%--------------------------------------------------------------------
-spec throw_on_error(Results :: service_executor:results() | #error{}) ->
    service_executor:results() | no_return().
throw_on_error(Results) ->
    case results_contain_error(Results) of
        {true, Error} -> ?throw_error(Error);
        false -> Results
    end.


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
                {badrpc, _} = Error -> ?throw_error(Error);
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

get_step(#step{hosts = undefined, ctx = #{hosts := Hosts}} = Step) ->
    get_step(Step#step{hosts = Hosts});

get_step(#step{hosts = undefined, service = Service} = Step) ->
    Hosts = hosts:all(Service),
    get_step(Step#step{hosts = Hosts});

get_step(#step{hosts = []}) ->
    [];

get_step(#step{hosts = Hosts, verify_hosts = true} = Step) ->
    ClusterHosts = service_onepanel:get_hosts(),
    lists:foreach(fun(Host) ->
        case lists:member(Host, ClusterHosts) of
            true -> ok;
            false -> ?throw_error({?ERR_HOST_NOT_FOUND, Host})
        end
    end, Hosts),
    get_step(Step#step{verify_hosts = false});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = any} = Step) ->
    Host = utils:random_element(Hosts),
    get_step(Step#step{hosts = [Host], selection = all,
        ctx = Ctx#{rest => lists:delete(Host, Hosts)}});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = first} = Step) ->
    get_step(Step#step{hosts = [hd(Hosts)],
        ctx = Ctx#{rest => tl(Hosts)}, selection = all});

get_step(#step{hosts = Hosts, ctx = StepCtx, selection = rest} = Step) ->
    get_step(Step#step{hosts = tl(Hosts), ctx = StepCtx#{first => hd(Hosts)},
        selection = all});

get_step(#step{condition = Condition, ctx = Ctx} = Step) ->
    case Condition(Ctx) of
        true -> Step;
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Returns a list of steps for the nested service action.
%% @end
%%--------------------------------------------------------------------
-spec get_nested_steps(Steps :: #steps{}) -> Steps :: [#step{}].
get_nested_steps(#steps{ctx = Ctx, condition = Condition} = Steps) ->
    case Condition(Ctx) of
        true -> get_steps(Steps);
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private @doc Logs the service action progress.
%% @end
%%--------------------------------------------------------------------
-spec log(Msg :: {Stage :: service:stage(), Details}) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
log({action_begin, {Module, Function}}) ->
    ?debug("Executing action ~p:~p", [Module, Function]);
log({action_end, {Module, Function, ok}}) ->
    ?debug("Action ~p:~p completed successfully", [Module, Function]);
log({action_end, {Module, Function, #error{reason = Reason, stacktrace = []}}}) ->
    ?error("Action ~p:~p failed due to: ~tp", [Module, Function, Reason]);
log({action_end, {Module, Function, #error{reason = Reason,
    stacktrace = Stacktrace}}}) ->
    ?error("Action ~p:~p failed due to: ~tp~nStacktrace: ~tp",
        [Module, Function, Reason, Stacktrace]);
log({step_begin, {Module, Function}}) ->
    ?debug("Executing step ~p:~p", [Module, Function]);
log({step_end, {Module, Function, {_, []}}}) ->
    ?debug("Step ~p:~p completed successfully", [Module, Function]);
log({step_end, {Module, Function, {_, Errors}}}) ->
    ?error("Step ~p:~p failed~n~ts", [Module, Function,
        format_errors(Errors, "")]).


%%--------------------------------------------------------------------
%% @private @doc Formats the service errors into a human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec format_errors(Errors :: [{node(), #error{}}], Acc :: string()) ->
    Log :: string().
format_errors([], Log) ->
    Log;

format_errors([{Node, #error{} = Error} | Errors], Log) ->
    ErrorStr = str_utils:format("Node: ~p~n~ts",
        [Node, onepanel_errors:format_error(Error)]),
    format_errors(Errors, Log ++ ErrorStr).

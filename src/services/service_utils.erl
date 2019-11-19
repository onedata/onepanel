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
-spec notify(Msg :: {Stage :: service:event(), Details},
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
        ({_, {error, _}}) -> false;
        (_) -> true
    end, Results).


%%--------------------------------------------------------------------
%% @doc Checks if an error occured during service action execution
%% and returns it.
%% @end
%%--------------------------------------------------------------------
-spec results_contain_error(Results :: service_executor:results() | {error, _}) ->
    {true, {error, _}} | {true, {error, term()}} | false.
results_contain_error({error, _} = Error) ->
    {true, Error};

results_contain_error(Results) ->
    case lists:reverse(Results) of
        [{task_finished, {_, _, {error, _} = Error}}] ->
            {true, Error};

        [{task_finished, {Service, Action, {error, _}}}, Step | _Steps] ->
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
-spec throw_on_error(Results :: service_executor:results() | {error, _} | {error, term()}) ->
    service_executor:results() | no_return().
throw_on_error(Results) ->
    case results_contain_error(Results) of
        {true, Error} -> ?throw_error(Error);
        false -> Results
    end.


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

get_step(#step{hosts = undefined, ctx = #{hosts := Hosts}} = Step) ->
    get_step(Step#step{hosts = Hosts});

get_step(#step{hosts = undefined, service = Service} = Step) ->
    case hosts:all(Service) of
        [] ->
            % do not silently skip steps because of empty list in service model,
            % unless it is explicitly given in step ctx or hosts field.
            throw(?ERROR_NO_SERVICE_NODES(Service));
        Hosts ->
            get_step(Step#step{hosts = Hosts})
    end;

get_step(#step{hosts = []}) ->
    [];

get_step(#step{hosts = Hosts, verify_hosts = true} = Step) ->
    ok = onepanel_utils:ensure_known_hosts(Hosts),
    get_step(Step#step{verify_hosts = false});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = any} = Step) ->
    Host = utils:random_element(Hosts),
    get_step(Step#step{hosts = [Host], selection = all,
        ctx = Ctx#{rest => lists:delete(Host, Hosts), all => Hosts}});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = first} = Step) ->
    get_step(Step#step{hosts = [hd(Hosts)],
        ctx = Ctx#{rest => tl(Hosts), all => Hosts}, selection = all});

get_step(#step{hosts = Hosts, ctx = Ctx, selection = rest} = Step) ->
    get_step(Step#step{hosts = tl(Hosts),
        ctx = Ctx#{first => hd(Hosts), all => Hosts}, selection = all});

get_step(#step{ctx = Ctx, args = undefined} = Step) ->
    get_step(Step#step{args = [Ctx]});

get_step(#step{condition = Condition, ctx = Ctx} = Step) when
    is_function(Condition, 1) ->
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
    is_function(Condition, 1) ->
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
-spec log(Msg :: {Event :: service:event(), Details}) -> ok when
    Details  :: {Service, Action}  | {Service, Action, ok | {error, _}}
              | {Service, Action, StepsCount :: integer()}
              | {Module, Function} | {Module, Function, Result},
    Service  :: service:name(),
    Module   :: module(),
    Action   :: service:action(),
    Function :: atom(),
    Result   :: term().
%% @formatter:on
log({action_steps_count, {Service, Action, StepsCount}}) ->
    ?debug("Executing action ~p:~p requires ~b steps", [Service, Action, StepsCount]);
log({action_begin, {Service, Action}}) ->
    ?debug("Executing action ~p:~p", [Service, Action]);
log({action_end, {Service, Action, ok}}) ->
    ?debug("Action ~p:~p completed successfully", [Service, Action]);
log({action_end, {Service, Action, {error, Reason}}}) ->
    ?error("Action ~p:~p failed due to: ~tp", [Service, Action, Reason]);
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
-spec format_errors(Errors :: [{node(), {error, _}}], Acc :: string()) ->
    Log :: string().
format_errors([], Log) ->
    Log;

format_errors([{Node, {error, _} = Error} | Errors], Log) ->
    ErrorStr = str_utils:format("Node: ~tp~n~ts",
        [Node, onepanel_errors:format_error(Error)]),
    format_errors(Errors, Log ++ ErrorStr).

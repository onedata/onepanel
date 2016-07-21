%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include("names.hrl").
-include("service.hrl").
-include("modules/logger.hrl").

%% Model behaviour callbacks
-export([get_fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

%% API
-export([start/1, start/2, stop/1, status/1, apply/3, apply/4]).
-export([get_module/1, get_hosts/1, get_nodes/1, is_member/2, add_host/2]).

-type name() :: atom().
-type action() :: atom().
-type ctx() :: #{}.
-type notify() :: pid() | undefined.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: fun((ctx()) -> boolean()).
-type stage() :: action_begin | action_end | step_begin | step_end.

-export_type([name/0, action/0, ctx/0, host/0, step/0, condition/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see model_behaviour:get_fields/0
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:create/1
%%--------------------------------------------------------------------
-spec create(Record :: model_behaviour:record()) ->
    ok | #error{} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:save/1
%%--------------------------------------------------------------------
-spec save(Record :: model_behaviour:record()) -> ok | no_return().
save(Record) ->
    model:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:update/2
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | no_return().
update(Key, Diff) ->
    model:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:get/1
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | #error{} | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:exists/1
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    model:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:delete/1
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | no_return().
delete(Key) ->
    model:delete(?MODULE, Key).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv service:start(InitScript, #{})
%%--------------------------------------------------------------------
-spec start(InitScript :: string()) -> ok | no_return().
start(InitScript) ->
    service:start(InitScript, #{}).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec start(InitScript :: string(), SystemLimits :: #{}) -> ok | no_return().
start(InitScript, SystemLimits) ->
    Tokens = maps:fold(fun
        (open_files, Value, Acc) -> ["ulimit", "-n", Value, ";" | Acc];
        (_, _, Acc) -> Acc
    end, ["service", InitScript, "start"], SystemLimits),
    onepanel_shell:check_call(Tokens).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec stop(InitScript :: string()) -> ok | no_return().
stop(InitScript) ->
    onepanel_shell:check_call(["service", InitScript, "stop"]).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec status(InitScript :: string()) -> running | stopped | not_found.
status(InitScript) ->
    case onepanel_shell:call(["service", InitScript, "status"]) of
        0 -> running;
        2 -> stopped;
        127 -> missing
    end.


%%--------------------------------------------------------------------
%% @doc @equiv apply(Service, Action, Ctx, undefined)
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx()) ->
    ok | {error, Reason :: term()}.
apply(Service, Action, Ctx) ->
    apply(Service, Action, Ctx, undefined).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply(Service :: name(), Action :: action(), Ctx :: ctx(), Notify :: notify()) ->
    ok | {error, Reason :: term()}.
apply([], _Action, _Ctx, _Notify) ->
    ok;

apply(Service, Action, Ctx, Notify) ->
    notify({action_begin, {Service, Action}}, Notify),
    Result = try
        Steps = get_steps(Service, Action, Ctx, false),
        ?log_info("Execution of ~p:~p requires following steps:~n~s",
            [Service, Action, format_steps(Steps, "")]),
        apply_steps(Steps, Notify)
    catch
        _:Reason -> ?error(Reason)
    end,
    notify({action_end, {Service, Action, Result}}, Notify),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_module(Service :: name()) -> Module :: module().
get_module(Service) ->
    erlang:list_to_atom("service_" ++ erlang:atom_to_list(Service)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_hosts(Service :: name()) -> Hosts :: [host()].
get_hosts(Service) ->
    {ok, #service{hosts = Hosts}} = service:get(Service),
    Hosts.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_nodes(Service :: name()) -> Nodes :: [node()].
get_nodes(Service) ->
    onepanel_cluster:hosts_to_nodes(Service, get_hosts(Service)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec is_member(Key :: model_behaviour:key(), Host :: host()) ->
    boolean() | {error, Reason :: term()}.
is_member(Key, Host) ->
    model:transaction(fun() ->
        case ?MODULE:get(Key) of
            {ok, #service{hosts = Hosts}} -> lists:member(Host, Hosts);
            _ -> false
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_host(Name :: name(), Host :: host()) -> ok.
add_host(Name, Host) ->
    ?MODULE:update(Name, fun(#service{hosts = Hosts} = Service) ->
        Service#service{hosts = [Host | lists:delete(Host, Hosts)]}
    end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_steps(Steps :: [step()], Notify :: notify()) -> ok | #error{}.
apply_steps([], _Notify) ->
    ok;

apply_steps([#step{hosts = []} | Steps], Notify) ->
    apply_steps(Steps, Notify);

apply_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args, ignore_errors = IgnoreErrors} | Steps], Notify) ->

    Nodes = onepanel_cluster:hosts_to_nodes(Hosts),
    notify({step_begin, {Module, Function}}, Notify),

    Results = onepanel_rpc:call(Nodes, Module, Function, Args),
    Status = partition_results(Results),

    notify({step_end, {Module, Function, Status}}, Notify),

    case {Status, IgnoreErrors} of
        {{_, []}, _} -> apply_steps(Steps, Notify);
        {_, true} -> apply_steps(Steps, Notify);
        {_, _} -> ?error({Module, Function, Status})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Service :: name(), Action :: action(), Ctx :: ctx(),
    IgnoreErrors :: boolean()) -> Steps :: [#step{}].
get_steps(Service, Action, Ctx, IgnoreErrors) ->
    Module = get_module(Service),
    Steps = Module:get_steps(Action, Ctx),
    lists:flatten(lists:map(fun
        (#step{} = Step) ->
            get_step(Service, Step, Ctx, IgnoreErrors);
        (#steps{} = NestedSteps) ->
            get_nested_steps(Service, NestedSteps, Ctx, IgnoreErrors)
    end, Steps)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_step(Service :: name(), Step :: #step{}, Ctx :: ctx(),
    IgnoreErrors :: boolean()) -> Step :: [] | #step{}.
get_step(Service, #step{service = undefined} = Step, Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{service = Service}, Ctx, IgnoreErrors);

get_step(Service, #step{service = StepService, module = undefined} = Step, Ctx,
    IgnoreErrors) ->
    Module = get_module(StepService),
    get_step(Service, Step#step{module = Module}, Ctx, IgnoreErrors);

get_step(Service, #step{ctx = undefined} = Step, Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{ctx = Ctx}, Ctx, IgnoreErrors);

get_step(Service, #step{ctx = Ctx, args = undefined} =
    Step, _Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{args = [Ctx]}, Ctx, IgnoreErrors);

get_step(Service, #step{ignore_errors = undefined} = Step, Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{ignore_errors = IgnoreErrors}, Ctx,
        IgnoreErrors);

get_step(_Service, #step{hosts = []} = Step, _Ctx, _IgnoreErrors) ->
    Step;

get_step(Service, #step{hosts = undefined, ctx = #{hosts := Hosts}} = Step, Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = Hosts}, Ctx, IgnoreErrors);

get_step(Service, #step{hosts = undefined, service = StepService} = Step, Ctx,
    IgnoreErrors) ->
    StepHosts = case service:get(StepService) of
        {ok, #service{hosts = Hosts}} -> Hosts;
        #error{reason = ?ERR_NOT_FOUND} -> onepanel_cluster:nodes_to_hosts()
    end,
    get_step(Service, Step#step{hosts = StepHosts}, Ctx, IgnoreErrors);

get_step(Service, #step{hosts = Hosts, ctx = Ctx, selection = any} = Step, _Ctx,
    IgnoreErrors) ->
    Host = utils:random_element(Hosts),
    get_step(Service, Step#step{hosts = [Host], selection = all,
        ctx = Ctx#{rest => lists:delete(Host, Hosts)}}, _Ctx, IgnoreErrors);

get_step(Service, #step{hosts = Hosts, ctx = Ctx, selection = first} = Step, _Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = [hd(Hosts)], ctx = Ctx#{rest => tl(Hosts)},
        selection = all}, _Ctx, IgnoreErrors);

get_step(Service, #step{hosts = Hosts, ctx = Ctx, selection = rest} = Step, _Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = tl(Hosts), ctx = Ctx#{first => hd(Hosts)},
        selection = all}, _Ctx, IgnoreErrors);

get_step(_Service, #step{condition = Condition, ctx = Ctx} = Step, _Ctx,
    _IgnoreErrors) ->
    case Condition(Ctx) of
        true -> Step;
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_nested_steps(Service :: name(), Steps :: #steps{}, Ctx :: ctx(),
    IgnoreErrors :: boolean()) -> Steps :: [step()].
get_nested_steps(Service, #steps{service = undefined} = Steps, Ctx,
    IgnoreErrors) ->
    get_nested_steps(Service, Steps#steps{service = Service}, Ctx,
        IgnoreErrors);

get_nested_steps(Service, #steps{ctx = undefined} = Steps, Ctx, IgnoreErrors) ->
    get_nested_steps(Service, Steps#steps{ctx = Ctx}, Ctx, IgnoreErrors);

get_nested_steps(Service, #steps{ignore_errors = undefined} = Steps, Ctx,
    IgnoreErrors) ->
    get_nested_steps(Service, Steps#steps{ignore_errors = IgnoreErrors}, Ctx,
        IgnoreErrors);

get_nested_steps(_Service, #steps{service = Service, action = Action, ctx = Ctx,
    condition = Condition, ignore_errors = IgnoreErrors}, _Ctx, _IgnoreErrors) ->
    case Condition(Ctx) of
        true -> get_steps(Service, Action, Ctx, IgnoreErrors);
        false -> []
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec notify(Msg :: {Stage :: stage(), Details}, Notify :: notify()) ->
    ok when
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
%% @private
%% @doc
%% @todo write me!
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
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec log(Msg :: {Stage :: stage(), Details}) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
log({action_begin, {Module, Function}}) ->
    ?log_info("Executing action ~p:~p", [Module, Function]);
log({action_end, {Module, Function, ok}}) ->
    ?log_info("Action ~p:~p completed successfully", [Module, Function]);
log({action_end, {Module, Function, #error{reason = Reason, stacktrace = []}}}) ->
    ?log_error("Action ~p:~p failed due to: ~p", [Module, Function, Reason]);
log({action_end, {Module, Function, #error{reason = Reason,
    stacktrace = Stacktrace}}}) ->
    ?log_error("Action ~p:~p failed due to: ~p~nStacktrace: ~p",
        [Module, Function, Reason, Stacktrace]);
log({step_begin, {Module, Function}}) ->
    ?log_info("Executing step ~p:~p", [Module, Function]);
log({step_end, {Module, Function, {_, []}}}) ->
    ?log_info("Step ~p:~p completed successfully", [Module, Function]);
log({step_end, {Module, Function, {_, Errors}}}) ->
    ?log_error("Step ~p:~p failed~n~s", [Module, Function,
        format_errors(Errors, "")]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec format_steps(Steps :: [step()], Acc :: string()) -> Log :: string().
format_steps([], Log) ->
    Log;
format_steps([#step{hosts = Hosts, module = Module, function = Function,
    args = Args, ignore_errors = IgnoreErrors} | Steps], Log) ->
    Step = io_lib:format("Hosts: ~p~nFunction: ~p:~p~nArgs: ~p~n"
    "Ignore errors: ~p~n~n", [Hosts, Module, Function, Args, IgnoreErrors]),
    format_steps(Steps, Log ++ Step).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec format_errors(Errors :: [{Node, {error, Reason}} |{Node, {error, Reason,
    Stacktrace}}], Acc :: string()) -> Log :: string() when
    Node :: node(),
    Reason :: term(),
    Stacktrace :: term().
format_errors([], Log) ->
    Log;
format_errors([{Node, #error{reason = Reason, stacktrace = []}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~n", [Node, Reason]),
    format_errors(Errors, Log ++ Error);
format_errors([{Node, #error{reason = Reason, stacktrace = Stacktrace}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~nStacktrace: ~p~n",
        [Node, Reason, Stacktrace]),
    format_errors(Errors, Log ++ Error).
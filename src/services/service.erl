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

-include("db/models.hrl").
-include("onepanel.hrl").
-include("service.hrl").
-include_lib("ctool/include/logging.hrl").

%% Model behaviour callbacks
-export([fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

%% API
-export([start/1, start/2, stop/1, status/1, apply/3]).
-export([nodes/1, param/2, module/1, domain/2, add_host/2]).

-type name() :: atom().
-type action() :: atom().
-type ctx() :: #{}.
-type host() :: string().
-type step() :: #step{} | #steps{}.
-type condition() :: fun((ctx()) -> boolean()).
-type stage() :: action_begin | action_end | step_begin | step_end.

-export_type([name/0, action/0, ctx/0, host/0, step/0, condition/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see model_behaviour:fields/0
%%--------------------------------------------------------------------
-spec fields() -> list(atom()).
fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:create/1
%%--------------------------------------------------------------------
-spec create(Record :: model_behaviour:record()) ->
    ok | {error, Reason :: term()}.
create(Record) ->
    model_logic:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:save/1
%%--------------------------------------------------------------------
-spec save(Record :: model_behaviour:record()) -> ok | {error, Reason :: term()}.
save(Record) ->
    model_logic:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:update/2
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | {error, Reason :: term()}.
update(Key, Diff) ->
    model_logic:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:get/1
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | {error, Reason :: term()}.
get(Key) ->
    model_logic:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:exists/1
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | {error, Reason :: term()}.
exists(Key) ->
    model_logic:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:delete/1
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | {error, Reason :: term()}.
delete(Key) ->
    model_logic:delete(?MODULE, Key).

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
        (processes, Value, Acc) -> ["ulimit", "-u", Value, ";" | Acc];
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
-spec status(InitScript :: string()) -> ok | no_return().
status(InitScript) ->
    onepanel_shell:check_call(["service", InitScript, "status"]).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply(Services :: [name()], Action :: action(), Ctx :: ctx()) ->
    ok | {error, Reason :: term()}.
apply([], _Action, _Ctx) ->
    ok;

apply([Service | Services], Action, Ctx) ->
    notify({action_begin, {Service, Action}}, Ctx),
    Result = try
        Steps = get_steps(Service, Action, Ctx, false),
        ?critical("~n~n~n~nSteps:~n~p~n~n~n~n", [Steps]),
        apply_steps(Steps)
    catch
        error:undef ->
            {error, service_not_found, erlang:get_stacktrace()};
        error:function_clause ->
            {error, action_not_supported, erlang:get_stacktrace()};
        _:Reason ->
            {error, Reason, erlang:get_stacktrace()}
    end,
    notify({action_end, {Service, Action, Result}}, Ctx),
    case Result of
        ok -> service:apply(Services, Action, Ctx);
        _ -> Result
    end;

apply(Service, Action, Ctx) ->
    service:apply([Service], Action, Ctx).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec nodes(Hosts :: [host()]) -> Nodes :: [node()].
nodes(Hosts) ->
    HostsToNodes = lists:foldl(fun(Node, Map) ->
        maps:put(onepanel_utils:node_to_host(Node), Node, Map)
    end, #{}, onepanel:nodes()),
    lists:foldl(fun(Host, Nodes) ->
        case maps:find(Host, HostsToNodes) of
            {ok, Node} -> [Node | Nodes];
            error -> throw({host_not_found, Host})
        end
    end, [], Hosts).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec param(Name :: name(), Ctx :: ctx()) -> Value :: term().
param(Name, Ctx) ->
    case maps:find(Name, Ctx) of
        {ok, Value} -> Value;
        error -> onepanel:get_env(Name)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec module(Service :: name()) -> Module :: module().
module(Service) ->
    erlang:list_to_atom("service_" ++ erlang:atom_to_list(Service)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec domain(Key :: atom(), Ctx :: ctx()) -> Domain :: string() | no_return().
domain(Key, Ctx) ->
    case maps:find(Key, Ctx) of
        {ok, Domain} -> Domain;
        error ->
            Hostname = onepanel_shell:check_output(["hostname", "-f"]),
            case string:tokens(Hostname, ".") of
                [_] -> throw({short_hostname, Hostname});
                [_ | Domain] -> string:join(Domain, ".")
            end
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec add_host(Name :: name(), Host :: host()) -> ok.
add_host(Name, Host) ->
    ok = service:update(Name, fun(#service{hosts = Hosts} = Service) ->
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
-spec apply_steps(Steps :: [step()]) -> ok | {error, Reason :: term()}.
apply_steps([]) ->
    ok;

apply_steps([#step{hosts = []} | Steps]) ->
    apply_steps(Steps);

apply_steps([#step{hosts = Hosts, module = Module, function = Function,
    ctx = Ctx, ignore_errors = IgnoreErrors} | Steps]) ->

    Nodes = service:nodes(Hosts),
    notify({step_begin, {Module, Function}}, Ctx),

    Results = onepanel_rpc:call(Nodes, Module, Function, [Ctx], ?RPC_TIMEOUT),
    Status = case filter_errors(Results) of
        [] -> ok;
        Errors -> {errors, Errors}
    end,

    notify({step_end, {Module, Function, Status}}, Ctx),

    case {Status, IgnoreErrors} of
        {ok, _} -> apply_steps(Steps);
        {_, true} -> apply_steps(Steps);
        {_, _} -> {error, {Module, Function, Status}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_steps(Service :: name(), Action :: action(), Ctx :: ctx(),
    IgnoreErrors :: boolean()) -> Steps :: [#step{}].
get_steps(Service, Action, Ctx, IgnoreErrors) ->
    Module = module(Service),
    Steps = Module:get_steps(Action, Ctx),
    lists:flatten(lists:map(fun
        (#step{} = Step) ->
            get_step(Service, Step, Ctx, IgnoreErrors);
        (#steps{} = NestedSteps) ->
            get_nested_steps(Service, NestedSteps, Ctx, IgnoreErrors)
    end, Steps)).


%%--------------------------------------------------------------------
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
    Module = module(StepService),
    get_step(Service, Step#step{module = Module}, Ctx, IgnoreErrors);

get_step(Service, #step{ctx = undefined} = Step, Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{ctx = Ctx}, Ctx, IgnoreErrors);

get_step(Service, #step{ignore_errors = undefined} = Step, Ctx, IgnoreErrors) ->
    get_step(Service, Step#step{ignore_errors = IgnoreErrors}, Ctx,
        IgnoreErrors);

get_step(Service, #step{hosts = undefined, ctx = #{hosts := Hosts}} = Step, Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = Hosts}, Ctx, IgnoreErrors);

get_step(Service, #step{hosts = undefined, service = StepService} = Step, Ctx,
    IgnoreErrors) ->
    {ok, #service{hosts = Hosts}} = service:get(StepService),
    get_step(Service, Step#step{hosts = Hosts}, Ctx, IgnoreErrors);

get_step(Service, #step{hosts = Hosts, selection = first} = Step, Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = [hd(Hosts)], selection = all}, Ctx,
        IgnoreErrors);

get_step(Service, #step{hosts = Hosts, selection = rest} = Step, Ctx,
    IgnoreErrors) ->
    get_step(Service, Step#step{hosts = tl(Hosts), selection = all}, Ctx,
        IgnoreErrors);

get_step(_Service, #step{condition = Condition, ctx = Ctx} = Step, _Ctx,
    _IgnoreErrors) ->
    case Condition(Ctx) of
        true -> Step;
        false -> []
    end.


%%--------------------------------------------------------------------
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
-spec notify(Msg :: {Stage :: stage(), Details}, Ctx :: ctx()) ->
    ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
notify(Msg, #{notify := Pid} = Ctx) ->
    log(Msg, Ctx),
    Pid ! Msg,
    ok;

notify(Msg, Ctx) ->
    log(Msg, Ctx),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec filter_errors(Results :: onepanel_rpc:results()) ->
    BadResults :: onepanel_rpc:results().
filter_errors(Results) ->
    lists:filter(fun
        ({_, {error, _}}) -> true;
        ({_, {error, _, _}}) -> true;
        (_) -> false
    end, Results).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec log(Msg :: {Stage :: stage(), Details}, Ctx :: ctx()) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: term().
log({action_begin, {Module, Function}}, Ctx) ->
    ?info("Executing action ~p:~p with context: ~p", [Module, Function, Ctx]);
log({action_end, {Module, Function, ok}}, _Ctx) ->
    ?info("Action ~p:~p completed successfully", [Module, Function]);
log({action_end, {Module, Function, {error, _}}}, _Ctx) ->
    ?error("Action ~p:~p failed", [Module, Function]);
log({action_end, {Module, Function, {error, Reason, Stacktrace}}}, _Ctx) ->
    ?error("Action ~p:~p failed due to: ~p~nStacktrace: ~p",
        [Module, Function, Reason, Stacktrace]);
log({step_begin, {Module, Function}}, Ctx) ->
    ?info("Executing step ~p:~p with context: ~p", [Module, Function, Ctx]);
log({step_end, {Module, Function, ok}}, _Ctx) ->
    ?info("Step ~p:~p completed successfully", [Module, Function]);
log({step_end, {Module, Function, {errors, Errors}}}, _Ctx) ->
    ?error("Step ~p:~p failed~n~s", [Module, Function,
        format_errors(Errors, "")]).


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
format_errors([{Node, {error, Reason}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~n", [Node, Reason]),
    format_errors(Errors, Log ++ Error);
format_errors([{Node, {error, Reason, Stacktrace}} | Errors], Log) ->
    Error = io_lib:format("Node: ~p~nReason: ~p~nStacktrace: ~p~n",
        [Node, Reason, Stacktrace]),
    format_errors(Errors, Log ++ Error).
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

-include("onepanel.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([apply/3]).

-type name() :: atom().
-type action() :: atom().
-type step() :: {Nodes :: [node()], Function :: atom()} |
{Nodes :: [node()], Module :: module(), Function :: atom()}.
-type args() :: #{}.
-type stage() :: action_begin | action_end | step_begin | step_end.

-export_type([name/0, action/0, step/0, args/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply(Services :: [name()], Action :: action(), Args :: args()) ->
    ok | {error, Reason :: term()}.
apply([], _Action, _Args) ->
    ok;

apply([Service | Services], Action, Args) ->
    notify({action_begin, {Service, Action}}, Args),
    Result = try
        Steps = Service:get_steps(Action, Args),
        ?info("Steps: ~p", [Steps]),
        apply_steps(Service, Steps, Args)
    catch
        error:undef -> {error, service_not_found};
        error:function_clause -> {error, action_not_supported};
        _:Reason -> {error, Reason}
    end,
    notify({action_end, {Service, Action, Result}}, Args),
    case Result of
        ok -> service:apply(Services, Action, Args);
        _ -> Result
    end;

apply(Service, Action, Args) ->
    service:apply([Service], Action, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_steps(Service :: name(), Steps :: [step()], Args :: args()) ->
    ok | {error, {onepanel_rpc, BadResults :: onepanel_rpc:results()}}.
apply_steps(_Service, [], _Args) ->
    ok;

apply_steps(Service, [{Nodes, Function} | Steps], Args) ->
    apply_steps(Service, [{Nodes, Service, Function} | Steps], Args);

apply_steps(Service, [{Nodes, Module, Function} | Steps], Args) ->
    notify({step_begin, {Module, Function}}, Args),
    Results = onepanel_rpc:call(Nodes, Module, Function, [Args], ?RPC_TIMEOUT),
    case filter_errors(Results) of
        [] ->
            notify({step_end, {Module, Function, ok}}, Args),
            apply_steps(Service, Steps, Args);
        Errors ->
            notify({step_end, {Module, Function, {error, Errors}}}, Args),
            {error, step_failure}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec notify(Msg :: {Stage :: stage(), Details}, Args :: args()) -> ok when
    Details :: {Module, Function} | {Module, Function, Result},
    Module :: module(),
    Function :: atom(),
    Result :: ok | {error, Reason :: term()}.
notify(Msg, #{notify := Pid}) ->
    Pid ! Msg,
    ok;

notify(_Msg, _Args) ->
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
        (_) -> false
    end, Results).
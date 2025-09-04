%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for returning formatted service task results.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_task_get_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("middleware/middleware.hrl").
-include("modules/errors.hrl").
-include("service.hrl").

-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: undefined.
-type state() :: #onp_req_state{input :: input()}.
-type output() :: map().

-export_type([t/0, input/0, state/0, output/0]).


%%%===================================================================
%%% middleware_handler callbacks
%%%===================================================================


-spec supported_interfaces() -> {true, [rest]}.
supported_interfaces() ->
    {true, [rest]}.


-spec service_availability_requirements(middleware_handler:req_ctx()) -> false.
service_availability_requirements(_) ->
    false.


-spec preauthorize(state()) -> boolean().
preauthorize(#onp_req_state{ctx = #onp_req_ctx{client = Client}}) ->
    middleware_handler_utils:is_cluster_member(Client).


-spec validate(state()) -> ok | no_return().
validate(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {task, Id}}}}) ->
    case service:exists_task(Id) of
        true -> ok;
        false -> throw(?ERROR_NOT_FOUND)
    end.


-spec process(state()) -> {ok, output()} | no_return().
process(#onp_req_state{ctx = #onp_req_ctx{gri = #gri{aspect = {task, Id}}}}) ->
    {ok, format_service_task_results(service:get_results(Id))}.


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Response) ->
    {ok, ?OK_REPLY(Response)}.


%%%===================================================================
%%% internal functions
%%%===================================================================


%% @private
-spec format_service_task_results(Results :: {service_executor:results(), Total}) ->
    json_utils:json_map()
    when Total :: non_neg_integer() | {error, _}.
format_service_task_results({{error, _} = Error, _}) ->
    throw(Error);

format_service_task_results({Results, TotalSteps}) ->
    Base = case TotalSteps of
        _ when is_integer(TotalSteps) -> #{totalSteps => TotalSteps};
        _Error -> #{}
    end,

    case service_utils:results_contain_error(Results) of
        {true, Error} ->
            Base2 = maps:merge(Base#{
                <<"status">> => <<"error">>
            }, format_error(Error)),

            case format_service_task_steps(Results) of
                [] -> Base2;
                StepNames -> Base2#{steps => StepNames}
            end;
        false ->
            case lists:reverse(Results) of
                [#action_end{} | _] ->
                    Base#{
                        <<"status">> => <<"ok">>,
                        <<"steps">> => format_service_task_steps(Results)
                    };
                _ ->
                    Base#{
                        <<"status">> => <<"running">>,
                        <<"steps">> => format_service_task_steps(Results)
                    }
            end
    end.


%% @private
-spec format_error(Reason :: term()) -> json_utils:json_map().
format_error({error, #exception{}}) ->
    #{<<"error">> => errors:to_json(?ERR_INTERNAL_SERVER_ERROR(?err_ctx(), undefined))};
format_error(Reason) ->
    #{<<"error">> => errors:to_json(Reason)}.


%% @private
-spec format_service_task_steps(Steps :: [service_executor:step_result()]) ->
    [binary()].
format_service_task_steps(Steps) ->
    lists:filtermap(fun
        (#step_begin{module = Module, function = Function}) ->
            {true, onepanel_utils:join([Module, Function], <<":">>)};
        (_) ->
            false
    end, Steps).

%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Middleware handler for performing DNS check and returning its results.
%%% @end
%%%-------------------------------------------------------------------
-module(panel_dns_check_middleware_handler).
-author("Bartosz Walkowicz").

-behaviour(middleware_handler).

-include("deployment_progress.hrl").
-include("middleware/middleware.hrl").
-include("modules/onepanel_dns.hrl").

-export([
    supported_interfaces/0,
    service_availability_requirements/1,
    preauthorize/1,
    validate/1,
    process/1,
    translate_output/2
]).

-type t() :: ?MODULE.
-type input() :: #{forceCheck => boolean()}.
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


-spec validate(state()) -> ok | errors:error().
validate(_) ->
    middleware_handler_utils:validate_cluster_deployed().


-spec process(state()) -> {ok, output()} | errors:error().
process(#onp_req_state{input = Input}) ->
    ClusterWorker = middleware_handler_utils:get_worker_service(),
    Ctx = #{force_check => maps:get(forceCheck, Input, false)},
    middleware_handler_utils:ok_result(middleware_utils:result_from_service_action(
        ClusterWorker, dns_check, Ctx, dns_check, get
    )).


-spec translate_output(state(), output()) ->
    {ok, middleware_handler:rest_output()}.
translate_output(#onp_req_state{ctx = #onp_req_ctx{interface = rest}}, Result) ->
    IpOrTxtToBinary = fun
        ({_, _, _, _} = Ip) ->
            {ok, Binary} = ip_utils:to_binary(Ip),
            Binary;
        (String) ->
            onepanel_utils:convert(String, binary)
    end,

    JsonMap = maps:fold(fun
        (Key, #dns_check{summary = Summary, expected = E, got = G, bind_records = Records}, Acc) ->
            Acc#{str_utils:to_binary(Key) => #{
                <<"summary">> => Summary,
                <<"expected">> => lists:map(IpOrTxtToBinary, E),
                <<"got">> => lists:map(IpOrTxtToBinary, G),
                <<"recommended">> => Records
            }};
        (timestamp, Seconds, Acc) ->
            Acc#{<<"timestamp">> => time:seconds_to_iso8601(Seconds)};
        (Key, LiteralValue, Acc) ->
            Acc#{str_utils:to_binary(Key) => LiteralValue}
    end, #{}, Result),
    {ok, ?OK_REPLY(JsonMap)}.

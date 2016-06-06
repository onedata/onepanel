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
-module(onepanel_shell).
-author("Krzysztof Trzepla").

-include_lib("ctool/include/logging.hrl").

%% API
-export([output/1, check_output/1, wait_output/3, wait_output/4,
    call/1, check_call/1, wait_call/2, wait_call/3]).

-type token() :: atom() | integer() | string().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec output(Tokens :: [token()]) -> Output :: string().
output(Tokens) ->
    Cmd = string:strip(lists:foldl(fun
        (Token, Acc) when is_atom(Token) ->
            string:join([Acc, erlang:atom_to_list(Token)], " ");
        (Token, Acc) when is_integer(Token) ->
            string:join([Acc, erlang:integer_to_list(Token)], " ");
        (Token, Acc) ->
            string:join([Acc, Token], " ")
    end, "", Tokens)),
    Result = string:strip(os:cmd(Cmd), right, $\n),
    ?info("Shell command: '~s' returned: '~s'", [Cmd, Result]),
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_output(Tokens :: [token()]) -> Output :: string() | no_return().
check_output(Tokens) ->
    Result = output(["R=`"] ++ Tokens ++ ["2>&1`;", "echo", "-n", "$?,$R"]),
    [CodeStr | OutputTokens] = string:tokens(Result, ","),
    Code = erlang:list_to_integer(CodeStr),
    Output = string:join(OutputTokens, ","),
    case Code of
        0 -> Output;
        _ -> throw({shell_error, Code, Output})
    end.


%%--------------------------------------------------------------------
%% @doc @equiv wait_output(Tokens, Expects, Attempts, timer:seconds(1))
%%--------------------------------------------------------------------
-spec wait_output(Tokens :: [token()], Expected :: string(), Attempts :: integer()) ->
    ok | no_return().
wait_output(Tokens, Expected, Attempts) ->
    wait_output(Tokens, Expected, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec wait_output(Tokens :: [token()], Expected :: string(),
    Attempts :: integer(), Delay :: integer()) -> ok | no_return().
wait_output(Tokens, Expected, Attempts, Delay) ->
    Validator = fun(Result) ->
        {match, [_]} = re:run(Result, Expected, [{capture, first, list}])
    end,
    onepanel_utils:wait_until(?MODULE, check_output, [Tokens],
        {validator, Validator}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec call(Tokens :: [token()]) -> Code :: integer().
call(Tokens) ->
    Code = output(Tokens ++ ["1>/dev/null", "2>&1;", "echo", "-n", "$?"]),
    erlang:list_to_integer(Code).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_call(Tokens :: [token()]) -> ok | no_return().
check_call(Tokens) ->
    case call(Tokens) of
        0 -> ok;
        Code -> throw({shell_error, Code})
    end.


%%--------------------------------------------------------------------
%% @doc @equiv wait_call(Tokens, Attempts, timer:seconds(1))
%%--------------------------------------------------------------------
-spec wait_call(Tokens :: [token()], Attempts :: integer()) ->
    ok | no_return().
wait_call(Tokens, Attempts) ->
    wait_call(Tokens, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc @equiv wait_call(Tokens, Attempts, timer:seconds(1))
%%--------------------------------------------------------------------
-spec wait_call(Tokens :: [token()], Attempts :: integer(), Delay :: integer()) ->
    ok | no_return().
wait_call(Tokens, Attempts, Delay) ->
    onepanel_utils:wait_until(?MODULE, check_call, [Tokens],
        {equal, ok}, Attempts, Delay).
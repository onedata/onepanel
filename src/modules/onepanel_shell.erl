%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides shell utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_shell).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([output/1, check_output/1, wait_output/3, wait_output/4,
    call/1, check_call/1, wait_call/2, wait_call/3]).
-export([sed/3, mktemp/0]).

-type token() :: atom() | integer() | string() | binary().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Evaluates the shell command and returns the result.
%% @end
%%--------------------------------------------------------------------
-spec output(Tokens :: [token()]) -> Output :: string().
output(Tokens) ->
    Cmd = erlang:binary_to_list(onepanel_utils:join(Tokens, <<" ">>)),
    Result = string:strip(os:cmd(Cmd), right, $\n),
    ?debug("Shell command: '~s' returned: '~s'", [Cmd, Result]),
    Result.


%%--------------------------------------------------------------------
%% @doc Evaluates the shell command and returns the result. If the command
%% return exit code different from 0, throws an exception.
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
        _ -> ?throw_error({Code, Output})
    end.


%%--------------------------------------------------------------------
%% @doc @equiv wait_output(Tokens, Expects, Attempts, timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_output(Tokens :: [token()], Expected :: string(), Attempts :: integer()) ->
    ok | no_return().
wait_output(Tokens, Expected, Attempts) ->
    wait_output(Tokens, Expected, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Waits until shell command returns expected result. In case of a timeout
%% throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec wait_output(Tokens :: [token()], Expected :: string(),
    Attempts :: integer(), Delay :: integer()) -> ok | no_return().
wait_output(Tokens, Expected, Attempts, Delay) ->
    Validator = fun(Result) ->
        {match, [_]} = re:run(Result, Expected, [{capture, first, list}]),
        ok
    end,
    onepanel_utils:wait_until(?MODULE, check_output, [Tokens],
        {validator, Validator}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Evaluates shell command and returns exit code.
%% @end
%%--------------------------------------------------------------------
-spec call(Tokens :: [token()]) -> Code :: integer().
call(Tokens) ->
    LogFile = onepanel_env:get(cmd_log_file),
    Code = output(Tokens ++ ["1>>" ++ LogFile, "2>&1;", "echo", "-n", "$?"]),
    erlang:list_to_integer(Code).


%%--------------------------------------------------------------------
%% @doc Evaluates shell command and in case of an exit code different from 0,
%% throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec check_call(Tokens :: [token()]) -> ok | no_return().
check_call(Tokens) ->
    case call(Tokens) of
        0 -> ok;
        Code -> ?throw_error(Code)
    end.


%%--------------------------------------------------------------------
%% @doc @equiv wait_call(Tokens, Attempts, timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_call(Tokens :: [token()], Attempts :: integer()) ->
    ok | no_return().
wait_call(Tokens, Attempts) ->
    wait_call(Tokens, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Waits until shell command completes successfully, i.e. exit code is
%% equal to 0. In case of a timeout throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec wait_call(Tokens :: [token()], Attempts :: integer(), Delay :: integer()) ->
    ok | no_return().
wait_call(Tokens, Attempts, Delay) ->
    onepanel_utils:wait_until(?MODULE, check_call, [Tokens],
        {equal, ok}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Wrapper for shell sed program.
%% @end
%%--------------------------------------------------------------------
-spec sed(Pattern :: string(), Replacement :: string(), Path :: file:name_all()) ->
    ok | no_return().
sed(Pattern, Replacement, Path) ->
    onepanel_shell:check_call(["sed", "-i", "-e", "'s/" ++ Pattern ++ "/"
        ++ Replacement ++ "/g'", Path]).


%%--------------------------------------------------------------------
%% @doc Creates temporary directory.
%% @end
%%--------------------------------------------------------------------
-spec mktemp() -> Path :: string().
mktemp() ->
    lib:nonl(os:cmd("mktemp")).
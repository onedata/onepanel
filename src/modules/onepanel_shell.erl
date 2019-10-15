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
-export([execute/1, ensure_success/1, get_success_output/1]).
-export([execute/2, ensure_success/2, get_success_output/2]).
-export([sed/3, mktemp/0]).
-export([quote/1]).

-type token() :: atom() | integer() | string() | binary().
-type exit_code() :: 0..255.
-export_type([token/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv execute(Tokens, Tokens)
%% @end
%%--------------------------------------------------------------------
-spec execute(Tokens :: [token()]) ->
    {exit_code(), StdOut :: binary(), StdERr :: binary()}.
execute(Tokens) ->
    execute(Tokens, Tokens).

%%--------------------------------------------------------------------
%% @doc Evaluates the shell command and returns exit code and result.
%% TokensToLog allows providing the command with sensitive info removed.
%% @end
%%--------------------------------------------------------------------
-spec execute(Tokens :: [token()], TokensToLog :: [token()]) ->
    {exit_code(), StdOut :: binary(), StdERr :: binary()}.
execute(Tokens, TokensToLog) ->
    utils:run_with_tempdir(fun(TempDir) ->
        Out = filename:join(TempDir, "stdout"),
        Err = filename:join(TempDir, "stderr"),

        % wrap in a {} block to handle commands joined with &&, ||, ;
        Wrapper = ["{"] ++ Tokens ++ ["; }", "1>", Out, "2>", Err, "; echo -n $?"],

        Result = os:cmd(tokens_to_cmd(Wrapper)),
        ExitCode = list_to_integer(Result),

        {ok, StdOut} = file:read_file(Out),
        {ok, StdErr} = file:read_file(Err),

        % string:trim raises exception if StdOut is not a unicode string.
        % In such cases it is desirable not to modify the binary output.
        Trimmed = try string:trim(StdOut, trailing)
        catch _:_ -> StdOut end,

        ?debug("~ts", [format_results(TokensToLog, ExitCode, Trimmed, StdErr)]),
        {ExitCode, Trimmed, StdErr}
    end).


%%--------------------------------------------------------------------
%% @doc @equiv ensure_success(Tokens, Tokens)
%% @end
%%--------------------------------------------------------------------
-spec ensure_success(Tokens :: [token()]) -> ok | no_return().
ensure_success(Tokens) ->
    ensure_success(Tokens, Tokens).

%%--------------------------------------------------------------------
%% @doc Evaluates shell command and in case of an exit code different from 0,
%% throws an exception.
%% TokensToLog allows providing the command with sensitive info removed.
%% @end
%%--------------------------------------------------------------------
-spec ensure_success(Tokens :: [token()], TokensToLog :: [token()]) ->
    ok | no_return().
ensure_success(Tokens, TokensToLog) ->
    case execute(Tokens, TokensToLog) of
        {0, _, _} -> ok;
        {Code, StdOut, StdErr} ->
            ?error("~ts", [format_results(TokensToLog, Code, StdOut, StdErr)]),
            ?throw_error(?ERR_CMD_FAILURE(Code, StdOut, StdErr), [TokensToLog])
    end.


%%--------------------------------------------------------------------
%% @doc @equiv get_success_output(Tokens, Tokens)
%% @end
%%--------------------------------------------------------------------
-spec get_success_output(Tokens :: [token()]) -> binary() | no_return().
get_success_output(Tokens) ->
    get_success_output(Tokens, Tokens).

%%--------------------------------------------------------------------
%% @doc Evaluates shell command and returns its output.
%% In case of an exit code different from 0 throws an exception.
%% TokensToLog allows providing the command with sensitive info removed.
%% @end
%%--------------------------------------------------------------------
-spec get_success_output(Tokens :: [token()], TokensToLog :: [token()]) ->
    binary() | no_return().
get_success_output(Tokens, TokensToLog) ->
    case execute(Tokens, TokensToLog) of
        {0, StdOut, _} -> StdOut;
        {Code, StdOut, StdErr} ->
            ?error("~ts", [format_results(TokensToLog, Code, StdOut, StdErr)]),
            ?throw_error(?ERR_CMD_FAILURE(Code, StdOut, StdErr), [TokensToLog])
    end.

%%--------------------------------------------------------------------
%% @doc Wrapper for shell sed program.
%% @end
%%--------------------------------------------------------------------
-spec sed(Pattern :: string(), Replacement :: string(), Path :: file:name_all()) ->
    ok | no_return().
sed(Pattern, Replacement, Path) ->
    onepanel_shell:ensure_success(["sed", "-i", "-e", "'s/" ++ Pattern ++ "/"
        ++ Replacement ++ "/g'", Path]).


%%--------------------------------------------------------------------
%% @doc Creates temporary file.
%% @end
%%--------------------------------------------------------------------
-spec mktemp() -> Path :: string().
mktemp() ->
    lib:nonl(os:cmd("mktemp")).


%%--------------------------------------------------------------------
%% @doc Utility to wrap a token in quote marks and escape
%% already present quote marks.
%% @end
%%--------------------------------------------------------------------
-spec quote(token()) -> binary().
quote(Token) when is_binary(Token) ->
    Escaped = string:replace(Token, <<$'>>, <<"\\'">>, all),
    unicode:characters_to_binary([$', Escaped, $']);

quote(Token) ->
    quote(onepanel_utils:convert(Token, binary)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec tokens_to_cmd(Tokens :: [token()]) -> string().
tokens_to_cmd(Tokens) ->
    unicode:characters_to_list(onepanel_utils:join(Tokens, <<" ">>)).


%%--------------------------------------------------------------------
%% @private
%% @doc Formats execution result for logging purposes.
%% @end
%%--------------------------------------------------------------------
-spec format_results(Tokens :: [token()], exit_code(),
    StdOut:: binary(), StdErr :: binary()) ->
    string().
format_results(Tokens, Code, StdOut, StdErr) ->
    str_utils:format("Command \"~ts\" exited with code ~p. Stdout:~n~ts~nStderr:~n~ts",
        [tokens_to_cmd(Tokens), Code, StdOut, StdErr]).

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

-type token() :: atom() | integer() | string() | binary().
-export_type([token/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv execute(Tokens, Tokens)
%% @end
%%--------------------------------------------------------------------
-spec execute(Tokens :: [token()]) -> {Code :: 0..255, Output :: string()}.
execute(Tokens) ->
    execute(Tokens, Tokens).

%%--------------------------------------------------------------------
%% @doc Evaluates the shell command and returns exit code and result.
%% TokensToLog allows providing the command with sensitive info removed.
%% @end
%%--------------------------------------------------------------------
-spec execute(Tokens :: [token()], TokensToLog :: [token()]) ->
    {Code :: 0..255, Output :: string()}.
execute(Tokens, TokensToLog) ->
    % Wrapper adds exit code to the output
    Wrapper = ["R=`"] ++ Tokens ++ ["2>&1`;", "echo", "-n", "$?,\"$R\";"],
    Result = string:strip(os:cmd(tokens_to_cmd(Wrapper)), right, $\n),

    [CodeStr, Output] = string:split(Result, ",", leading),
    Code = erlang:list_to_integer(CodeStr),

    ?debug("Command \"~ts\" exited with code ~p and output~n\"~ts\"",
        [tokens_to_cmd(TokensToLog), Code, Output]),
    {Code, Output}.


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
        {0, _} -> ok;
        {Code, Output} ->
            ?error("Command \"~ts\" exited with code ~p and output~n\"~ts\"",
                [tokens_to_cmd(TokensToLog), Code, Output]),
            ?throw_error(?ERR_CMD_FAILURE(Code, Output), [TokensToLog])
    end.


%%--------------------------------------------------------------------
%% @doc @equiv get_success_output(Tokens, Tokens)
%% @end
%%--------------------------------------------------------------------
-spec get_success_output(Tokens :: [token()]) -> string() | no_return().
get_success_output(Tokens) ->
    get_success_output(Tokens, Tokens).

%%--------------------------------------------------------------------
%% @doc Evaluates shell command and returns its output.
%% In case of an exit code different from 0 throws an exception.
%% TokensToLog allows providing the command with sensitive info removed.
%% @end
%%--------------------------------------------------------------------
-spec get_success_output(Tokens :: [token()], TokensToLog :: [token()]) ->
    string() | no_return().
get_success_output(Tokens, TokensToLog) ->
    case execute(Tokens) of
        {0, Output} -> Output;
        {Code, Output} ->
            ?error("Command \"~ts\" exited with code ~p and output~n\"~ts\"",
                [tokens_to_cmd(TokensToLog), Code, Output]),
            ?throw_error(?ERR_CMD_FAILURE(Code, Output), [TokensToLog])
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec tokens_to_cmd(Tokens :: [token()]) -> string().
tokens_to_cmd(Tokens) ->
    erlang:binary_to_list(onepanel_utils:join(Tokens, <<" ">>)).

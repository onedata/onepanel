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
-export([sed/3, mktemp/0]).

-type token() :: atom() | integer() | string() | binary().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Evaluates the shell command and returns exit code and result.
%% @end
%%--------------------------------------------------------------------
-spec execute(Tokens :: [token()]) -> {Code :: 0..255, Output :: string()}.
execute(Tokens) ->
    LogFile = onepanel_env:get(cmd_log_file),
    % Wrapper adds exit code to output and tees command output to the cmd log
    Wrapper = ["R=`"] ++ Tokens ++
        ["2>&1`;", "echo", "-n", "$?,;", "echo", "$R", " | tee", "-a", LogFile],
    Result = string:strip(os:cmd(tokens_to_cmd(Wrapper)), right, $\n),

    [CodeStr, Output] = string:split(Result, ",", leading),
    Code = erlang:list_to_integer(CodeStr),
    {Code, Output}.


%%--------------------------------------------------------------------
%% @doc Evaluates shell command and in case of an exit code different from 0,
%% throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec ensure_success(Tokens :: [token()]) -> ok | no_return().
ensure_success(Tokens) ->
    case execute(Tokens) of
        {0, _} -> ok;
        {Code, Output} ->
            ?error("Command \"~ts\" exited with code ~p and output~n\"~ts\"",
                [tokens_to_cmd(Tokens), Code, Output]),
            ?throw_error({Code, Output}, [Tokens])
    end.


%%--------------------------------------------------------------------
%% @doc Evaluates shell command and returns its output.
%% In case of an exit code different from 0 throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec get_success_output(Tokens :: [token()]) -> string() | no_return().
get_success_output(Tokens) ->
    case execute(Tokens) of
        {0, Output} -> Output;
        {Code, Output} ->
            ?error("Command \"~ts\" exited with code ~p and output~n\"~ts\"",
                [tokens_to_cmd(Tokens), Code, Output]),
            ?throw_error({Code, Output}, [Tokens])
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

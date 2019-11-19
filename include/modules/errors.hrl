%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains errors definitions and utility macros.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_ERRORS_HRL).
-define(ONEPANEL_ERRORS_HRL, 1).

-include_lib("ctool/include/errors.hrl").

-record(service_error, {
    service :: service:name(),
    action :: service:action(),
    module :: module(),
    function :: atom(),
    bad_results :: onepanel_rpc:results()
}).


-define(make_error(Reason), ?make_error(Reason, undefined)).

-define(make_error(Reason, Args),
    ?make_error(Reason, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Args)).

-define(make_error(Reason, Module, Function, Arity),
    ?make_error(Reason, Module, Function, Arity, undefined)).

-define(make_error(Reason, Module, Function, Arity, Args),
    case Reason of
        {error, _} -> Reason;
        _ -> {error, Reason}
    end).


-define(make_stacktrace(Reason), ?make_stacktrace(Reason, undefined)).

-define(make_stacktrace(Reason, Args),
    ?make_stacktrace(Reason, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Args)).

-define(make_stacktrace(Reason, Args, Stacktrace),
    ?make_stacktrace(Reason, ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Args, Stacktrace)).

-define(make_stacktrace(Reason, Module, Function, Arity),
    ?make_stacktrace(Reason, Module, Function, Arity, undefined)).

-define(make_stacktrace(Reason, Module, Function, Arity, Args),
    ?make_stacktrace(Reason, Module, Function, Arity, Args, erlang:get_stacktrace())).

-define(make_stacktrace(Reason, Module, Function, Arity, Args, Stacktrace),
    case Reason of
        {error, _} -> Reason;  % @fixme triggers many errors about impossible clauses
        _ -> {error, Reason}
    end).


-define(throw_error(Reason), erlang:throw(?make_error(Reason))).

-define(throw_error(Reason, Args), erlang:throw(?make_error(Reason, Args))).

-define(throw_error(Reason, Module, Function, Arity),
    erlang:throw(?make_error(Reason, Module, Function, Arity))).

-define(throw_error(Reason, Module, Function, Arity, Args),
    erlang:throw(?make_error(Reason, Module, Function, Arity, Args))).


-define(throw_stacktrace(Reason), erlang:throw(?make_stacktrace(Reason))).

-define(throw_stacktrace(Reason, Args), erlang:throw(?make_stacktrace(Reason, Args))).

-define(throw_stacktrace(Reason, Args, Stacktrace),
    erlang:throw(?make_stacktrace(Reason, Args, Stacktrace))).

-define(throw_stacktrace(Reason, Module, Function, Arity),
    erlang:throw(?make_stacktrace(Reason, Module, Function, Arity))).

-define(throw_stacktrace(Reason, Module, Function, Arity, Args),
    erlang:throw(?make_stacktrace(Reason, Module, Function, Arity, Args))).

-define(throw_stacktrace(Reason, Module, Function, Arity, Args, Stacktrace),
    erlang:throw(?make_stacktrace(Reason, Module, Function, Arity, Args, Stacktrace))).




%% Errors for internal onepanel use. Do not have a REST translator
-define(ERR_AMBIGUOUS_HOSTS, {error, ambiguous_hosts}).
-define(ERR_CMD_FAILURE(Code, StdOut, StdOrr), {error, {shell_command_failure, Code, StdOut, StdErr}}).
-define(ERR_DOC_NOT_FOUND, {error, not_found}).
-define(ERR_PARSING_FAILURE(OffendingLine), {parsing_failure, OffendingLine}).
-define(ERR_UNKNOWN_TYPE(Value), {error, {unknown_type, Value}}).


-endif.

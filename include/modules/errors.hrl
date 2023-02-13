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

-record(exception, {
    % 'throw' is usually treated as intentional flow control
    % and not wrapped in this record
    type :: error | exit | throw,
    value :: term(),
    stacktrace = [] :: list()
}).

%% Errors for internal onepanel use. Do not have a REST translator
-define(ERR_AMBIGUOUS_HOSTS, {error, ambiguous_hosts}).
-define(ERR_ALREADY_EXISTS, {error, already_exists}).
-define(ERR_DOC_NOT_FOUND, {error, not_found}).
-define(ERR_PARSING_FAILURE(OffendingLine), {parsing_failure, OffendingLine}).
-define(ERR_UNKNOWN_TYPE(Value), {error, {unknown_type, Value}}).


-endif.

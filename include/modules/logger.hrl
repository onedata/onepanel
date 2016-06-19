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
-author("Krzysztof Trzepla").

-ifndef(ONEPANEL_MODULES_LOGGER_HRL).
-define(ONEPANEL_MODULES_LOGGER_HRL, 1).

-include_lib("ctool/include/logging_backend.hrl").

-ifdef(skip_debug).
-define(log_debug(_Message), ok).
-define(log_debug(_Format, _Args), ok).
-define(log_debug_stacktrace(_Message), ok).
-define(log_debug_stacktrace(_Format, _Args), ok).
-endif.

-ifndef(skip_debug).
-define(log_debug(Message), ?log_debug(Message, [])).
-define(log_debug(Format, Args), ?log_debug(Format, Args, false)).
-define(log_debug(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(debug), Format, Args, Stacktrace)).
-endif.

-define(log_info(Message), ?log_info(Message, [])).
-define(log_info(Format, Args), ?log_info(Format, Args, false)).
-define(log_info(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(info), Format, Args, Stacktrace)).

-define(log_notice(Message), ?log_notice(Message, [])).
-define(log_notice(Format, Args), ?log_notice(Format, Args, false)).
-define(log_notice(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(notice), Format, Args, Stacktrace)).

-define(log_warning(Message), ?log_warning(Message, [])).
-define(log_warning(Format, Args), ?log_warning(Format, Args, false)).
-define(log_warning(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(warning), Format, Args, Stacktrace)).

-define(log_error(Message), ?log_error(Message, [])).
-define(log_error(Format, Args), ?log_error(Format, Args, false)).
-define(log_error(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(error), Format, Args, Stacktrace)).

-define(log_critical(Message), ?log_critical(Message, [])).
-define(log_critical(Format, Args), ?log_critical(Format, Args, false)).
-define(log_critical(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(critical), Format, Args, Stacktrace)).

-define(log_alert(Message), ?log_alert(Message, [])).
-define(log_alert(Format, Args), ?log_alert(Format, Args, false)).
-define(log_alert(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(alert), Format, Args, Stacktrace)).

-define(log_emergency(Message), ?log_emergency(Message, [])).
-define(log_emergency(Format, Args), ?log_emergency(Format, Args, false)).
-define(log_emergency(Format, Args, Stacktrace),
    ?do_log(logger:loglevel_atom_to_int(emergency), Format, Args, Stacktrace)).

-endif.

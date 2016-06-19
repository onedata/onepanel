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

-ifndef(ONEPANEL_ERRORS_HRL).
-define(ONEPANEL_ERRORS_HRL, 1).

-record(error, {
    module :: module(),
    function :: atom(),
    arity :: non_neg_integer(),
    args :: term(),
    reason :: term(),
    stacktrace :: term(),
    line :: non_neg_integer()
}).

-define(error(Reason), begin
    {current_function, {__Module, __Function, __Arity}} =
        erlang:process_info(self(), current_function),
    ?error(Reason, __Module, __Function, __Arity)
end).

-define(error(Reason, Module, Function, Arity),
    ?error(Reason, Module, Function, Arity, undefined)).

-define(error(Reason, Module, Function, Arity, Args),
    onepanel_errors:new(Module, Function, Arity, Args, Reason,
        erlang:get_stacktrace(), ?LINE)
).

-define(throw(Reason), erlang:throw(?error(Reason))).

-define(throw(Reason, Module, Function, Arity),
    ?throw(?error(Reason, Module, Function, Arity))).

-define(throw(Reason, Module, Function, Arity, Args),
    ?throw(?error(Reason, Module, Function, Arity, Args))).


-define(ERR_TIMEOUT, timeout).
-define(ERR_NOT_FOUND, not_found).
-define(ERR_ALREADY_EXISTS, already_exists).
-define(ERR_BAD_NODE, bad_node).
-define(ERR_NIF_NOT_LOADED, nif_not_loaded).

-define(ERR_USERNAME_NOT_AVAILABLE, username_not_available).
-define(ERR_INVALID_USERNAME, invalid_username).
-define(ERR_INVALID_PASSWORD, invalid_password).
-define(ERR_INVALID_ROLE, invalid_role).
-define(ERR_INVALID_USERNAME_OR_PASSWORD, invalid_username_or_password).

-define(ERR_MISSING_REQUIRED_KEY, missing_required_key).
-define(ERR_INVALID_VALUE_TYPE, invalid_value_type).

-endif.

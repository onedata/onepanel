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

-record(error, {
    module :: module(),
    function :: atom(),
    arity :: non_neg_integer(),
    args :: term(),
    reason :: term(),
    stacktrace :: term(),
    line :: non_neg_integer()
}).

-record(service_error, {
    service :: service:name(),
    action :: service:action(),
    module :: module(),
    function :: atom(),
    bad_results :: onepanel_rpc:results()
}).

-define(make_error(Reason), ?make_error(Reason, erlang:get_stacktrace())).

-define(make_error(Reason, Stacktrace), begin
    {current_function, {__Module, __Function, __Arity}} =
        erlang:process_info(self(), current_function),
    ?make_error(Reason, Stacktrace, __Module, __Function, __Arity, undefined)
end).

-define(make_error(Reason, Module, Function, Arity),
    ?make_error(Reason, Module, Function, Arity, undefined)).

-define(make_error(Reason, Module, Function, Arity, Args),
    ?make_error(Reason, erlang:get_stacktrace(), Module, Function, Arity, Args)).

-define(make_error(Reason, Stacktrace, Module, Function, Arity, Args),
    onepanel_errors:create(Module, Function, Arity, Args, Reason, Stacktrace, ?LINE)
).

-define(throw_error(Reason), erlang:throw(?make_error(Reason))).

-define(throw_error(Reason, Stacktrace), erlang:throw(?make_error(Reason, Stacktrace))).

-define(throw_error(Reason, Module, Function, Arity),
    erlang:throw(?make_error(Reason, Module, Function, Arity))).

-define(throw_error(Reason, Module, Function, Arity, Args),
    erlang:throw(?make_error(Reason, Module, Function, Arity, Args))).


-define(ERR_TIMEOUT, timeout).
-define(ERR_NOT_FOUND, not_found).
-define(ERR_ALREADY_EXISTS, already_exists).
-define(ERR_BAD_NODE, bad_node).
-define(ERR_NIF_NOT_LOADED, nif_not_loaded).
-define(ERR_FAILURE_ON_ALL_NODES, failure_on_all_nodes).
-define(ERR_SERVICE_STEP_NOT_FOUND, service_step_not_found).
-define(ERR_HOST_NOT_FOUND, host_not_found).

-define(ERR_USERNAME_NOT_AVAILABLE, username_not_available).
-define(ERR_INVALID_USERNAME, invalid_username).
-define(ERR_INVALID_PASSWORD, invalid_password).
-define(ERR_INVALID_ROLE, invalid_role).
-define(ERR_INVALID_USERNAME_OR_PASSWORD, invalid_username_or_password).

-define(ERR_MISSING_KEY, missing_key).
-define(ERR_MISSING_PARAM, missing_param).
-define(ERR_MISSING_ANY_KEY, missing_any_key).
-define(ERR_INVALID_VALUE, invalid_value).
-define(ERR_HOST_NOT_FOUND_FOR_ALIAS, host_not_found_for_alias).

-define(ERR_STORAGE_TEST_FILE_CREATE, storage_test_file_creation).
-define(ERR_STORAGE_TEST_FILE_READ, storage_test_file_read).
-define(ERR_STORAGE_TEST_FILE_WRITE, storage_test_file_write).
-define(ERR_STORAGE_TEST_FILE_REMOVE, storage_test_file_removal).
-define(ERR_STORAGE_ADDITION, storage_addition).

-define(ERR_UNREGISTERED_PROVIDER, unregistered_provider).

-define(ERR_STORAGE_SYNC, storage_sync).
-define(ERR_STORAGE_SYNC(Reason), {?ERR_STORAGE_SYNC, Reason}).

-define(ERR_STORAGE_SYNC_IMPORT_STARTED, ?ERR_STORAGE_SYNC(import_already_started)).

-endif.

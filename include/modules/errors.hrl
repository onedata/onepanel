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
    args = undefined :: term(),
    reason :: term(),
    stacktrace = [] :: term(),
    line :: non_neg_integer()
}).

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
    onepanel_errors:create(Module, Function, Arity, Args, Reason, [], ?LINE)).

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
    onepanel_errors:create(Module, Function, Arity, Args, Reason, Stacktrace, ?LINE)).

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

-define(ERR_TIMEOUT, timeout).
-define(ERR_NOT_FOUND, not_found).
-define(ERR_ALREADY_EXISTS, already_exists).
-define(ERR_BAD_NODE, bad_node).
-define(ERR_NIF_NOT_LOADED, nif_not_loaded).
-define(ERR_CMD_FAILURE(Code, Output), {shell_command_failure, {Code, Output}}).
-define(ERR_FAILURE_ON_ALL_NODES, failure_on_all_nodes).
-define(ERR_SERVICE_STEP_NOT_FOUND, service_step_not_found).
-define(ERR_HOST_NOT_FOUND, host_not_found).
-define(ERR_NODE_NOT_EMPTY(Host), {node_not_empty, Host}).
-define(ERR_INCOMPATIBLE_NODE(Host, ClusterType), {incompatible_node, Host, ClusterType}).

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

-define(ERR_ONEZONE_NOT_AVAILABLE, onezone_not_available).
-define(ERR_SUBDOMAIN_NOT_AVAILABLE, subdomain_not_available).

-define(ERR_FILE_ACCESS(Path, Reason), {file_access, Path, Reason}).
-define(ERR_LETSENCRYPT(ErrorURN, Message), {letsencrypt, ErrorURN, Message}).
-define(ERR_LETSENCRYPT_LIMIT(ErrorURN, Message), {letsencrypt_limit, ErrorURN, Message}).
-define(ERR_LETSENCRYPT_AUTHORIZATION(Message), {letsencrypt_authorization, Message}).
-define(ERR_LETSENCRYPT_NOT_SUPPORTED, letsencrypt_not_supported).
-define(ERR_SUBDOMAIN_DELEGATION_DISABLED, subdomain_delegation_disabled).

-define(ERR_DNS_CHECK_ERROR(Message), {dns_check_error, Message}).

-define(ERR_STORAGE_TEST_FILE_CREATE, storage_test_file_create).
-define(ERR_STORAGE_TEST_FILE_READ, storage_test_file_read).
-define(ERR_STORAGE_TEST_FILE_REMOVE, storage_test_file_remove).
-define(ERR_STORAGE_ADDITION, storage_addition).
-define(ERR_STORAGE_NOT_FOUND, storage_not_found).
-define(ERR_LUMA_CONFIG(Key), {?ERR_STORAGE_ADDITION, {missing_key, Key}}).

-define(ERR_SPACE_SUPPORT_TOO_LOW(Minimum), {space_support_too_low, Minimum}).

-define(ERR_STORAGE_SYNC, storage_sync).
-define(ERR_STORAGE_SYNC(Reason), {?ERR_STORAGE_SYNC, Reason}).

-define(ERR_STORAGE_SYNC_IMPORT_STARTED, ?ERR_STORAGE_SYNC(import_already_started)).

-define(ERR_CONFIG_AUTO_CLEANING, error_configuring_autocleaning).
-define(ERR_AUTOCLEANING, error_autocleaning).

-endif.

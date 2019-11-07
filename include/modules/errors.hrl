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


-define(ERR_TIMEOUT, timeout).
-define(ERR_NODE_DOWN, {badrpc, nodedown}).
-define(ERR_NOT_FOUND, not_found).
-define(ERR_NOT_SUPPORTED, not_supported).
-define(ERR_ALREADY_EXISTS, already_exists).
-define(ERR_BAD_NODE, bad_node).
-define(ERR_CMD_FAILURE(Code, StdOut, StdOrr), {shell_command_failure, {Code, StdOut, StdErr}}).
-define(ERR_FAILURE_ON_ALL_NODES, failure_on_all_nodes).
-define(ERR_BAD_UPGRADE, bad_upgrade).
-define(ERR_UPGRADE_FROM_FUTURE_ERROR(Model, CurrentVsn, TargetVsn),
    {future_version, Model, CurrentVsn, TargetVsn}).
-define(ERR_SERVICE_STEP_NOT_FOUND, service_step_not_found).
-define(ERR_NO_SERVICE_HOSTS(Service), {no_service_hosts, Service}).
-define(ERR_AMBIGUOUS_HOSTS, ambiguous_hosts).
-define(ERR_HOST_NOT_FOUND(Host), {host_not_found, Host}).
-define(ERR_NODE_NOT_EMPTY(Host), {node_not_empty, Host}).
-define(ERR_INCOMPATIBLE_NODE(Host, ClusterType), {incompatible_node, Host, ClusterType}).
-define(ERR_PARSING_FAILURE(OffendingLine), {parsing_failure, OffendingLine}).

-define(ERR_INVALID_USERNAME, invalid_username).
-define(ERR_INVALID_NEW_PASSPHRASE, invalid_new_passphrase).
-define(ERR_INVALID_CURRENT_PASSPHRASE, invalid_current_passphrase).
-define(ERR_INVALID_AUTH_TOKEN, invalid_auth_token).
-define(ERR_INVALID_PASSPHRASE, invalid_passphrase).
-define(ERR_UNAUTHORIZED, unauthorized).
-define(ERR_USER_NOT_IN_CLUSTER, user_not_in_cluster).

-define(ERR_MISSING_KEY, missing_key).
-define(ERR_MISSING_PARAM, missing_param).
-define(ERR_MISSING_ANY_KEY, missing_any_key).
-define(ERR_INVALID_VALUE, invalid_value).
-define(ERR_INVALID_VALUE_TOKEN, invalid_value_token).
-define(ERR_UNKNOWN_TYPE(Value), {unknown_type, Value}).
-define(ERR_HOST_NOT_FOUND_FOR_ALIAS, host_not_found_for_alias).

-define(ERR_NOT_REGISTERED, not_registered).
-define(ERR_ONEZONE_NOT_AVAILABLE, onezone_not_available).
-define(ERR_SUBDOMAIN_NOT_AVAILABLE, subdomain_not_available).

-define(ERR_FILE_ACCESS(Path, Reason), {file_access, Path, Reason}).
-define(ERR_LETSENCRYPT(ErrorURN, Message), {letsencrypt, ErrorURN, Message}).
-define(ERR_LETSENCRYPT_LIMIT(ErrorURN, Message), {letsencrypt_limit, ErrorURN, Message}).
-define(ERR_LETSENCRYPT_AUTHORIZATION(Message), {letsencrypt_authorization, Message}).
-define(ERR_LETSENCRYPT_NOT_SUPPORTED, letsencrypt_not_supported).

-define(ERR_CEPH_TOO_FEW_OSDS(RequestedCopies, OSDs),
    {ceph_too_few_osds, RequestedCopies, OSDs}).
-define(ERR_FILE_ALLOCATION_FAILURE(ActualSize, TargetSize),
    {error_file_allocation_failure, ActualSize, TargetSize}).
-define(ERR_AMBIGUOUS_UUID, error_ambiguous_uuid).


-define(ERR_DNS_CHECK_ERROR(Message), {dns_check_error, Message}).

-define(ERR_STORAGE_TEST_FILE_CREATE, storage_test_file_create).
-define(ERR_STORAGE_TEST_FILE_READ, storage_test_file_read).
-define(ERR_STORAGE_TEST_FILE_REMOVE, storage_test_file_remove).
-define(ERR_STORAGE_ADDITION, storage_addition).
-define(ERR_STORAGE_UPDATE_MISMATCH, storage_update_mismatch).
-define(ERR_STORAGE_IN_USE, storage_in_use).
-define(ERR_STORAGE_NOT_FOUND, storage_not_found).
-define(ERR_LUMA_CONFIG(Key), {?ERR_STORAGE_ADDITION, {missing_key, Key}}).

-define(ERR_SPACE_SUPPORT_TOO_LOW(Minimum), {space_support_too_low, Minimum}).

-define(ERR_STORAGE_SYNC, storage_sync).
-define(ERR_STORAGE_SYNC(Reason), {?ERR_STORAGE_SYNC, Reason}).

-define(ERR_STORAGE_SYNC_IMPORT_STARTED, ?ERR_STORAGE_SYNC(import_already_started)).

-define(ERR_CONFIG_AUTO_CLEANING, error_configuring_autocleaning).
-define(ERR_AUTOCLEANING, error_autocleaning).

-define(ERR_CONFIG_FILE_POPULARITY, error_config_file_popularity).
-define(ERR_FILE_POPULARITY, error_file_popularity).

-endif.

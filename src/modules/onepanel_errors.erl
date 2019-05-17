%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains error handling functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_errors).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("validation.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/7, translate/2, format_error/1]).

-define(FORMAT_NONEMPTY(Variable), case Variable of
    undefined -> "";
    [] -> "";
    Value -> io_lib:format("~s: ~tp~n", [??Variable, Variable])
end).


%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates an error record.
%% @end
%%--------------------------------------------------------------------
-spec create(Module :: module(), Function :: atom(), Arity :: arity(),
    Args :: term(), Reason :: term(), Stacktrace :: term(), Line :: non_neg_integer()) ->
    #error{}.
create(Module, Function, Arity, Args, {error, #error{} = Reason}, Stacktrace, Line) ->
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);
create(Module, Function, Arity, Args, {badmatch, #error{} = Reason}, Stacktrace, Line) ->
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);
create(Module, Function, Arity, Args, {case_clause, #error{} = Reason}, Stacktrace, Line) ->
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);
create(Module, Function, Arity, Args, {try_clause, #error{} = Reason}, Stacktrace, Line) ->
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);

create(_, _, _, _, #error{stacktrace = []} = Reason, NewStacktrace, _) ->
    Reason#error{stacktrace = NewStacktrace};
create(_, _, _, _, #error{} = Reason, _, _) ->
    Reason;

create(Module, Function, Arity, Args, Reason, Stacktrace, Line) ->
    #error{module = Module, function = Function, arity = Arity, args = Args,
        reason = Reason, stacktrace = Stacktrace, line = Line}.


%%--------------------------------------------------------------------
%% @doc Converts error to a human-readable description.
%% @end
%%--------------------------------------------------------------------
-spec translate(Type :: atom(), Reason :: term()) ->
    {Name :: binary(), Description :: binary()} | no_return().
translate(_Type, #error{reason = {?ERR_MISSING_KEY, Keys}}) ->
    Key = get_key(Keys),
    {<<"Invalid Request">>, <<"Missing required key: '", Key/binary, "'.">>};

translate(_Type, #error{reason = {?ERR_MISSING_ANY_KEY, Keys}}) ->
    Key = get_key(Keys, <<", ">>),
    {<<"Invalid Request">>, <<"Missing one of required keys: {", Key/binary, "}.">>};

translate(_Type, #error{reason = {?ERR_INVALID_VALUE, Keys, ValueSpec}}) ->
    Key = get_key(Keys),
    Expectation = get_expectation(ValueSpec, <<>>),
    {<<"Invalid Request">>, <<"Invalid '", Key/binary, "' value, expected: ",
        Expectation/binary, ".">>};

translate(_Type, #error{reason = ?ERR_INVALID_VALUE_TOKEN}) ->
    {<<"Invalid Request">>, <<"Provided token is not valid.">>};

translate(_Type, #error{reason = {?ERR_MISSING_PARAM, Keys}}) ->
    Key = get_key(Keys),
    {<<"Invalid Request">>, <<"Missing required parameter: '", Key/binary, "'.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME}) ->
    {<<"Invalid Request">>, <<"Username must be 2-15 characters long and composed ",
        "of letters and digits. Dashes and underscores are allowed ",
        "(but not at the beginning or the end).">>};

translate(_Type, #error{reason = ?ERR_INVALID_PASSWORD}) ->
    {<<"Invalid Request">>, <<"Password must be at least ",
        (integer_to_binary(?PASSWORD_MIN_LENGTH))/binary,
        " characters long and must not contain a colon character ':'">>};

translate(_Type, #error{reason = ?ERR_INVALID_ROLE}) ->
    {<<"Invalid Request">>, <<"User role must be one of 'admin' or 'regular'.">>};

translate(_Type, #error{reason = ?ERR_USERNAME_NOT_AVAILABLE}) ->
    {<<"Operation Error">>, <<"Username is not available.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD}) ->
    {<<"Authentication Error">>, <<"Invalid username or password.">>};

translate(_Type, #error{reason = ?ERR_INVALID_AUTH_TOKEN}) ->
    {<<"Authentication Error">>, <<"Invalid REST API token.">>};

translate(_Type, #error{reason = ?ERR_INVALID_PASSPHRASE}) ->
    {<<"Authentication Error">>, <<"Invalid emergency passphrase.">>};

translate(_Type, #error{reason = ?ERR_USER_NOT_IN_CLUSTER}) ->
    {<<"Authentication Error">>, <<"User is not authorized to access this cluster.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [onepanel_session, _]}) ->
    {<<"Authentication Error">>, <<"Session not found or expired.">>};

translate(_Type, #error{reason = ?ERR_BAD_NODE}) ->
    {<<"Invalid Request">>, <<"Node connection error.">>};

translate(_Type, #error{reason = {?ERR_HOST_NOT_FOUND, Host}}) ->
    {<<"Invalid Request">>, <<"Host not found: '",
        (onepanel_utils:convert(Host, binary))/binary, "'.">>};

translate(_Type, #error{reason = ?ERR_NO_SERVICE_HOSTS(Service)}) ->
    {<<"Invalid Request">>, str_utils:format_bin(
        "Requires service '~s' which is not deployed on any node.", [Service])};

translate(_Type, #error{reason = ?ERR_NODE_NOT_EMPTY(Host)}) ->
    {<<"Invalid Request">>, str_utils:format_bin(
        "Host at '~s' is already a part of an existing cluster.", [Host])};

translate(_Type, #error{reason = ?ERR_INCOMPATIBLE_NODE(Host, ClusterType)}) ->
    {<<"Operation error">>, str_utils:format_bin(
        "Cannot add node ~s to the cluster. It is a ~s node, expected ~s.",
        [Host, ClusterType, onepanel_env:get_cluster_type()])};

translate(_Type, #error{reason = {?ERR_HOST_NOT_FOUND_FOR_ALIAS, Alias}}) ->
    {<<"Invalid Request">>, <<"Host not found for node: '",
        (onepanel_utils:convert(Alias, binary))/binary, "'.">>};

translate(_Type, #error{reason = ?ERR_ONEZONE_NOT_AVAILABLE}) ->
    {<<"Onezone connection error">>, <<"Onezone not available">>};

translate(_Type, #error{reason = ?ERR_SUBDOMAIN_NOT_AVAILABLE}) ->
    % DO NOT modify this error name as it is used to identify the error in GUI
    {<<"Subdomain reserved error">>, <<"Requested subdomain is currently used "
        "by another provider. Please choose another one. The subdomain will be "
        "available again when the conflicting provider is unregistered or "
        "configured to use a different subdomain. Please contact your Onezone "
        "administror if you want to reclaim the subdomain of an unused provider.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_ADDITION, aleady_exists}}) ->
    {<<"Operation Error">>, <<"Storage name is not available.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_ADDITION, Reason}}) ->
    ?error("Cannot add storage due to: ~p", [Reason]),
    {<<"Operation Error">>, <<"Storage addition error.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_NOT_FOUND, StorageId}}) ->
    {<<"Operation Error">>, <<"Storage '", StorageId/binary, "' not found.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_TEST_FILE_CREATE, Node, Reason}}) ->
    translate_storage_test_file_error("create", <<"creation">>, Node, Reason);

translate(_Type, #error{reason = {?ERR_STORAGE_TEST_FILE_READ, Node, Reason}}) ->
    translate_storage_test_file_error("read", <<"read">>, Node, Reason);

translate(_Type, #error{reason = {?ERR_STORAGE_TEST_FILE_REMOVE, Node, Reason}}) ->
    translate_storage_test_file_error("remove", <<"removal">>, Node, Reason);

translate(_Type, #error{reason = {no_exists, onepanel_user}}) ->
    {<<"Invalid Request">>, <<"Onepanel cluster not configured.">>};

translate(_Type, #error{reason = invalid_json}) ->
    {<<"Operation Error">>, <<"Malformed JSON data.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [service, couchbase]}) ->
    {<<"Operation Error">>, <<"Database not configured.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [service, cluster_manager]}) ->
    {<<"Operation Error">>, <<"Cluster Manager not configured.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [service, op_worker]}) ->
    {<<"Operation Error">>, <<"Cluster Worker not configured.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [service, oz_worker]}) ->
    {<<"Operation Error">>, <<"Cluster Worker not configured.">>};

translate(_Type, #error{reason = ?ERR_SUBDOMAIN_DELEGATION_DISABLED}) ->
    {<<"Operation Error">>, <<"Subdomain delegation is not enabled.">>};

translate(_Type, #error{reason = ?ERR_DNS_CHECK_ERROR(Message)}) ->
    {<<"Operation Error">>, str_utils:format_bin("Error performing DNS check: ~ts", [Message])};

translate(_Type, #error{reason = ?ERR_FILE_ACCESS(Path, Reason)}) ->
    {<<"File access error">>, str_utils:format_bin("Error opening file ~p: ~p", [Path, Reason])};

% DO NOT modify this error message as it is used to identify the error in GUI
translate(_Type, #error{reason = ?ERR_LETSENCRYPT(ErrorURN, Message)}) ->
    {<<"Let's Encrypt Error">>, str_utils:format_bin("Let's Encrypt error: ~s: ~s",
        [ErrorURN, Message])};

% DO NOT modify this error message as it is used to identify the error in GUI
translate(_Type, #error{reason = ?ERR_LETSENCRYPT_LIMIT(ErrorURN, Message)}) ->
    {<<"Let's Encrypt Limit Error">>,
    str_utils:format_bin("Let's Encrypt limit error: ~s: ~s", [ErrorURN, Message])};

% DO NOT modify this error message as it is used to identify the error in GUI
translate(_Type, #error{reason = ?ERR_LETSENCRYPT_AUTHORIZATION(Message)}) ->
    {<<"Let's Encrypt Authorization Error">>,
        str_utils:format_bin("Let's Encrypt authroization error: ~s", [Message])};

translate(_Type, #error{reason = ?ERR_LETSENCRYPT_NOT_SUPPORTED}) ->
    {<<"Let's Encrypt Not Supported Error">>,
        <<"Automated Let's Encrypt certificates retrieval is currently unavailable.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_SYNC, import_already_started}}) ->
    {<<"Operation Error">>, <<"Modifying storage_import that has already been started">>};

translate(_Type, #error{reason = {?ERR_STORAGE_ADDITION, {missing_key, MissingKey}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("LUMA configuration error. "
    "Missing key: ~p", [MissingKey])};

translate(_Type, #error{reason = ?ERR_SPACE_SUPPORT_TOO_LOW(Minimum)}) ->
    {<<"Operation Error">>, str_utils:format_bin(
        "Space support size must exceed currently used storage space and the minimum imposed by "
        "Onezone. Please specify size of at least ~B bytes.", [Minimum])};

translate(_Type, #error{reason = {?ERR_CONFIG_AUTO_CLEANING, file_popularity_disabled}}) ->
    {<<"Operation Error">>, <<"File popularity statistics must be turned on to enable autocleaning">>};

translate(_Type, #error{reason = {?ERR_CONFIG_AUTO_CLEANING, {negative_value, Parameter}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("Auto-cleaning configuration error. Negative value not allowed for key: ~p.", [Parameter])};

translate(_Type, #error{reason = {?ERR_CONFIG_AUTO_CLEANING, {illegal_type, Parameter}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("Auto-cleaning configuration error. Illegal type for key: ~p.", [Parameter])};

translate(_Type, #error{reason = {?ERR_CONFIG_AUTO_CLEANING, {value_grater_than, Name1, Name2}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("Auto-cleaning configuration error. Setting value of parameter ~p greater than ~p is forbidden", [Name1, Name2])};

translate(_Type, #error{reason = {?ERR_AUTOCLEANING, file_popularity_disabled}}) ->
    {<<"Operation Error">>, <<"Auto-cleaning error. File popularity is disabled.">>};

translate(_Type, #error{reason = {?ERR_AUTOCLEANING, autocleaning_disabled}}) ->
    {<<"Operation Error">>, <<"Auto-cleaning error. Auto-cleaning is disabled.">>};

translate(_Type, #error{reason = {?ERR_AUTOCLEANING, Reason}}) ->
    {<<"Operation Error">>, str_utils:format_bin("Auto-cleaning unexpected error: ~p ", [Reason])};

translate(_Type, #error{reason = {?ERR_CONFIG_FILE_POPULARITY, {negative_value, Parameter}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("file-popularity configuration error. Negative value not allowed for key: ~p.", [Parameter])};

translate(_Type, #error{reason = {?ERR_CONFIG_FILE_POPULARITY, {illegal_type, Parameter}}}) ->
    {<<"Operation Error">>, str_utils:format_bin("file-popularity configuration error. Illegal type for key: ~p.", [Parameter])};

translate(_Type, #error{reason = {?ERR_FILE_POPULARITY, Error}}) ->
    {<<"Operation Error">>, str_utils:format_bin("file-popularity error: ~p.", [Error])};

translate(_Type, #error{reason = ?ERR_CMD_FAILURE(_, _)}) ->
    {<<"Internal Error">>, <<"Server encountered an unexpected error.">>};

translate(_Type, #error{reason = {error, {Code, Error, Description}}})
    when is_integer(Code), is_binary(Error), is_binary(Description) ->
    {<<"Operation Error">>, Error};

translate(_Type, #error{} = Error) ->
    ?error("~s", [format_error(Error)]),
    {<<"Internal Error">>, <<"Server encountered an unexpected error.">>};

translate(Type, Reason) ->
    ?error("Type: ~p~nReason: ~p~n", [Type, Reason]),
    erlang:raise(Type, Reason, []).


-spec format_error(#error{}) -> list().
format_error(#error{module = Module, function = Function, arity = Arity,
    args = Args, reason = Reason, stacktrace = Stacktrace, line = Line}) ->
    io_lib:format("Function: ~p:~p/~p~n~tsReason: ~tp~n~ts"
    "Line: ~p~n", [Module, Function, Arity, ?FORMAT_NONEMPTY(Args),
        Reason, ?FORMAT_NONEMPTY(Stacktrace), Line]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Merge a list of keys with a "." character into human-readable
%% format.
%% @end
%%--------------------------------------------------------------------
-spec get_key(Keys :: onepanel_parser:keys()) -> Key :: binary().
get_key(Keys) ->
    get_key(Keys, <<".">>).


%%--------------------------------------------------------------------
%% @private @doc Merge a list of keys with a given separator character into
%% human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec get_key(Keys :: onepanel_parser:keys(), Separator :: binary()) -> Key :: binary().
get_key(Keys, Separator) ->
    onepanel_utils:join(Keys, Separator).


%%--------------------------------------------------------------------
%% @private @doc Returns human-readable expectation for a value specification.
%% @end
%%--------------------------------------------------------------------
-spec get_expectation(ValueSpec :: onepanel_parser:value_spec(), Acc :: binary()) ->
    Expectation :: binary().
get_expectation({ValueSpec, optional}, Acc) ->
    <<(get_expectation(ValueSpec, Acc))/binary, " (optional)">>;

get_expectation({ValueSpec, {optional, Default}}, Acc) ->
    <<(get_expectation(ValueSpec, Acc))/binary, " (optional, default: ",
        (onepanel_utils:convert(Default, binary))/binary, ")">>;

get_expectation({ValueSpec, required}, Acc) ->
    get_expectation(ValueSpec, Acc);

get_expectation(atom, Acc) ->
    <<Acc/binary, "'<string>'">>;

get_expectation(ValueSpec, Acc) when is_atom(ValueSpec) ->
    <<Acc/binary, "'<", (onepanel_utils:convert(ValueSpec, binary))/binary, ">'">>;

get_expectation({equal, Value}, Acc) ->
    <<Acc/binary, "'", (onepanel_utils:convert(Value, binary))/binary, "'">>;

get_expectation({oneof, ValueSpecs}, Acc) ->
    Tokens = lists:map(fun(ValueSpec) ->
        get_expectation(ValueSpec, <<>>)
    end, ValueSpecs),
    <<Acc/binary, "[", (onepanel_utils:join(Tokens, <<" | ">>))/binary, "]">>;

get_expectation(ValueSpecs, Acc) when is_list(ValueSpecs) ->
    Tokens = lists:map(fun(ValueSpec) ->
        get_expectation(ValueSpec, <<>>)
    end, ValueSpecs),
    <<Acc/binary, "[", (onepanel_utils:join(Tokens, <<", ">>))/binary, "]">>;

get_expectation(ValueSpec, Acc) when is_map(ValueSpec) ->
    Tokens = lists:map(fun({Key, Value}) ->
        BinKey = onepanel_utils:convert(Key, binary),
        <<"'", (BinKey)/binary, "': ", (get_expectation(Value, <<>>))/binary>>
    end, maps:to_list(ValueSpec)),
    <<Acc/binary, "{", (onepanel_utils:join(Tokens, <<", ">>))/binary, "}">>.


%%--------------------------------------------------------------------
%% @private @doc Translates storage test file error into human-readable format.
%% @end
%%--------------------------------------------------------------------
-spec translate_storage_test_file_error(OperVerb :: string(), OperNoun :: binary(),
    Node :: node(), Reason :: term()) -> {Name :: binary(), Description :: binary()}.
translate_storage_test_file_error(OperVerb, OperNoun, Node, Reason) ->
    Host = hosts:from_node(Node),
    ?error("Cannot " ++ OperVerb ++ " storage test file on node ~p due to: ~p",
        [Node, Reason]),
    {<<"Operation Error">>, <<"Storage test file ", OperNoun/binary, " failed "
    "on host: '", (onepanel_utils:convert(Host, binary))/binary, "'.">>}.

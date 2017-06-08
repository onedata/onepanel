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
-include_lib("ctool/include/logging.hrl").

%% API
-export([create/7, translate/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates an error record.
%% @end
%%--------------------------------------------------------------------
-spec create(Module :: module(), Function :: atom(), Arity :: non_neg_integer(),
    Args :: term(), Reason :: term(), Stacktrace :: term(), Line :: non_neg_integer()) ->
    #error{}.
create(Module, Function, Arity, Args, {badmatch, Reason}, Stacktrace, Line) ->
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);

create(_Module, _Function, _Arity, _Args, #error{} =
    _Reason, _Stacktrace, _Line) ->
    #error{module = Module, function = Function, arity = Arity, args = Args,
        reason = Reason, stacktrace = Stacktrace, line = Line} = _Reason,
    create(Module, Function, Arity, Args, Reason, Stacktrace, Line);

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

translate(_Type, #error{reason = {?ERR_MISSING_PARAM, Keys}}) ->
    Key = get_key(Keys),
    {<<"Invalid Request">>, <<"Missing required parameter: '", Key/binary, "'.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME}) ->
    {<<"Invalid Request">>, <<"Username must not be empty and must not contain "
    "a colon character ':'">>};

translate(_Type, #error{reason = ?ERR_INVALID_PASSWORD}) ->
    {<<"Invalid Request">>, <<"Password must not be empty.">>};

translate(_Type, #error{reason = ?ERR_INVALID_ROLE}) ->
    {<<"Invalid Request">>, <<"User role must be one of 'admin' or 'regular'.">>};

translate(_Type, #error{reason = ?ERR_USERNAME_NOT_AVAILABLE}) ->
    {<<"Operation Error">>, <<"Username is not available.">>};

translate(_Type, #error{reason = ?ERR_INVALID_USERNAME_OR_PASSWORD}) ->
    {<<"Authentication Error">>, <<"Invalid username or password.">>};

translate(_Type, #error{module = model, function = get, reason = ?ERR_NOT_FOUND,
    args = [onepanel_session, _]}) ->
    {<<"Authentication Error">>, <<"Session not found or expired.">>};

translate(_Type, #error{reason = ?ERR_BAD_NODE}) ->
    {<<"Invalid Request">>, <<"Node connection error.">>};

translate(_Type, #error{reason = {?ERR_HOST_NOT_FOUND, Host}}) ->
    {<<"Invalid Request">>, <<"Host not found: '",
        (onepanel_utils:convert(Host, binary))/binary, "'.">>};

translate(_Type, #error{reason = {?ERR_HOST_NOT_FOUND_FOR_ALIAS, Alias}}) ->
    {<<"Invalid Request">>, <<"Host not found for node: '",
        (onepanel_utils:convert(Alias, binary))/binary, "'.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_ADDITION, aleady_exists}}) ->
    {<<"Operation Error">>, <<"Storage name is not available.">>};

translate(_Type, #error{reason = {?ERR_STORAGE_ADDITION, Reason}}) ->
    ?error("Cannot add storage due to: ~p", [Reason]),
    {<<"Operation Error">>, <<"Storage addition error.">>};

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

translate(_Type, #error{reason = ?ERR_UNREGISTERED_PROVIDER}) ->
    {<<"Operation Error">>, <<"Unregistered provider.">>};

translate(_Type, #error{reason = {error, {Code, Error, Description}}})
    when is_integer(Code), is_binary(Error), is_binary(Description) ->
    {<<"Operation Error">>, Error};

translate(_Type, #error{module = Module, function = Function, arity = Arity,
    args = Args, reason = Reason, stacktrace = Stacktrace, line = Line}) ->
    ?error("Function: ~p:~p/~p~nArgs: ~p~nReason: ~p~nStacktrace: ~p~n"
    "Line: ~p~n", [Module, Function, Arity, Args, Reason, Stacktrace, Line]),
    {<<"Internal Error">>, <<"Server encountered an unexpected error.">>};

translate(Type, Reason) ->
    ?error("Type: ~p~nReason: ~p~n", [Type, Reason]),
    erlang:raise(Type, Reason, []).

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
    Host = onepanel_cluster:node_to_host(Node),
    ?error("Cannot " ++ OperVerb ++ " storage test file on node ~p due to: ~p",
        [Node, Reason]),
    {<<"Operation Error">>, <<"Storage test file ", OperNoun/binary, " failed "
    "on host: '", (onepanel_utils:convert(Host, binary))/binary, "'.">>}.

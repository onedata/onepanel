%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_utils).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include_lib("ctool/include/http/headers.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_basic_auth_header/2]).
-export([wait_until/5, wait_until/6]).
-export([gen_uuid/0, join/1, join/2, trim/2]).
-export([convert/2, get_type/1, convert_recursive/2]).
-export([get_converted/3, get_converted/4, find_converted/3]).
-export([ensure_known_hosts/1, distribute_file/2]).

% @formatter:off
-type primitive_type() :: atom | binary | float | integer | list | boolean.
-type collection_modifier() :: seq | keys | values | map.
-type type() :: primitive_type() | {collection_modifier(), type()}.
-type expectation(Result) :: {equal, Result} |
    {validator, fun((term()) -> Result | no_return())}.
-type uuid() :: binary().
% @formatter:on

-export_type([type/0, uuid/0]).

-define(UUID_LEN, 32).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Constructs basic authentication header.
%% @end
%%--------------------------------------------------------------------
-spec get_basic_auth_header(Username :: string() | binary(),
    Password :: string() | binary()) -> http_client:headers().
get_basic_auth_header(Username, Password) ->
    Hash = base64:encode(<<
        (onepanel_utils:convert(Username, binary))/binary, ":",
        (onepanel_utils:convert(Password, binary))/binary>>
    ),
    #{?HDR_AUTHORIZATION => <<"Basic ", Hash/binary>>}.


%%--------------------------------------------------------------------
%% @doc @equiv wait_until(Module, Function, Args, Expectation, Attempts,
%% timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_until(module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(Result), Attempts :: integer()) ->
    Result | no_return().
wait_until(Module, Function, Args, Expectation, Attempts) ->
    wait_until(Module, Function, Args, Expectation, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Waits until evaluation of ```apply(Module, Function, Args)''' returns
%% expected result, defined by validation function or exact value.
%% @end
%%--------------------------------------------------------------------
-spec wait_until(module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(Result), Attempts :: integer(), Delay :: integer()) ->
    Result | no_return().
wait_until(_Module, _Function, _Args, _Expectation, Attempts, _Delay) when
    Attempts =< 0 ->
    throw(attempts_limit_exceeded);

wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay) ->
    wait_until(Module, Function, Args,
        {validator, fun(Result) -> Expected = Result end}, Attempts, Delay);

wait_until(Module, Function, Args, {validator, Validator}, Attempts, Delay) ->
    try
        Result = erlang:apply(Module, Function, Args),
        Validator(Result)
    catch
        _:Reason ->
            timer:sleep(Delay),
            ?debug_stacktrace("Call ~p:~p(~p) returned unexpected result: ~p."
            " Retrying...", [Module, Function, Args, Reason]),
            wait_until(Module, Function, Args, {validator, Validator},
                Attempts - 1, Delay)
    end;

wait_until(Module, Function, Args, Expected, Attempts, Delay) ->
    wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Generates random UUID.
%% @end
%%--------------------------------------------------------------------
-spec gen_uuid() -> uuid().
gen_uuid() ->
    http_utils:base64url_encode(crypto:strong_rand_bytes(?UUID_LEN)).


%%--------------------------------------------------------------------
%% @doc @equiv join(Tokens, ```<<>>''')
%% @end
%%--------------------------------------------------------------------
-spec join(Tokens :: [term()]) -> Binary :: binary().
join(Tokens) ->
    join(Tokens, <<>>).


%%--------------------------------------------------------------------
%% @doc Joins sequence of tokens using provided separator.
%% @end
%%--------------------------------------------------------------------
-spec join(Tokens :: [term()], Sep :: binary()) -> Binary :: binary().
join([], _Sep) ->
    <<>>;

join(Tokens, Sep) ->
    [_ | NewTokens] = lists:foldl(fun(Token, Acc) ->
        [Sep, convert(Token, binary) | Acc]
    end, [], Tokens),
    lists:foldl(fun(Token, Acc) ->
        <<Token/binary, Acc/binary>>
    end, <<>>, NewTokens).


%%--------------------------------------------------------------------
%% @doc Removes whitespace characters from a given side or both sides of
%% a character sequence.
%% @end
%%--------------------------------------------------------------------
-spec trim(Text :: binary(), Side :: left | right | both) -> NewText :: binary().
trim(Text, left) ->
    re:replace(Text, <<"^\\s+">>, <<>>, [{return, binary}]);
trim(Text, right) ->
    re:replace(Text, <<"\\s+$">>, <<>>, [{return, binary}]);
trim(Text, both) ->
    trim(trim(Text, left), right).


%%--------------------------------------------------------------------
%% @doc Converts value to a provided type.
%% @end
%%--------------------------------------------------------------------
-spec convert
    ([Old :: term()], {seq, type()}) -> [Converted :: term()];
    (#{Old :: term() => V}, {keys, type()}) -> #{Converted :: term() => V};
    (#{K => Old :: term()}, {values, type()}) -> #{K => Converted :: term()};
    (#{OldKey :: term() => OldValue :: term()}, {map, type()}) ->
        #{ConvertedKey :: term() => ConvertedValue :: term()};
    (Old :: term(), atom) -> Converted :: atom();
    (Old :: term(), boolean) -> Converted :: boolean();
    (Old :: term(), float) -> Converted :: float();
    (Old :: term(), integer) -> Converted :: integer();
    (Old :: term(), list) -> Converted :: string();
    (Old :: term(), binary) -> Converted :: binary().
convert(Values, {seq, Type}) ->
    lists:map(fun(Value) -> convert(Value, Type) end, Values);

% convert keys in a map
convert(Map, {keys, Type}) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{convert(Key, Type) => Value}
    end, #{}, Map);

% convert values in a map
convert(Map, {values, Type}) ->
    maps:map(fun(_Key, Value) ->
        convert(Value, Type)
    end, Map);

% convert keys and values in a map
convert(Map, {map, Type}) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{convert(Key, Type) => convert(Value, Type)}
    end, #{}, Map);

convert(Value, boolean) ->
    case convert(Value, atom) of
        true -> true;
        false -> false
    end;

convert(Value, binary) when is_atom(Value) ->
    erlang:atom_to_binary(Value, utf8);

convert(Value, atom) when is_binary(Value) ->
    erlang:binary_to_atom(Value, utf8);

convert(Value, float) when is_integer(Value) ->
    Value * 1.0;

convert(Value, binary) when is_list(Value) ->
    unicode:characters_to_binary(Value);

convert(Value, Type) ->
    case get_type(Value) of
        Type ->
            Value;
        ValueType ->
            TypeConverter = list_to_atom(
                atom_to_list(ValueType) ++ "_to_" ++ atom_to_list(Type)
            ),
            erlang:TypeConverter(Value)
    end.

%%--------------------------------------------------------------------
%% @doc Converts value to a provided type.
%% @end
%%--------------------------------------------------------------------
-spec convert_recursive(Old :: term(), type()) -> Converted :: term().
convert_recursive(Values, TypeSpec = {seq, _}) when is_list(Values) ->
    lists:map(fun(Value) -> convert_recursive(Value, TypeSpec) end, Values);

convert_recursive(Map, TypeSpec = {keys, Type}) when is_map(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{convert(Key, Type) => convert_recursive(Value, TypeSpec)}
    end, #{}, Map);

convert_recursive(Map, TypeSpec = {values, _}) when is_map(Map) ->
    maps:map(fun(_Key, Value) ->
        convert_recursive(Value, TypeSpec)
    end, Map);

convert_recursive(Map, TypeSpec = {map, Type}) when is_map(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc#{convert(Key, Type) => convert_recursive(Value, TypeSpec)}
    end, #{}, Map);

convert_recursive(Term, {_, Type}) ->
    convert(Term, Type);

convert_recursive(Term, Type) ->
    convert(Term, Type).


%%--------------------------------------------------------------------
%% @doc Returns type of a given value.
%% @end
%%--------------------------------------------------------------------
-spec get_type
    (Value :: atom()) -> atom;
    (Value :: binary()) -> binary;
    (Value :: float()) -> float;
    (Value :: integer()) -> integer;
    (Value :: list()) -> list.
get_type(Atom) when is_atom(Atom) -> atom;
get_type(Binary) when is_binary(Binary) -> binary;
get_type(Float) when is_float(Float) -> float;
get_type(Integer) when is_integer(Integer) -> integer;
get_type(List) when is_list(List) -> list;
get_type(Value) ->
    ?error("Could not determine type of ~tp", [Value]),
    error(?ERR_UNKNOWN_TYPE(Value)).


%%--------------------------------------------------------------------
%% @doc Performs {@link kv_utils:get/2} and converts obtained result to given type.
%% @end
%%--------------------------------------------------------------------
-spec get_converted(kv_utils:path(K), kv_utils:nested(K, _), type()) -> term().
get_converted(Path, Nested, Type) ->
    onepanel_utils:convert(kv_utils:get(Path, Nested), Type).


%%--------------------------------------------------------------------
%% @doc Performs {@link kv_utils:get/3} and converts obtained result to given type.
%% @end
%%--------------------------------------------------------------------
-spec get_converted(kv_utils:path(K), kv_utils:nested(K, _), type(), Default) ->
    term() | Default.
get_converted(Path, Nested, Type, Default) ->
    case kv_utils:find(Path, Nested) of
        {ok, Found} -> onepanel_utils:convert(Found, Type);
        error -> Default
    end.



%%--------------------------------------------------------------------
%% @doc Performs {@link kv_utils:find/3} and converts obtained result to given type.
%% @end
%%--------------------------------------------------------------------
-spec find_converted(kv_utils:path(K), kv_utils:nested(K, _), type()) ->
    {ok, term()} | error.
find_converted(Path, Nested, Type) ->
    case kv_utils:find(Path, Nested) of
        {ok, Found} -> {ok, onepanel_utils:convert(Found, Type)};
        error -> error
    end.


%%--------------------------------------------------------------------
%% @doc Ensures all given hosts are part of the cluster.
%% Throws otherwise.
%% @end
%%--------------------------------------------------------------------
-spec ensure_known_hosts(Hosts :: [service:host()]) -> ok | no_return().
ensure_known_hosts(Hosts) ->
    KnownHosts = service_onepanel:get_hosts(),
    lists:foreach(fun(Host) ->
        case lists:member(Host, KnownHosts) of
            true -> ok;
            false -> throw(?ERROR_BAD_VALUE_LIST_NOT_ALLOWED(
                <<"hosts">>, onepanel_utils:convert(KnownHosts, {seq, binary})))
        end
    end, Hosts).


%%--------------------------------------------------------------------
%% @doc Copies file from current node to all given hosts.
%% @end
%%--------------------------------------------------------------------
-spec distribute_file(HostsOrNodes :: [service:host() | node()], Path :: file:name_all()) ->
    ok | no_return().
distribute_file(Hosts, Path) ->
    Nodes = nodes:service_to_nodes(?SERVICE_PANEL, Hosts),
    {ok, Content} = file:read_file(Path),
    onepanel_rpc:call_all(Nodes, filelib, ensure_dir, [Path]),
    onepanel_rpc:call_all(Nodes, file, write_file, [Path, Content]),
    ok.

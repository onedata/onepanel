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
-export([convert/2, get_type/1, typed_get/3, typed_get/4, typed_find/3]).
-export([ensure_known_hosts/1, distribute_file/2]).

% @formatter:off
-type primitive_type() :: atom | binary | float | integer | list | boolean.
-type collection_modifier() :: seq | keys | values.
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
    Password :: string() | binary()) -> {Key :: binary(), Value :: binary()}.
get_basic_auth_header(Username, Password) ->
    Hash = base64:encode(<<
        (onepanel_utils:convert(Username, binary))/binary, ":",
        (onepanel_utils:convert(Password, binary))/binary>>
    ),
    {?HDR_AUTHORIZATION, <<"Basic ", Hash/binary>>}.


%%--------------------------------------------------------------------
%% @doc @equiv wait_until(Module, Function, Args, Expectation, Attempts,
%% timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(Result), Attempts :: integer()) ->
    Result | no_return().
wait_until(Module, Function, Args, Expectation, Attempts) ->
    wait_until(Module, Function, Args, Expectation, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Waits until evaluation of ```apply(Module, Function, Args)''' returns
%% expected result, defined by validation function or exact value.
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
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
-spec convert(Value :: term(), Type :: type()) -> Value :: term().
convert(Values, {seq, Type}) ->
    lists:map(fun(Value) -> convert(Value, Type) end, Values);

% convert keys in a map
convert(Map, {keys, Type}) ->
    maps:from_list(
        [{convert(Key, Type), Value} || {Key, Value} <- maps:to_list(Map)]
    );

% convert values in a map
convert(Map, {values, Type}) ->
    maps:map(fun(_Key, Value) ->
        convert(Value, Type)
    end, Map);

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

convert(Value, Type) ->
    case get_type(Value) of
        unknown -> ?throw_error(?ERR_UNKNOWN_TYPE(Value));
        Type -> Value;
        ValueType ->
            TypeConverter = list_to_atom(
                atom_to_list(ValueType) ++ "_to_" ++ atom_to_list(Type)
            ),
            erlang:TypeConverter(Value)
    end.


%%--------------------------------------------------------------------
%% @doc Returns type of a given value.
%% @end
%%--------------------------------------------------------------------
-spec get_type(Value :: term()) -> Type :: type() | unknown.
get_type(Value) ->
    SupportedTypes = ["atom", "binary", "float", "integer", "list", "boolean"],
    onepanel_lists:foldl_while(fun(Type, unknown) ->
        TypeMatcher = erlang:list_to_atom("is_" ++ Type),
        case erlang:TypeMatcher(Value) of
            true -> {halt, erlang:list_to_atom(Type)};
            false -> {cont, unknown}
        end
    end, unknown, SupportedTypes).


%%--------------------------------------------------------------------
%% @doc Returns a value from the nested property list or map and converts it
%% to match the provided type.
%% Throws on error.
%% @end
%%--------------------------------------------------------------------
-spec typed_get(Keys :: onepanel_lists:keys() | onepanel_maps:keys(),
    Terms :: onepanel_lists:terms() | onepanel_maps:terms(), Type :: type()) ->
    Value :: term() | no_return().
typed_get(Keys, Terms, Type) ->
    case typed_find(Keys, Terms, Type) of
        {ok, Value} -> Value;
        #error{} = Error -> throw(Error)
    end.

%%--------------------------------------------------------------------
%% @doc Returns a value from the nested property list or map and converts it
%% to match the provided type. If key is missing returns default value.
%% @end
%%--------------------------------------------------------------------
-spec typed_get(Keys :: onepanel_lists:keys() | onepanel_maps:keys(),
    Terms :: onepanel_lists:terms() | onepanel_maps:terms(), Type :: type(),
    Default :: term()) -> Value :: term().
typed_get(Keys, Terms, Type, Default) ->
    case typed_find(Keys, Terms, Type) of
        {ok, Value} -> Value;
        _ -> Default
    end.


%%--------------------------------------------------------------------
%% @doc Returns a value from the nested property list or map and converts it
%% to match the provided type.
%% @end
%%--------------------------------------------------------------------
-spec typed_find(Keys :: onepanel_lists:keys() | onepanel_maps:keys(),
    Terms :: onepanel_lists:terms() | onepanel_maps:terms(), Type :: type()) ->
    {ok, Value :: term()} | #error{}.
typed_find(Keys, Terms, Type) when is_list(Terms) ->
    case onepanel_lists:get(Keys, Terms) of
        {ok, Value} -> {ok, convert(Value, Type)};
        #error{} = Error -> Error
    end;

typed_find(Keys, Terms, Type) when is_map(Terms) ->
    case onepanel_maps:get(Keys, Terms) of
        {ok, Value} -> {ok, convert(Value, Type)};
        #error{} = Error -> Error
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
            false -> ?throw_error(?ERR_HOST_NOT_FOUND(Host))
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
    onepanel_rpc:call_all(Nodes, file, write_file, [Path, Content]),
    ok.

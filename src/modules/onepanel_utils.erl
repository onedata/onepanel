%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions.
%%%--------------------------------------------------------------------
-module(onepanel_utils).
-author("Krzysztof Trzepla").

-include("names.hrl").
-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([get_basic_auth_header/2, get_ip_address/0]).
-export([wait_until/5, wait_until/6, save_file/2]).
-export([gen_uuid/0, get_nif_library_path/1, join/1, join/2, trim/2]).
-export([convert/2, get_type/1]).

-type primitive_type() :: atom | binary | float | integer | list | boolean.
-type type() :: primitive_type() | {seq, primitive_type()}.
-type expectation() :: {equal, Expected :: term()} | {validator,
    Validator :: fun((term()) -> term() | no_return())}.

-export_type([type/0]).

-define(UUID_LEN, 32).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns basic authorization header.
%%--------------------------------------------------------------------
-spec get_basic_auth_header(Username :: string() | binary(),
    Password :: string() | binary()) -> {Key :: binary(), Value :: binary()}.
get_basic_auth_header(Username, Password) ->
    Hash = base64:encode(<<
        (onepanel_utils:convert(Username, binary))/binary, ":",
        (onepanel_utils:convert(Password, binary))/binary>>
    ),
    {<<"Authorization">>, <<"Basic ", Hash/binary>>}.


%%--------------------------------------------------------------------
%% @doc @equiv wait_until(Module, Function, Args, Expectation, Attempts,
%% timer:seconds(1))
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(), Attempts :: integer()) -> ok | no_return().
wait_until(Module, Function, Args, Expectation, Attempts) ->
    wait_until(Module, Function, Args, Expectation, Attempts, timer:seconds(1)).


%%--------------------------------------------------------------------
%% @doc Waits until evaluation of ```apply(Module, Function, Args)''' returns
%% expected result, defined by validation function or exact value.
%% @end
%%--------------------------------------------------------------------
-spec wait_until(Module :: module(), Function :: atom(), Args :: list(),
    Expectation :: expectation(), Attempts :: integer(), Delay :: integer()) ->
    ok | no_return().
wait_until(_Module, _Function, _Args, _Expectation, Attempts, _Delay) when
    Attempts =< 0 ->
    throw(attempts_limit_exceeded);

wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay) ->
    wait_until(Module, Function, Args,
        {validator, fun(Result) -> Expected = Result end}, Attempts, Delay);

wait_until(Module, Function, Args, {validator, Validator}, Attempts, Delay) ->
    try
        Result = erlang:apply(Module, Function, Args),
        Validator(Result),
        ok
    catch
        _:Reason ->
            timer:sleep(Delay),
            ?log_debug("Call ~p:~p(~p) returned unexpected result: ~p."
            " Retrying...", [Module, Function, Args, Reason], true),
            wait_until(Module, Function, Args, {validator, Validator},
                Attempts - 1, Delay)
    end;

wait_until(Module, Function, Args, Expected, Attempts, Delay) ->
    wait_until(Module, Function, Args, {equal, Expected}, Attempts, Delay).


%%--------------------------------------------------------------------
%% @doc Writes content of a file indicated by path. In case of an error throws
%% an exception.
%% @end
%%--------------------------------------------------------------------
-spec save_file(Path :: file:name_all(), Content :: binary()) -> ok | no_return().
save_file(Path, Content) ->
    case file:write_file(Path, Content) of
        ok -> ok;
        {error, Reason} -> ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Queries onezone for IP address of the host where function has been
%% evaluated. In case of an error throws an exception.
%% @end
%%--------------------------------------------------------------------
-spec get_ip_address() -> IpAddress :: binary() | no_return().
get_ip_address() ->
    case oz_providers:check_ip_address(provider) of
        {ok, IpAddress} -> IpAddress;
        {error, Reason} -> ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Generates random UUID.
%%--------------------------------------------------------------------
-spec gen_uuid() -> binary().
gen_uuid() ->
    http_utils:base64url_encode(crypto:rand_bytes(?UUID_LEN)).


%%--------------------------------------------------------------------
%% @doc Returns the NIF native library path. The library is first searched
%% in application priv dir, and then under ../priv and ./priv .
%%--------------------------------------------------------------------
-spec get_nif_library_path(LibName :: string()) ->
    LibPath :: file:filename_all().
get_nif_library_path(LibName) ->
    case code:priv_dir(?APP_NAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", "priv"])) of
                true -> filename:join(["..", "priv", LibName]);
                _ -> filename:join(["priv", LibName])
            end;
        Dir -> filename:join(Dir, LibName)
    end.


%%--------------------------------------------------------------------
%% @doc @equiv join(Tokens, ```<<>>''')
%%--------------------------------------------------------------------
-spec join(Tokens :: [term()]) -> Binary :: binary().
join(Tokens) ->
    join(Tokens, <<>>).


%%--------------------------------------------------------------------
%% @doc Joins sequence of tokens using provided separator.
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
%%--------------------------------------------------------------------
-spec convert(Value :: term(), Type :: type()) -> Value :: term().
convert(Values, {seq, Type}) ->
    lists:map(fun(Value) -> convert(Value, Type) end, Values);

convert(Value, boolean) when is_atom(Value) ->
    convert(Value, atom);

convert(Value, binary) when is_atom(Value) ->
    erlang:atom_to_binary(Value, utf8);

convert(Value, atom) when is_binary(Value) ->
    erlang:binary_to_atom(Value, utf8);

convert(Value, float) when is_integer(Value) ->
    Value * 1.0;

convert(Value, Type) ->
    case get_type(Value) of
        Type -> Value;
        ValueType ->
            TypeConverter = erlang:list_to_atom(erlang:atom_to_list(ValueType)
            ++ "_to_" ++ erlang:atom_to_list(Type)),
            erlang:TypeConverter(Value)
    end.


%%--------------------------------------------------------------------
%% @doc Returns type of a given value.
%%--------------------------------------------------------------------
-spec get_type(Value :: term()) -> Type :: type().
get_type(Value) ->
    SupportedTypes = ["atom", "binary", "float", "integer", "list", "boolean"],
    lists:foldl(fun
        (Type, unknown = ValueType) ->
            TypeMatcher = erlang:list_to_atom("is_" ++ Type),
            case erlang:TypeMatcher(Value) of
                true -> erlang:list_to_atom(Type);
                false -> ValueType
            end;
        (_Type, ValueType) -> ValueType
    end, unknown, SupportedTypes).


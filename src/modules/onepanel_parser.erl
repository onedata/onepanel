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
-module(onepanel_parser).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/logger.hrl").

%% API
-export([parse/2]).

-type key() :: atom().
-type keys() :: [key()].
-type data() :: proplists:proplist().
-type args() :: #{Key :: key() => Value :: term()}.
-type args_list() :: [{Key :: key(), Value :: term()}].
-type presence() :: required | optional | {optional, Default :: term()}.
-type spec() :: #{Key :: key() => ValueSpec :: value_spec()}.
-type value_spec() :: term() | {term(), presence()}.

-export_type([key/0, keys/0, data/0, args/0, spec/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec parse(Data :: data(), ArgsSpec :: spec()) -> Args :: args().
parse(Data, ArgsSpec) ->
    parse(Data, maps:to_list(ArgsSpec), [], #{}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec parse(Data :: data(), ArgsList :: args_list(), Keys :: keys(), Args :: args()) ->
    Args :: args() | no_return().
parse(_Data, [], _Keys, Args) ->
    Args;

parse(Data, [{'_', Spec} = ValueSpec], Keys, Args) ->
    lists:foldl(fun
        ({Key, Value}, Acc) when is_binary(Key) ->
            Arg = parse_value(Value, Spec, [Key | Keys]),
            maps:put(Key, Arg, Acc);
        (_, _) ->
            ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec})
    end, Args, Data);

parse(Data, [{'_', _} = ValueSpec | ArgsSpec], Keys, Args) ->
    parse(Data, ArgsSpec ++ [ValueSpec], Keys, Args);

parse(Data, [{Key, Spec} | ArgsSpec], Keys, Args) ->
    BinKey = erlang:atom_to_binary(Key, utf8),
    case {lists:keyfind(BinKey, 1, Data), is_optional(Spec)} of
        {{BinKey, Value}, {_, ValueSpec}} ->
            NewData = lists:keydelete(BinKey, 1, Data),
            Arg = parse_value(Value, ValueSpec, [Key | Keys]),
            NewArgs = maps:put(Key, Arg, Args),
            parse(NewData, ArgsSpec, Keys, NewArgs);
        {false, true} ->
            parse(Data, ArgsSpec, Keys, Args);
        {false, {true, Arg}} ->
            parse(Data, ArgsSpec, Keys, maps:put(Key, Arg, Args));
        {false, {false, _}} ->
            ?throw({?ERR_MISSING_KEY, [Key | Keys]})
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec parse_value(Value :: term(), ValueSpec :: value_spec(), Keys :: keys()) ->
    Value :: term() | no_return().
parse_value(Value, {equal, {Type, Equal}} = ValueSpec, Keys) ->
    case parse_value(Value, Type, Keys) of
        Equal -> Equal;
        _ -> ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec})
    end;

parse_value(_Value, {equal, _} = ValueSpec, Keys) ->
    ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec});

parse_value(Value, integer, _Keys) when is_integer(Value) ->
    Value;

parse_value(Value, float, _Keys) when is_float(Value) ->
    Value;

parse_value(Value, string, _Keys) when is_binary(Value) ->
    Value;

parse_value(Value, atom, _Keys) when is_atom(Value) ->
    Value;

parse_value(Value, boolean = ValueSpec, Keys) when is_atom(Value) ->
    case Value of
        true -> true;
        false -> false;
        _ -> ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec})
    end;

parse_value(Value, boolean, Keys) ->
    parse_value(parse_value(Value, atom, Keys), boolean, Keys);

parse_value(Value, atom = ValueSpec, Keys) ->
    convert(Value, ValueSpec, Keys, fun(V) ->
        erlang:binary_to_atom(V, utf8)
    end);

parse_value(Value, integer = ValueSpec, Keys) ->
    convert(Value, ValueSpec, Keys, fun(V) -> erlang:binary_to_integer(V) end);

parse_value(Value, float = ValueSpec, Keys) ->
    convert(Value, ValueSpec, Keys, fun(V) -> erlang:binary_to_float(V) end);

parse_value(Value, string = ValueSpec, Keys) when is_list(Value) ->
    convert(Value, ValueSpec, Keys, fun(V) -> erlang:list_to_binary(V) end);

parse_value(Value, {oneof, ValueSpecs}, Keys) when is_list(ValueSpecs) ->
    Arg = lists:foldl(fun
        (ValueSpec, undefined) ->
            try
                parse_value(Value, ValueSpec, Keys)
            catch
                _:_ -> undefined
            end;
        (_, _Arg) -> _Arg
    end, undefined, ValueSpecs),
    case Arg of
        undefined ->
            ?throw({?ERR_INVALID_KEY_VALUE, Keys, {oneof, ValueSpecs}});
        _ -> Arg
    end;

parse_value(Values, ValueSpec, Keys) when is_list(ValueSpec) ->
    try
        lists:map(fun(Value) ->
            parse_value(Value, hd(ValueSpec), Keys)
        end, Values)
    catch
        _:_ -> ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec})
    end;

parse_value(Value, ValueSpec, Keys) when is_map(ValueSpec) ->
    parse(Value, maps:to_list(ValueSpec), Keys, #{});

parse_value(_Value, ValueSpec, Keys) ->
    ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec is_optional(Spec :: spec()) ->
    Optional :: boolean() | {boolean(), Spec :: spec()}.
is_optional({_Spec, optional}) -> true;
is_optional({_Spec, {optional, Value}}) -> {true, Value};
is_optional({Spec, required}) -> {false, Spec};
is_optional(Spec) -> {false, Spec}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec convert(Value :: term(), ValueSpec :: value_spec(), Keys :: keys(),
    Fun :: fun()) -> Term :: term() | no_return().
convert(Value, ValueSpec, Keys, Fun) ->
    try
        Fun(Value)
    catch
        _:_ -> ?throw({?ERR_INVALID_KEY_VALUE, Keys, ValueSpec})
    end.
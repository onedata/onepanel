%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for parsing data according to
%%% provided specification.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_parser).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([parse/2, prepare_subclasses/1]).

%% @formatter:off
-type key() :: atom().
-type keys() :: [key()].
-type data() :: map().
-type args() :: #{Key :: key() => Value :: term()}.
-type args_list() :: [{Key :: key(), Value :: term()}].
-type presence() :: required | optional | {optional, Default :: term()}.

-type type_spec() :: integer | float | string | binary | atom | boolean |
    [type_spec()].
-type model_spec() :: type_spec() |
    {enum, type_spec(), Allowed :: [term()]} |
    {subclasses, {Discriminator :: key(), #{Value :: term() => spec()}}}.
-type field_spec() :: model_spec() | {model_spec() | presence()}.
-type spec() :: #{Key :: key() => ValueSpec :: spec () | field_spec()}.
%% @formatter:on

-export_type([key/0, keys/0, data/0, args/0, spec/0, field_spec/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Parses data according to provided specification.
%% @end
%%--------------------------------------------------------------------
-spec parse(Data :: data(), ArgsSpec :: spec()) -> Args :: args().
parse(Data, ArgsSpec) ->
    parse(Data, maps:to_list(ArgsSpec), [], #{}).


%%--------------------------------------------------------------------
%% @doc Takes a list of possible specification for a polymorphic
%% rest data model. Returns the discriminator field (key used to distinguish
%% the types) and a map from discriminator values to specifications
%% of subclasses.
%% @end
%%--------------------------------------------------------------------
-spec prepare_subclasses(Subclass) -> {Discriminator, #{binary() => spec()}} when
    Subclass :: #{Discriminator := {discriminator, binary()}, key() => field_spec()},
    Discriminator :: key().
prepare_subclasses(SubclassModels) ->
    lists:foldl(fun(Model, {FieldAcc, ValueToSubclassAcc}) ->
        IsDiscriminator = fun
            ({Field, {discriminator, Value}}) -> {true, {Field, Value}};
            (_) -> false
        end,
        case lists:filtermap(IsDiscriminator, maps:to_list(Model)) of
            [] ->
                ?critical("Bad parser spec: missing discriminator"),
                error({bad_model_subclasses, SubclassModels});
            [{Field, Value}] ->
                case FieldAcc of
                    undefined -> {Field, #{Value => Model#{Field => {equal, Value}}}};
                    Field ->
                        {Field, ValueToSubclassAcc#{Value => Model#{Field => {equal, Value}}}};
                    _OtherField ->
                        ?critical("Bad parser spec: different discriminator fields"),
                        error({bad_model_subclasses, SubclassModels})
                end;
            _MultipleDiscriminators ->
                ?critical("Bad parser spec: multiple discriminator fields"),
                error({bad_model_subclasses, SubclassModels})
        end
    end, {undefined, #{}}, SubclassModels).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Parses data according to provided specification.
%% @end
%%--------------------------------------------------------------------
-spec parse(Data :: data(), ArgsList :: args_list(), Keys :: keys(), Args :: args()) ->
    Args :: args() | no_return().
parse(_Data, [], _Keys, Args) ->
    Args;

parse(Data, [{'_', Spec} = _ValueSpec], Keys, Args) when is_map(Data) ->
    maps:fold(fun
        (Key, Value, Acc) when is_binary(Key) ->
            Arg = parse_value(Value, Spec, [Key | Keys]),
            maps:put(Key, Arg, Acc);
        (_, _, _) -> throw(?ERROR_BAD_DATA(join_keys(Keys)))
    end, Args, Data);

parse(Data, [{'_', _} = ValueSpec | ArgsSpec], Keys, Args) when is_map(Data) ->
    parse(Data, ArgsSpec ++ [ValueSpec], Keys, Args);

parse(Data, [{Key, Spec} | ArgsSpec], Keys, Args) when is_map(Data) ->
    BinKey = onepanel_utils:convert(Key, binary),
    case {maps:find(BinKey, Data), is_optional(Spec)} of
        {{ok, Value}, {_, ValueSpec}} ->
            NewData = maps:remove(BinKey, Data),
            Arg = parse_value(Value, ValueSpec, [Key | Keys]),
            NewArgs = maps:put(Key, Arg, Args),
            parse(NewData, ArgsSpec, Keys, NewArgs);
        {error, {true, _}} ->
            case has_default(Spec) of
                {true, Arg} ->
                    parse(Data, ArgsSpec, Keys, maps:put(Key, Arg, Args));
                false ->
                    parse(Data, ArgsSpec, Keys, Args)
            end;
        {error, {false, _}} ->
            throw(?ERROR_MISSING_REQUIRED_VALUE(join_keys([Key | Keys])))
    end;

parse(_Data, [{Key, _Spec} | _ArgsSpec], Keys, _Args) ->
    throw(?ERROR_BAD_DATA(join_keys([Key | Keys]))).


%%--------------------------------------------------------------------
%% @private @doc Parses value according to provided specification.
%% @end
%%--------------------------------------------------------------------
-spec parse_value(Value :: term(), ValueSpec :: field_spec(), Keys :: keys()) ->
    Value :: term() | no_return().
parse_value(Value, {equal, Equal}, Keys) ->
    Type = onepanel_utils:get_type(Equal),
    case parse_value(Value, Type, Keys) of
        Equal -> Equal;
        _ -> throw(?ERROR_BAD_VALUE_NOT_ALLOWED(join_keys(Keys), [Equal]))
    end;

parse_value(Value, integer, _Keys) when is_integer(Value) ->
    Value;

parse_value(Value, float, _Keys) when is_integer(Value) ->
    Value * 1.0;

parse_value(Value, float, _Keys) when is_float(Value) ->
    Value;

parse_value(Value, string, Keys) ->
    parse_value(Value, binary, Keys);

parse_value(Value, binary, _Keys) when is_binary(Value) ->
    Value;

parse_value(_Value, binary, Keys) ->
    throw(?ERROR_BAD_VALUE_BINARY(join_keys(Keys)));

parse_value(Value, atom, _Keys) when is_atom(Value) ->
    Value;

parse_value(Value, boolean, Keys) when is_atom(Value) ->
    case Value of
        true -> true;
        false -> false;
        _ -> throw(?ERROR_BAD_VALUE_BOOLEAN(join_keys(Keys)))
    end;

parse_value(Value, boolean, Keys) ->
    parse_value(parse_value(Value, atom, Keys), boolean, Keys);

parse_value(Value, atom, Keys) ->
    try
        erlang:binary_to_atom(Value, utf8)
    catch
        _:_ -> throw(?ERROR_BAD_VALUE_ATOM(join_keys(Keys)))
    end;

parse_value(Value, integer, Keys) ->
    try
        erlang:binary_to_integer(Value)
    catch
        _:_ -> throw(?ERROR_BAD_VALUE_INTEGER(join_keys(Keys)))
    end;

parse_value(Value, float, Keys) ->
    try
        case string:to_float(Value) of
            {Float, <<>>} -> Float;
            {error, no_float} -> erlang:binary_to_integer(Value) * 1.0
        end
    catch
        _:_ -> throw(?ERROR_BAD_VALUE_FLOAT(join_keys(Keys)))
    end;

parse_value(Value, {enum, ValueType, AllowedValues}, Keys) ->
    TypedValue = parse_value(Value, ValueType, Keys),
    case lists:member(TypedValue, AllowedValues) of
        true -> TypedValue;
        false -> throw(?ERROR_BAD_VALUE_NOT_ALLOWED(join_keys(Keys), AllowedValues))
    end;

parse_value(Value, {subclasses, {DiscriminatorField, ValueToSpec}}, Keys) ->
    BinKey = onepanel_utils:convert(DiscriminatorField, binary),
    DiscriminatorValue = case maps:find (BinKey, Value) of
        {ok, V} -> V;
        error -> throw(?ERROR_MISSING_REQUIRED_VALUE(join_keys([BinKey | Keys])))
    end,
    case maps:find(DiscriminatorValue, ValueToSpec) of
        {ok, Model} -> parse_value(Value, Model, Keys);
        error -> throw(?ERROR_BAD_VALUE_NOT_ALLOWED(join_keys([BinKey | Keys]), maps:keys(ValueToSpec)))
    end;

parse_value(Values, [ValueSpec], Keys) ->
    try
        [parse_value(Value, ValueSpec, Keys) || Value <- Values]
    catch
        _:_ ->
            case ValueSpec of
                atom ->
                    throw(?ERROR_BAD_VALUE_LIST_OF_ATOMS(join_keys(Keys)));
                _ when ValueSpec == binary; ValueSpec == string ->
                    throw(?ERROR_BAD_VALUE_LIST_OF_BINARIES(join_keys(Keys)));
                _ ->
                    throw(?ERROR_BAD_VALUE(join_keys(Keys),
                        [get_expectation([ValueSpec])]))
            end
        end;

parse_value(Value, ValueSpec, Keys) when is_map(ValueSpec) ->
    parse(Value, maps:to_list(ValueSpec), Keys, #{});

parse_value(_Value, ValueSpec, Keys) ->
    throw(?ERROR_BAD_VALUE(join_keys(Keys), get_expectation(ValueSpec))).


%%--------------------------------------------------------------------
%% @private @doc Returns 'true' if key associated with provided specification
%% is optional, otherwise 'false'.
%% @end
%%--------------------------------------------------------------------
-spec is_optional(Spec :: field_spec()) -> {Optional :: boolean(), Spec :: term()}.
is_optional({Spec, optional}) -> {true, Spec};
is_optional({Spec, {optional, _Value}}) -> {true, Spec};
is_optional({Spec, required}) -> {false, Spec};
is_optional(Spec) -> {false, Spec}.


%%--------------------------------------------------------------------
%% @private @doc Checks whether provided specification has default value.
%% @end
%%--------------------------------------------------------------------
-spec has_default(Spec :: field_spec()) -> {true, Spec :: term()} | false.
has_default({_Spec, {optional, Value}}) -> {true, Value};
has_default(_) -> false.


%%--------------------------------------------------------------------
%% @private @doc Merges a list of keys into human-readable format
%% using the "." character to indicate nested keys.
%% Nested keys should be given from the most nested to the outermost.
%% @end
%%--------------------------------------------------------------------
-spec join_keys(Keys :: onepanel_parser:keys()) -> Key :: binary().
join_keys(KeysBottomUp) ->
    onepanel_utils:join(lists:reverse(KeysBottomUp), <<".">>).


%%--------------------------------------------------------------------
%% @private @doc Returns human-readable expectation for a value specification.
%% @end
%%--------------------------------------------------------------------
-spec get_expectation(ValueSpec :: field_spec()) ->
    Expectation :: binary().
get_expectation(ValueSpec) ->
    get_expectation(ValueSpec, <<>>).


%%--------------------------------------------------------------------
%% @private @doc Returns human-readable expectation for a value specification.
%% @end
%%--------------------------------------------------------------------
-spec get_expectation(ValueSpec :: field_spec(), Acc :: binary()) ->
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

get_expectation({enum, Values}, Acc) ->
    <<Acc/binary, (onepanel_utils:join(["'",Values,"'"], <<" | ">>))/binary>>;

get_expectation({subclasses, {DiscriminatorField, ValueToSpec}}, Acc) ->
    <<Acc/binary, "{", DiscriminatorField/binary, ": ",
        (get_expectation({enum, maps:keys(ValueToSpec)}))/binary, ", ...}">>;

get_expectation(ValueSpecs, Acc) when is_list(ValueSpecs) ->
    Tokens = lists:map(fun(ValueSpec) ->
        get_expectation(ValueSpec)
    end, ValueSpecs),
    <<Acc/binary, "[", (onepanel_utils:join(Tokens, <<", ">>))/binary, "]">>;

get_expectation(ValueSpec, Acc) when is_map(ValueSpec) ->
    Tokens = lists:map(fun({Key, Value}) ->
        BinKey = onepanel_utils:convert(Key, binary),
        <<"'", (BinKey)/binary, "': ", (get_expectation(Value))/binary>>
    end, maps:to_list(ValueSpec)),
    <<Acc/binary, "{", (onepanel_utils:join(Tokens, <<", ">>))/binary, "}">>.


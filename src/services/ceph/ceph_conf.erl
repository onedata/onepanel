%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Functions for manipulation of Ceph config files.
%%% The config file is split by scopes to which rules apply.
%%% Scope begins by name in squared brackets and contains entries
%%% in the key = value format. For example:
%%% [global]
%%% auth client required = cephx
%%% [osd]
%%% osd journal size = 1024
%%% @end
%%%--------------------------------------------------------------------
-module(ceph_conf).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

% having 'global' as an atom ensures it is the first scope after sorting
% which improves readability of resulting file
-type scope() :: global | ceph:daemon() | binary().
-type key() :: binary().

% when parsing the file only binaries are returned, but configuration
% passed for writing can contain integers
-type value() :: binary() | [binary()] | integer() | [integer()].
-type path() :: file:name_all().

-type config() :: #{scope() => #{key() => value()}}.

-export_type([config/0, key/0, value/0, path/0]).

% on-disk manipulation
-export([write/2, update_file/4, put_in_file/4, read/1, read/3]).
% in-memory manipulation
-export([parse/1, update/4, find/3, put/4, append/4]).

%%--------------------------------------------------------------------
%% @doc
%% Writes provided config to file.
%% @end
%%--------------------------------------------------------------------
-spec write(config(), path()) -> ok.
write(Config, Path) ->
    ok = file:write_file(Path, format(Config)).

%%--------------------------------------------------------------------
%% @doc
%% Rewrites config in given file by setting given value.
%% @end
%%--------------------------------------------------------------------
put_in_file(Scope, Key, Value, Path) ->
    update_file(Scope, Key, fun(_) -> Value end, Path).


%%--------------------------------------------------------------------
%% @doc
%% Sets given value in the config object.
%% @end
%%--------------------------------------------------------------------
-spec put(Scope :: scope(), key(), value(), config()) -> config().
put(Scope, Key, Value, Config) ->
    update(Scope, Key, fun(_) -> Value end, Config).


%%--------------------------------------------------------------------
%% @doc
%% Append list stored in given config field.
%% @end
%%--------------------------------------------------------------------
-spec append(scope(), key(), value(), config()) -> config().
append(Scope, Key, NewValues, Config) when is_list(NewValues) ->
    update(Scope, Key, fun
        (undefined) -> NewValues;
        (Previous) when is_list(Previous) -> Previous ++ NewValues;
        (Previous) -> [Previous | NewValues]
    end, Config);

append(Scope, Key, NewValue, Config) ->
    append(Scope, Key, [NewValue], Config).


%%--------------------------------------------------------------------
%% @doc
%% Updates value in a config using given function.
%% If the key does not exist the function is given 'undefined'.
%% If the update fun returns atom 'undefined' the key is removed
%% from config.
%% @end
%%--------------------------------------------------------------------
-spec update(scope(), key(), Updater, config()) -> config()
    when Updater :: fun((value() | undefined) -> value() | undefined).
update(Scope, Key, Updater, Config) when is_function(Updater, 1) ->
    NormalizedScope = parse_scope(Scope),
    OldScope = maps:get(NormalizedScope, Config, #{}),
    OldValue = maps:get(Key, OldScope, undefined),
    NewScope = case Updater(OldValue) of
        undefined -> maps:remove(Key, OldScope);
        NewValue -> OldScope#{Key => NewValue}
    end,
    case NewScope == #{} of
        true -> maps:remove(NormalizedScope, Config);
        false -> Config#{NormalizedScope => NewScope}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Updates config value in the given file.
%% The update semantics are the same as for {@link update/4}.
%% @end
%%--------------------------------------------------------------------
-spec update_file(scope(), key(), Updater, path()) -> ok
    when Updater :: fun((value() | undefined) -> value() | undefined).
update_file(Scope, Key, Updater, Path) ->
    OldConfig = read(Path),
    write(update(Scope, Key, Updater, OldConfig), Path).


%%--------------------------------------------------------------------
%% @doc
%% Finds value in a parsed config.
%% @end
%%--------------------------------------------------------------------
-spec find(scope(), key(), config()) -> {ok, Value :: value()} | error.
find(Scope, Key, Config) ->
    NormalizedScope = parse_scope(Scope),
    kv_utils:find([NormalizedScope, Key], Config).


%%--------------------------------------------------------------------
%% @doc
%% Reads specified key from configuration in given file.
%% @end
%%--------------------------------------------------------------------
-spec read(Scope :: scope(), Key :: key(), Path :: file:name_all()) ->
    {ok, Value :: value()} | error.
read(Scope, Key, Path) ->
    Content = read(Path),
    find(Scope, Key, Content).


%%--------------------------------------------------------------------
%% @doc
%% Reads and parses configuration from given file.
%% @end
%%--------------------------------------------------------------------
-spec read(Path :: path()) -> config() | no_return().
read(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            parse(Content);
        {error, Reason} ->
            throw(?ERROR_FILE_ACCESS(Path, Reason))
    end.


%%--------------------------------------------------------------------
%% @doc
%% Parse string formatted like ceph.conf.
%% @end
%%--------------------------------------------------------------------
-spec parse(binary()) -> config().
parse(Contents) ->
    Lines = string:split(Contents, "\n", all),

    {_, Result} = lists:foldl(fun(Line, {CurrentScope, Config}) ->
        Stripped = remove_comment(Line),
        case parse_line(Stripped) of
            {scope_head, NewScope} ->
                {NewScope, Config};
            {entry, Key, Value} ->
                Parsed = parse_value(Value),
                NewConfig = maps:update_with(CurrentScope, fun(ScopeEntries) ->
                    ScopeEntries#{Key => Parsed}
                end, #{Key => Parsed}, Config),
                {CurrentScope, NewConfig};
            ignore ->
                {CurrentScope, Config}
        end
    end, {global, #{}}, Lines),
    Result.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Strips comment starting with # or ; from input.
%% @end
%%--------------------------------------------------------------------
-spec remove_comment(binary()) -> binary().
remove_comment(Line) ->
    {Value, _Comment} = string:take(Line, [$#, $;], true, leading),
    string:trim(Value).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Identifies contents of a single config line.
%% Expects input to be whitespace-trimmed.
%% @end
%%--------------------------------------------------------------------
-spec parse_line(Line :: binary()) ->
    ignore | {scope_head, scope()} | {entry, key(), value()}.
parse_line(<<>>) ->
    ignore;

% for scopes like [osd]
parse_line(<<"[", Rest/binary>>) ->
    % capture bracketed scope name
    [Scope, _] = string:split(Rest, "]"),
    {scope_head, parse_scope(Scope)};

parse_line(<<Line/binary>>) ->
    case string:split(Line, <<"=">>, leading) of
        [Key, Value] ->
            {entry, string:trim(Key), string:trim(Value)};
        _ ->
            ?error("Ceph config parsing faild on line ~B", [Line]),
            error(?ERR_PARSING_FAILURE(Line))
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts known scopes to atoms.
%% @end
%%--------------------------------------------------------------------
-spec parse_scope(binary() | atom()) -> scope().
parse_scope(Atom) when is_atom(Atom) ->
    Binary = atom_to_binary(Atom, utf8),
    parse_scope(Binary);
parse_scope(<<"global">>) -> global;
parse_scope(<<"osd">>) -> osd;
parse_scope(<<"mon">>) -> mon;
parse_scope(<<"mgr">>) -> mgr;
parse_scope(<<Binary/binary>>) -> Binary.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Splits value into elements if it is a comma-separated list.
%% Returns unchanged otherwise.
%% @end
%%--------------------------------------------------------------------
-spec parse_value(unicode:chardata()) -> value().
parse_value(Value) ->
    case string:split(Value, ",", all) of
        [Value] -> Value;
        List when is_list(List) -> [string:trim(Val) || Val <- List]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts internal configuration format into binary
%% ready to be written to ceph.conf file.
%% @end
%%--------------------------------------------------------------------
-spec format(config()) -> binary().
format(Config) ->
    Result = lists:foldr(fun({Scope, Entries}, Acc) ->
        EntriesStr = [format_entry(Key, Value)
            || {Key, Value} <- maps:to_list(Entries)],
        ["[", onepanel_utils:convert(Scope, binary), "]\n", EntriesStr | Acc]
    end, [], lists:sort(maps:to_list(Config))),
    unicode:characters_to_binary(Result).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Formats single key-value pair.
%% If value is a list joins its elements with a comma.
%% @end
%%--------------------------------------------------------------------
-spec format_entry(key(), value()) -> binary().
format_entry(Key, Values) when is_list(Values) ->
    Value = onepanel_utils:join(Values, <<", ">>),
    <<Key/binary, " = ", Value/binary, "\n">>;

format_entry(Key, Value) ->
    format_entry(Key, [Value]).

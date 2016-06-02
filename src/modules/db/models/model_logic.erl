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
-module(model_logic).
-author("Krzysztof Trzepla").

%% API
-export([table_name/1]).
-export([create/2, save/2, update/3, get/2, exists/2, delete/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

table_name(Model) ->
    Model.

create(Model, Record) ->
    run(fun() ->
        Table = table_name(Model),
        Key = erlang:element(2, Record),
        case mnesia:read(Table, Key) of
            [] -> ok;
            [_ | _] -> mnesia:abort(already_exists)
        end,
        mnesia:write(Table, Record, write)
    end).

save(Model, Record) ->
    run(fun() ->
        Table = table_name(Model),
        mnesia:write(Table, Record, write)
    end).

update(Model, Key, Diff) ->
    run(fun() ->
        Table = table_name(Model),
        Record = case mnesia:read(Table, Key) of
            [] -> mnesia:abort(not_found);
            [R] -> R
        end,
        mnesia:write(Table, apply_diff(Record, Diff), write)
    end).

get(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> mnesia:abort(not_found);
            [Record] -> {ok, Record}
        end
    end).

exists(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> false;
            [_ | _] -> true
        end
    end).

delete(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        mnesia:delete(Table, Key, write)
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

run(Transaction) ->
    try
        mnesia:activity(transaction, Transaction)
    catch
        _:{aborted, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

apply_diff(Record, Diff) when is_function(Diff) ->
    Diff(Record);

apply_diff(Record, Diff) when is_map(Diff) ->
    [Model | Values] = erlang:tuple_to_list(Record),
    Fields = Model:fields(),
    NewValues = lists:map(fun({Field, Value}) ->
        case maps:find(Field, Diff) of
            {ok, NewValue} -> NewValue;
            error -> Value
        end
    end, lists:zip(Fields, Values)),
    erlang:list_to_tuple([Model | NewValues]).
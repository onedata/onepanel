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

-type model() :: atom().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec table_name(Model :: model()) -> Name :: atom().
table_name(Model) ->
    Model.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec create(Model :: model(), Record :: model_behaviour:record()) ->
    ok | {error, Reason :: term()}.
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


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec save(Model :: model(), Record :: model_behaviour:record()) ->
    ok | {error, Reason :: term()}.
save(Model, Record) ->
    run(fun() ->
        Table = table_name(Model),
        mnesia:write(Table, Record, write)
    end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec update(Model :: model(), Key :: model_behaviour:key(),
    Diff :: model_behaviour:diff()) -> ok | {error, Reason :: term()}.
update(Model, Key, Diff) ->
    run(fun() ->
        Table = table_name(Model),
        Record = case mnesia:read(Table, Key) of
            [] -> mnesia:abort(not_found);
            [R] -> R
        end,
        mnesia:write(Table, apply_diff(Record, Diff), write)
    end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get(Model :: model(), Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | {error, Reason :: term()}.
get(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> mnesia:abort(not_found);
            [Record] -> {ok, Record}
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec exists(Model :: model(), Key :: model_behaviour:key()) -> 
    boolean() | {error, Reason :: term()}.
exists(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> false;
            [_ | _] -> true
        end
    end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec delete(Model :: model(), Key :: model_behaviour:key()) ->
    ok | {error, Reason :: term()}.
delete(Model, Key) ->
    run(fun() ->
        Table = table_name(Model),
        mnesia:delete(Table, Key, write)
    end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec run(Transaction :: fun()) -> term() | {error, Reason :: term()}.
run(Transaction) ->
    try
        mnesia:activity(transaction, Transaction)
    catch
        _:{aborted, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec apply_diff(Record :: model_behaviour:record(), model_behaviour:diff()) ->
    NewRecord :: model_behaviour:record().
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
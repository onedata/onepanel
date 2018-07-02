%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains model management functions.
%%% @end
%%%--------------------------------------------------------------------
-module(model).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").

%% API
-export([get_table_name/1, get_models/0]).
-export([create/2, save/2, update/3, get/2, exists/2, delete/2, list/1]).
-export([exists/1, select/2, size/1, clear/1, transaction/1]).

-type model() :: atom().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a table name associated with the model.
%% @end
%%--------------------------------------------------------------------
-spec get_table_name(Model :: model()) -> Name :: atom().
get_table_name(Model) ->
    Model.


%%--------------------------------------------------------------------
%% @doc Returns a list of supported models.
%% @end
%%--------------------------------------------------------------------
-spec get_models() -> Models :: [model()].
get_models() ->
    ?MODELS.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Model :: model(), Record :: model_behaviour:record()) ->
    {ok, model_behaviour:key()} | #error{} | no_return().
create(Model, Record) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        Key = erlang:element(2, Record),
        case mnesia:read(Table, Key) of
            [] ->
                mnesia:write(Table, Record, write),
                {ok, Key};
            [_ | _] ->
                ?make_error(?ERR_ALREADY_EXISTS, [Model, Record])
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Model :: model(), Record :: model_behaviour:record()) ->
    ok | no_return().
save(Model, Record) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        mnesia:write(Table, Record, write)
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Model :: model(), Key :: model_behaviour:key(),
    Diff :: model_behaviour:diff()) -> ok | no_return().
update(Model, Key, Diff) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> mnesia:abort(?make_error(?ERR_NOT_FOUND, [Model, Key, Diff]));
            [Record] -> mnesia:write(Table, apply_diff(Record, Diff), write)
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Model :: model(), Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | #error{} | no_return().
get(Model, Key) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> ?make_error(?ERR_NOT_FOUND, [Model, Key]);
            [Record] -> {ok, Record}
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Model :: model(), Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Model, Key) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> false;
            [_ | _] -> true
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:delete/1}
%% @end
%%--------------------------------------------------------------------
-spec delete(Model :: model(), Key :: model_behaviour:key()) ->
    ok | no_return().
delete(Model, Key) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        mnesia:delete(Table, Key, write)
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:list/0}
%% @end
%%--------------------------------------------------------------------
-spec list(Model :: model()) ->
    Records :: [model_behaviour:record()] | no_return().
list(Model) ->
    select(Model, [{'_', [], ['$_']}]).


%%--------------------------------------------------------------------
%% @doc Returns 'true' if model table exists, otherwise false.
%%--------------------------------------------------------------------
-spec exists(Model :: model()) -> boolean().
exists(Model) ->
    Table = get_table_name(Model),
    lists:member(Table, mnesia:system_info(tables)).


%%--------------------------------------------------------------------
%% @doc Returns a list of the selected model instances.
%% @end
%%--------------------------------------------------------------------
-spec select(Model :: model(), MatchSpec :: any()) ->
    Records :: [model_behaviour:record()] | no_return().
select(Model, MatchSpec) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        mnesia:select(Table, MatchSpec)
    end).


%%--------------------------------------------------------------------
%% @doc Returns number of model instances.
%% @end
%%--------------------------------------------------------------------
-spec size(Model :: model()) -> non_neg_integer().
size(Model) ->
    Table = get_table_name(Model),
    mnesia:table_info(Table, size).


%%--------------------------------------------------------------------
%% @doc Removes all the model instances.
%% @end
%%--------------------------------------------------------------------
-spec clear(Model :: model()) -> ok | #error{}.
clear(Model) ->
    Table = get_table_name(Model),
    case mnesia:clear_table(Table) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> ?make_error(Reason, [Model])
    end.


%%--------------------------------------------------------------------
%% @doc Provides transactional environment for operations on models.
%% @end
%%--------------------------------------------------------------------
-spec transaction(Transaction :: fun()) -> term() | no_return().
transaction(Transaction) ->
    try
        mnesia:activity(transaction, Transaction)
    catch
        _:{aborted, Reason} -> ?throw_error(Reason, [Transaction]);
        _:Reason -> ?throw_error(Reason, [Transaction])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Modifies model instance according to provided update method.
%% @end
%%--------------------------------------------------------------------
-spec apply_diff(Record :: model_behaviour:record(), model_behaviour:diff()) ->
    NewRecord :: model_behaviour:record().
apply_diff(Record, Diff) when is_function(Diff) ->
    Diff(Record);

apply_diff(Record, Diff) when is_map(Diff) ->
    [Model | Values] = erlang:tuple_to_list(Record),
    Fields = Model:get_fields(),
    NewValues = lists:map(fun({Field, OldValue}) ->
        maps:get(Field, Diff, OldValue)
    end, lists:zip(Fields, Values)),
    erlang:list_to_tuple([Model | NewValues]).
%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains model management functions.
%%%--------------------------------------------------------------------
-module(model).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").

%% API
-export([table_name/1, get_models/0]).
-export([create/2, save/2, update/3, get/2, exists/2, delete/2, size/1,
    transaction/1]).

-type model() :: atom().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns table name associated with a model.
%%--------------------------------------------------------------------
-spec table_name(Model :: model()) -> Name :: atom().
table_name(Model) ->
    Model.


%%--------------------------------------------------------------------
%% @doc Returns list of supported models.
%%--------------------------------------------------------------------
-spec get_models() -> Models :: [model()].
get_models() ->
    ?MODELS.


%%--------------------------------------------------------------------
%% @doc Creates model instance. Returns an error if instance already exists.
%%--------------------------------------------------------------------
-spec create(Model :: model(), Record :: model_behaviour:record()) ->
    ok | #error{} | no_return().
create(Model, Record) ->
    transaction(fun() ->
        Table = table_name(Model),
        Key = erlang:element(2, Record),
        case mnesia:read(Table, Key) of
            [] -> mnesia:write(Table, Record, write);
            [_ | _] ->
                ?error(?ERR_ALREADY_EXISTS, ?MODULE, create, 1, [Model, Record])
        end
    end).


%%--------------------------------------------------------------------
%% @doc Saves model instance.
%%--------------------------------------------------------------------
-spec save(Model :: model(), Record :: model_behaviour:record()) ->
    ok | no_return().
save(Model, Record) ->
    transaction(fun() ->
        Table = table_name(Model),
        mnesia:write(Table, Record, write)
    end).


%%--------------------------------------------------------------------
%% @doc Updates model instance.
%%--------------------------------------------------------------------
-spec update(Model :: model(), Key :: model_behaviour:key(),
    Diff :: model_behaviour:diff()) -> ok | no_return().
update(Model, Key, Diff) ->
    transaction(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> mnesia:abort(?error(?ERR_NOT_FOUND, ?MODULE, update, 2,
                [Model, Key, Diff]));
            [Record] -> mnesia:write(Table, apply_diff(Record, Diff), write)
        end
    end).


%%--------------------------------------------------------------------
%% @doc Returns model instance.
%%--------------------------------------------------------------------
-spec get(Model :: model(), Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | #error{} | no_return().
get(Model, Key) ->
    transaction(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> ?error(?ERR_NOT_FOUND, ?MODULE, get, 1, [Model, Key]);
            [Record] -> {ok, Record}
        end
    end).


%%--------------------------------------------------------------------
%% @doc Returns 'true' if model instance exists, otherwise 'false'.
%%--------------------------------------------------------------------
-spec exists(Model :: model(), Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Model, Key) ->
    transaction(fun() ->
        Table = table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> false;
            [_ | _] -> true
        end
    end).


%%--------------------------------------------------------------------
%% @doc Deletes model instance.
%%--------------------------------------------------------------------
-spec delete(Model :: model(), Key :: model_behaviour:key()) ->
    ok | no_return().
delete(Model, Key) ->
    transaction(fun() ->
        Table = table_name(Model),
        mnesia:delete(Table, Key, write)
    end).


%%--------------------------------------------------------------------
%% @doc Returns number of model instances.
%%--------------------------------------------------------------------
-spec size(Model :: model()) -> non_neg_integer().
size(Model) ->
    Table = table_name(Model),
    mnesia:table_info(Table, size).


%%--------------------------------------------------------------------
%% @doc Provides transactional environment for operations on models.
%%--------------------------------------------------------------------
-spec transaction(Transaction :: fun()) -> term() | no_return().
transaction(Transaction) ->
    try
        mnesia:activity(transaction, Transaction)
    catch
        _:{aborted, Reason} -> ?throw(Reason);
        _:Reason -> ?throw(Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Modifies model instance according to provided update method.
%%--------------------------------------------------------------------
-spec apply_diff(Record :: model_behaviour:record(), model_behaviour:diff()) ->
    NewRecord :: model_behaviour:record().
apply_diff(Record, Diff) when is_function(Diff) ->
    Diff(Record);

apply_diff(Record, Diff) when is_map(Diff) ->
    [Model | Values] = erlang:tuple_to_list(Record),
    Fields = Model:get_fields(),
    NewValues = lists:map(fun({Field, Value}) ->
        case maps:find(Field, Diff) of
            {ok, NewValue} -> NewValue;
            error -> Value
        end
    end, lists:zip(Fields, Values)),
    erlang:list_to_tuple([Model | NewValues]).
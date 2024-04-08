%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains model management functions.
%%% Each model module defines its schema as a record. In mnesia, those
%%% records are wrapped in a common #document{} format to facilitate
%%% version tracking.
%%% @end
%%%--------------------------------------------------------------------
-module(model).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_table_name/1, get_models/0]).
-export([get_fields/0]).
-export([create/2, save/2, upgrade/2, update/3, get/2, exists/2, delete/2, list/1]).
-export([exists/1, select/2, size/1, clear/1, transaction/1]).

-type doc() :: #document{}.
-type model() :: atom().
-type version() :: model_behaviour:version().
-type key() :: model_behaviour:key().

-export_type([model/0]).

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
%% @doc Returns list of fields in #document record.
%% @end
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?WRAPPER_RECORD).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Model :: model(), Record :: model_behaviour:record()) ->
    {ok, key()} | ?ERR_ALREADY_EXISTS | no_return().
create(Model, Record) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        Key = record_key(Record),
        Doc = record_to_document(Record),
        case mnesia:read(Table, Key) of
            [] ->
                mnesia:write(Table, Doc, write),
                {ok, Key};
            [_ | _] ->
                ?ERR_ALREADY_EXISTS
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
        mnesia:write(Table, record_to_document(Record), write)
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Model :: model(), Key :: key(), Diff :: model_behaviour:diff()) ->
    {ok, model_behaviour:record()} | ?ERR_DOC_NOT_FOUND.
update(Model, Key, Diff) ->
    % @TODO VFS-5272 Handle key change by the diff function
    transaction(fun() ->
        Table = get_table_name(Model),
        case mnesia:read(Table, Key) of
            [] ->
                ?ERR_DOC_NOT_FOUND;
            [Doc] ->
                #document{value = NewRecord} = NewDoc = apply_diff(Doc, Diff),
                ok = mnesia:write(Table, NewDoc, write),
                {ok, NewRecord}
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Model :: model(), Key :: key()) ->
    {ok, Record :: model_behaviour:record()} | ?ERR_DOC_NOT_FOUND | no_return().
get(Model, Key) ->
    transaction(fun() ->
        Table = get_table_name(Model),
        case mnesia:read(Table, Key) of
            [] -> ?ERR_DOC_NOT_FOUND;
            [#document{key = Key, value = Record}] -> {ok, Record}
        end
    end).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Model :: model(), Key :: key()) ->
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
-spec delete(Model :: model(), Key :: key()) ->
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
    select(Model, [{'_', []}]).


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
-spec select(Model :: model(), [MatchSpec]) ->
    Records :: [model_behaviour:record()] | no_return()
    when MatchSpec :: {MatchHead :: tuple() | '_', Guards :: [tuple()]}.
select(Model, MatchSpecs) ->
    DocSpecs = [{#document{value = MatchHead, _ = '_'}, Guards, ['$_']}
        || {MatchHead, Guards} <- MatchSpecs],
    transaction(fun() ->
        Table = get_table_name(Model),
        extract_records(mnesia:select(Table, DocSpecs))
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
-spec clear(Model :: model()) -> ok | {error, _}.
clear(Model) ->
    Table = get_table_name(Model),
    case mnesia:clear_table(Table) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
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
        _:{aborted, Error} -> error(Error);
        _:Error -> error(Error)
    end.


-spec upgrade(ModelName :: model(), CurrentDocument :: tuple()) ->
    NewDocument :: doc().
upgrade(Model, #document{version = CurrentVsn, value = CurrentRecord} = Document) ->
    TargetVsn = Model:get_record_version(),
    NewRecord = upgrade(TargetVsn, CurrentVsn, Model, CurrentRecord),
    Document#document{value = NewRecord, version = TargetVsn};

% old-style record, different for each model
upgrade(Model, Record) ->
    upgrade(Model, record_to_document(Record, 1)).


%% @private
-spec upgrade(TargetVsn :: version(), CurrentVsn :: version(),
    ModelName :: model(), CurrentDocument :: tuple()) ->
    NewDocument :: model_behaviour:record() | no_return().
upgrade(Vsn, Vsn, _Model, Record) ->
    Record;

upgrade(TargetVsn, CurrentVsn, Model, _Record)
    when CurrentVsn > TargetVsn ->
    ?emergency("Upgrade requested for model '~tp' with future version ~tp "
    "(known versions up to: ~tp)", [Model, CurrentVsn, TargetVsn]),
    error({upgrade_from_future, {Model, CurrentVsn, TargetVsn}});

upgrade(TargetVsn, CurrentVsn, Model, Record) ->
    case Model:upgrade(CurrentVsn, Record) of
        {CurrentVsn, _} ->
            ?critical("Upgrade function for model ~ts did not increase version number", [Model]),
            error(bad_upgrade);
        {NewVersion, NewRecord} ->
            upgrade(TargetVsn, NewVersion, Model, NewRecord)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Modifies model instance according to provided update method.
%% @end
%%--------------------------------------------------------------------
-spec apply_diff(Doc :: doc() | model_behaviour:record(), model_behaviour:diff()) ->
    NewDoc :: doc() | model_behaviour:record().
apply_diff(#document{value = Record} = Doc, Diff) ->
    NewRecord = apply_diff(Record, Diff),
    Doc#document{key = record_key(NewRecord), value = NewRecord};

apply_diff(Record, Diff) when is_function(Diff) ->
    Diff(Record);

apply_diff(Record, Diff) when is_map(Diff) ->
    [Model | Values] = erlang:tuple_to_list(Record),
    Fields = Model:get_fields(),
    NewValues = lists:map(fun({Field, OldValue}) ->
        maps:get(Field, Diff, OldValue)
    end, lists:zip(Fields, Values)),
    erlang:list_to_tuple([Model | NewValues]).


%%--------------------------------------------------------------------
%% @private
%% @doc Wraps model record in #document{}.
%% Sets current version for the model.
%% @end
%%--------------------------------------------------------------------
-spec record_to_document(model_behaviour:record()) -> doc().
record_to_document(Record) ->
    Model = erlang:element(1, Record),
    Version = Model:get_record_version(),
    record_to_document(Record, Version).


%%--------------------------------------------------------------------
%% @private
%% @doc Wraps model record in #document{}. Sets given Version.
%% @end
%%--------------------------------------------------------------------
-spec record_to_document(model_behaviour:record(), version()) -> doc().
record_to_document(Record, Version) ->
    Key = record_key(Record),
    #document{key = Key, version = Version, value = Record}.


%%--------------------------------------------------------------------
%% @private
%% @doc Extracts field used as the key from a model record.
%% @end
%%--------------------------------------------------------------------
-spec record_key(model_behaviour:record()) -> key().
record_key(Record) ->
    erlang:element(2, Record).


%% @private
-spec extract_records([doc()]) -> [model_behaviour:record()].
extract_records(Documents) ->
    [Record || #document{value = Record} <- Documents].

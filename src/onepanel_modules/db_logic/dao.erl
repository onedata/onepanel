%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains access functions to Mnesia database.
%% It allows to save, update and retrieve records from database.
%% @end
%% ===================================================================
-module(dao).

-include_lib("ctool/include/logging.hrl").

%% API
-export([save_record/2, update_record/3, get_record/2, exist_record/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% save_record/2
%% ====================================================================
%% @doc Saves record in database table.
%% @end
-spec save_record(Table :: atom(), Record :: record()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
save_record(Table, Record) ->
    try
        Transaction = fun() ->
            case mnesia:write(Table, Record, write) of
                ok -> ok;
                Other -> {error, Other}
            end
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            ?error("Cannot save record ~p in table ~p: ~p", [Record, Table, Reason]),
            {error, Reason}
    end.


%% update_record/2
%% ====================================================================
%% @doc Updates given record fields in database table. Update is a list
%% of tuples, where first element indicates name of a record field to be
%% change and second element is a substitution value. If record does not
%% exist new is created.
%% @end
-spec update_record(Table :: atom(), Key :: term(), Values) -> Result when
    Result :: ok | {error, Reason :: term()},
    Values :: [{Field :: atom(), Value :: term()}].
%% ====================================================================
update_record(Table, Key, Values) ->
    try
        Transaction = fun() ->
            OldRecord = case mnesia:read(Table, Key) of
                            [Record] -> Record;
                            [] -> get_table_record(Table, Key);
                            ReadError -> mnesia:abort(ReadError)
                        end,
            [RecordName | OldValues] = tuple_to_list(OldRecord),
            Attributes = mnesia:table_info(Table, attributes),
            NewValues = lists:map(fun
                ({Column, OldValue}) ->
                    case proplists:get_value(Column, Values, not_found) of
                        not_found -> OldValue;
                        NewValue -> NewValue
                    end
            end, lists:zip(Attributes, OldValues)),
            NewRecord = list_to_tuple([RecordName | NewValues]),
            case mnesia:write(Table, NewRecord, write) of
                ok -> ok;
                WriteError -> mnesia:abort(WriteError)
            end
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            ?error("Cannot update record in table ~p using key ~p and substitution values ~p: ~p", [Table, Key, Values, Reason]),
            {error, Reason}
    end.


%% get_record/2
%% ====================================================================
%% @doc Gets record from database table. Returns error if record was not
%% found.
%% @end
-spec get_record(Table :: atom(), Key :: term()) -> Result when
    Result :: {ok, Record :: record()} | {error, Reason :: term()}.
%% ====================================================================
get_record(Table, Key) ->
    try
        Transaction = fun() ->
            case mnesia:read(Table, Key) of
                [Record] -> {ok, Record};
                [] -> {error, "Record not found."};
                Other -> {error, Other}
            end
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            ?error("Cannot get record from table ~p using key ~p: ~p", [Table, Key, Reason]),
            {error, Reason}
    end.


%% exist_record/2
%% ====================================================================
%% @doc Checks whether record exists in database table.
%% @end
-spec exist_record(Table :: atom(), Key :: term()) -> Result when
    Result :: {ok, Exists :: boolean()} | {error, Reason :: term()}.
%% ====================================================================
exist_record(Table, Key) ->
    try
        Transaction = fun() ->
            case mnesia:read(Table, Key) of
                [_ | _] -> true;
                _ -> false
            end
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            ?error("Cannot check record exists in table ~p using key ~p: ~p", [Table, Key, Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% get_table_record/1
%% ====================================================================
%% @doc Returns default (empty) record for a table with given primary key.
%% @end
-spec get_table_record(Table :: atom(), Key :: term()) -> Result when
    Result :: record().
%% ====================================================================
get_table_record(Table, Key) ->
    Arity = mnesia:table_info(Table, arity),
    RecordName = mnesia:table_info(Table, record_name),
    list_to_tuple([RecordName, Key | lists:duplicate(Arity - 1, undefined)]).
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

-include("onepanel_modules/db_logic.hrl").

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
            lager:error("Cannot save record ~p in table ~p: ~p", [Record, Table, Reason]),
            {error, Reason}
    end.


%% update_record/2
%% ====================================================================
%% @doc Updates given record fields in database table. Update is a list
%% of tuples, where first element indicates name of a record field to be
%% change and second element is a substitution value. If record does not
%% exist in database an error is returned.
%% @end
-spec update_record(Table :: atom(), Key :: term(), Update) -> Result when
    Result :: ok | {error, Reason :: term()},
    Update :: [{Field :: atom(), Value :: term()}].
%% ====================================================================
update_record(Table, Key, Update) ->
    try
        case get_record(Table, Key) of
            {ok, OldRecord} ->
                [RecordName, OldValues] = tuple_to_list(OldRecord),
                Columns = get_table_columns(Table),
                NewValues = lists:map(fun
                    (Column, OldValue) ->
                        case proplists:get_value(Column, Update) of
                            undefined -> OldValue;
                            NewValue -> NewValue
                        end
                end, lists:zip(Columns, OldValues)),
                NewRecord = list_to_tuple([RecordName, NewValues]),
                save_record(Table, NewRecord);
            Other -> Other
        end
    catch
        _:Reason ->
            lager:error("Cannot update record in table ~p using key ~p and update ~p: ~p", [Table, Key, Update, Reason]),
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
                [] -> {error, not_found};
                Other -> {error, Other}
            end
        end,
        mnesia:activity(transaction, Transaction)
    catch
        _:Reason ->
            lager:error("Cannot get record from table ~p using key ~p: ~p", [Table, Key, Reason]),
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
            lager:error("Cannot check record exists in table ~p using key ~p: ~p", [Table, Key, Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% get_table_columns/1
%% ====================================================================
%% @doc Returns list of database table columns as atoms.
%% @end
-spec get_table_columns(Table :: atom()) -> Result when
    Result :: [atom()].
%% ====================================================================
get_table_columns(?USER_TABLE) ->
    record_info(fields, ?USER_TABLE);
get_table_columns(?CONFIG_TABLE) ->
    record_info(fields, ?CONFIG_TABLE);
get_table_columns(?PORT_TABLE) ->
    record_info(fields, ?PORT_TABLE).
%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database access functions
%% @end
%% ===================================================================
-module(dao).

%% API
-export([save_record/2, update_record/2, get_record/2, exist_record/2]).

%% save_record/2
%% ====================================================================
%% @doc Saves record in database table
%% @end
-spec save_record(Table :: atom(), Record :: record()) -> ok | error.
%% ====================================================================
save_record(Table, Record) ->
  Transaction = fun() ->
    case mnesia:write(Table, Record, write) of
      ok -> ok;
      _ -> error
    end
  end,
  mnesia:activity(transaction, Transaction).

%% update_record/2
%% ====================================================================
%% @doc Updates record in database table
%% @end
-spec update_record(Table :: atom(), Record :: record()) -> ok | error.
%% ====================================================================
update_record(Table, NewRecord) ->
  case get_record(Table, element(2, NewRecord)) of
    {ok, OldRecord} ->
      List = lists:map(fun
        ({Old, undefined}) -> Old;
        ({Old, []}) -> Old;
        ({_, New}) -> New
      end, lists:zip(tuple_to_list(OldRecord), tuple_to_list(NewRecord))),
      save_record(Table, list_to_tuple(List));
    _ -> save_record(Table, NewRecord)
  end.

%% get_record/2
%% ====================================================================
%% @doc Gets record from database table
%% @end
-spec get_record(Table :: atom(), Key :: term()) -> {ok, Record :: record()} | not_found | error.
%% ====================================================================
get_record(Table, Key) ->
  Transaction = fun() ->
    case mnesia:read(Table, Key) of
      [Record] -> {ok, Record};
      [] -> not_found;
      _ -> error
    end
  end,
  mnesia:activity(transaction, Transaction).

%% exist_record/2
%% ====================================================================
%% @doc Checks whether record exist in database table
%% @end
-spec exist_record(Table :: atom(), Key :: term()) -> {ok, Record :: record()} | not_found | error.
%% ====================================================================
exist_record(Table, Key) ->
  Transaction = fun() ->
    case mnesia:read(Table, Key) of
      [_] -> true;
      _ -> false
    end
  end,
  mnesia:activity(transaction, Transaction).
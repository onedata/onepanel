%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of lists module functionality.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_lists).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([hd/1, get/2, get/3, store/2, store/3, get_store/3, get_store/4]).
-export([rename/3, remove/2]).
-export([union/2, intersect/2, subtract/2, foldl_while/3, undefined_to_null/1]).

-type key() :: any().
-type keys() :: key() | [key()].
-type value() :: term().
-type terms() :: [tuple()].

-export_type([key/0, keys/0, value/0, terms/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns first element in the lists or if the lists is empty
%% an 'undefined' atom.
%% @end
%%--------------------------------------------------------------------
-spec hd(List :: list()) -> Elem :: undefined | any().
hd([]) -> undefined;
hd([Head | _]) -> Head.

%%--------------------------------------------------------------------
%% @doc Returns a value from the nested proplist.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), Terms :: terms()) -> {ok, Value :: value()} | #error{}.
get([], Terms) ->
    {ok, Terms};

get([Key | Keys], Terms) ->
    case lists:keyfind(Key, 1, Terms) of
        {Key, NewTerms} -> get(Keys, NewTerms);
        false -> ?make_error(?ERR_NOT_FOUND)
    end;

get(Key, Terms) ->
    get([Key], Terms).


%%--------------------------------------------------------------------
%% @doc Returns a value from the nested proplist. If the value is missing
%% the default one is returned.
%% @end
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), Terms :: terms(), Default :: value()) -> Value :: value().
get(Keys, Terms, Default) ->
    case get(Keys, Terms) of
        {ok, Value} -> Value;
        #error{reason = ?ERR_NOT_FOUND} -> Default
    end.


%%--------------------------------------------------------------------
%% @doc @equiv store(Keys, Value, [])
%% @end
%%--------------------------------------------------------------------
-spec store(Keys :: keys(), Value :: value()) -> NewTerms :: terms().
store(Keys, Value) ->
    store(Keys, Value, []).


%%--------------------------------------------------------------------
%% @doc Stores the value in the nested proplist.
%% @end
%%--------------------------------------------------------------------
-spec store(Keys :: keys(), Value :: value(), Terms :: terms()) ->
    NewTerms :: terms().
store([], Value, _Terms) ->
    Value;

store([Key | Keys], Value, Terms) ->
    NewValue = case get(Key, Terms) of
        {ok, NewTerms} -> store(Keys, Value, NewTerms);
        #error{reason = ?ERR_NOT_FOUND} -> store(Keys, Value, [])
    end,
    lists:keystore(Key, 1, Terms, {Key, NewValue});

store(Key, Value, Terms) ->
    store([Key], Value, Terms).


%%--------------------------------------------------------------------
%% @doc Removes value from a nested proplist.
%% @end
%%--------------------------------------------------------------------
-spec remove(Keys :: keys(), Terms :: terms()) -> NewTerms :: terms().
remove(Keys, Terms) when is_list(Keys) ->
    [Key | RevIntermediate] = lists:reverse(Keys),
    % all but last
    Intermediate = lists:reverse(RevIntermediate),
    case get(Intermediate, Terms) of
        {ok, List} when is_list(List) ->
            NewList = lists:keydelete(Key, 1, List),
            store(Intermediate, NewList, Terms);
        _ -> Terms
    end;

remove(Key, Terms) ->
    remove([Key], Terms).


%%--------------------------------------------------------------------
%% @doc Moves value from one location in a nested proplist to another.
%% @end
%%--------------------------------------------------------------------
-spec rename(OldKeys :: keys(), NewKeys :: keys(), Terms :: terms()) ->
    NewTerms :: terms().
rename(OldKeys, NewKeys, Terms) ->
    get_store(OldKeys, Terms, NewKeys, remove(OldKeys, Terms)).


%%--------------------------------------------------------------------
%% @doc @equiv get_store(SrcKeys, SrcTerms, DstKeys, [])
%% @end
%%--------------------------------------------------------------------
-spec get_store(SrcKeys :: keys(), SrcTerms :: terms(), DstKeys :: keys()) ->
    NewTerms :: terms().
get_store(SrcKeys, SrcTerms, DstKeys) ->
    get_store(SrcKeys, SrcTerms, DstKeys, []).


%%--------------------------------------------------------------------
%% @doc Gets a value from the source nested proplist and stores it in
%% the target nested proplist. If the value is not found in the source list
%% returns unchanged target list.
%% @end
%%--------------------------------------------------------------------
-spec get_store(SrcKeys :: keys(), SrcTerms :: terms(), DstKeys :: keys(),
    DstTerms :: terms()) -> NewTerms :: terms().
get_store(SrcKeys, SrcTerms, DstKeys, DstTerms) ->
    case get(SrcKeys, SrcTerms) of
        {ok, Value} -> store(DstKeys, Value, DstTerms);
        #error{reason = ?ERR_NOT_FOUND} -> DstTerms
    end.


%%--------------------------------------------------------------------
%% @doc Returns a union of lists without duplicates.
%% @end
%%--------------------------------------------------------------------
-spec union(List1 :: list(), List2 :: list()) -> List :: list().
union(List1, List2) ->
    ordsets:to_list(ordsets:union(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Returns a list of elements from the List1 that does belong to the
%% List2.
%% @end
%%--------------------------------------------------------------------
-spec intersect(List1 :: list(), List2 :: list()) -> List :: list().
intersect(List1, List2) ->
    ordsets:to_list(ordsets:intersection(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Returns a list of elements from the List1 that does not belong to the
%% List2.
%% @end
%%--------------------------------------------------------------------
-spec subtract(List1 :: list(), List2 :: list()) -> List :: list().
subtract(List1, List2) ->
    ordsets:to_list(ordsets:subtract(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc
%% Foldls the list until Fun returns {halt, Term}.
%% The return value for Fun is expected to be
%% {cont, Acc} to continue the fold with Acc as the new accumulator or
%% {halt, Acc} to halt the fold and return Acc as the return value of this function
%% @end
%%--------------------------------------------------------------------
-spec foldl_while(F, Accu, List) -> Accu1 when
    F :: fun((Elem :: T, AccIn) -> AccOut),
    Accu :: term(), Accu1 :: term(),
    AccIn :: term(), AccOut :: {cont, term()} | {halt, term()},
    List :: [T], T :: term().
foldl_while(F, Accu, List) ->
    do_foldl(F, {cont, Accu}, List).


%% @private
-spec do_foldl(F, AccOut, List) -> AccIn when
    F :: fun((Elem :: T, AccIn) -> AccOut),
    AccIn :: term(), AccOut :: {cont, term()} | {halt, term()},
    List :: [T], T :: term().
do_foldl(_F, {halt, Accu}, _) -> Accu;
do_foldl(_F, {cont, Accu}, []) -> Accu;
do_foldl(F, {cont, Accu}, [Hd|Tail]) -> do_foldl(F, F(Hd, Accu), Tail).


%%-------------------------------------------------------------------
%% @doc
%% This function is responsible for recursive mapping all undefined
%% values in given proplist to null
%% @end
%%-------------------------------------------------------------------
-spec undefined_to_null(Proplist :: proplists:proplist()) ->
    proplists:proplist().
undefined_to_null(Proplist) ->
    lists:map(fun
        ({Key, undefined}) ->
            {Key, null};
        ({Key, SubProplist}) when is_list(SubProplist) ->
            Map2 = undefined_to_null(SubProplist),
            {Key, Map2};
        ({Key, Value}) ->
            {Key, Value}
    end, Proplist).
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
-export([hd/1]).
-export([union/1, union/2, intersect/2, subtract/2, foldl_while/3]).
-export([ensure_length/2]).

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
%% @doc @equiv union([List1, List2])
%% @end
%%--------------------------------------------------------------------
-spec union(List1 :: [T], List2 :: [T]) -> [T].
union(List1, List2) ->
    union([List1, List2]).

%%--------------------------------------------------------------------
%% @doc Returns a union of lists without duplicates.
%% @end
%%--------------------------------------------------------------------
-spec union(ListOfLists :: [list(T)]) -> list(T).
union(ListOfLists) ->
    Union = ordsets:union(
        lists:map(fun ordsets:from_list/1, ListOfLists)
    ),
    ordsets:to_list(Union).


%%--------------------------------------------------------------------
%% @doc Returns a list of elements from the List1 that does belong to the
%% List2.
%% @end
%%--------------------------------------------------------------------
-spec intersect(List1 :: [T], List2 :: [T]) -> List :: [T].
intersect(List1, List2) ->
    ordsets:to_list(ordsets:intersection(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Returns a list of elements from the List1 that does not belong to the
%% List2.
%% @end
%%--------------------------------------------------------------------
-spec subtract(List1 :: [T], List2 :: [T]) -> List :: [T].
subtract(List1, List2) ->
    ordsets:to_list(ordsets:subtract(
        ordsets:from_list(List1), ordsets:from_list(List2)
    )).


%%--------------------------------------------------------------------
%% @doc Shortens or duplicates list to ensure exact number of elements
%% (unless given list is empty). For example:
%% (3, [a, b]) -> [a, b, a]
%% (1, [a, b]) -> [a]
%% (9, []) -> []
%% @end
%%--------------------------------------------------------------------
-spec ensure_length(TargetLength :: non_neg_integer(), List :: [X]) -> [X].
ensure_length(_, []) -> [];
ensure_length(TargetLength, List) ->
    Repeats = utils:ceil(TargetLength / length(List)),
    lists:sublist(lists:append(lists:duplicate(Repeats, List)), TargetLength).


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
do_foldl(F, {cont, Accu}, [Hd | Tail]) -> do_foldl(F, F(Hd, Accu), Tail).

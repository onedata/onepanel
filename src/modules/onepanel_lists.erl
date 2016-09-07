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
-export([get/2, get/3, store/2, store/3, get_store/3, get_store/4]).

-type key() :: any().
-type keys() :: key() | [key()].
-type value() :: term().
-type terms() :: [tuple()].

-export_type([key/0, keys/0, value/0, terms/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns a value from the nested property list.
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
%% @doc Returns a value from the nested property list. If the value is missing
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
%% @doc Stores the value in the nested property list.
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
%% @doc @equiv get_store(SrcKeys, SrcTerms, DstKeys, [])
%% @end
%%--------------------------------------------------------------------
-spec get_store(SrcKeys :: keys(), SrcTerms :: terms(), DstKeys :: keys()) ->
    NewTerms :: terms().
get_store(SrcKeys, SrcTerms, DstKeys) ->
    get_store(SrcKeys, SrcTerms, DstKeys, []).


%%--------------------------------------------------------------------
%% @doc Gets a value from the source nested property list and stores it in
%% the target nested property list. If the value is not found in the source list
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
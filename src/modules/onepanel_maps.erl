%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of maps module functionality.
%%%--------------------------------------------------------------------
-module(onepanel_maps).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include("names.hrl").

%% API
-export([get/2, get/3, store/3, store/4]).

-type key() :: any().
-type keys() :: key() | [key()].
-type maps() :: #{}.

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv get(Keys, Map, ?error(?ERR_NOT_FOUND))
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), Map :: maps()) -> Value :: term() | #error{}.
get(Keys, Map) ->
    get(Keys, Map, ?error(?ERR_NOT_FOUND)).


%%--------------------------------------------------------------------
%% @doc Returns a value from a nested map.
%%--------------------------------------------------------------------
-spec get(Keys :: keys(), Map :: maps(), Default :: term()) -> Value :: term().
get([Key], Map, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end;

get([Key | Keys], Map, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, NewMap} -> get(Keys, NewMap);
        error -> Default
    end;

get(Keys, _Map, Default) when is_list(Keys) ->
    Default;

get(Key, Map, Default) ->
    get([Key], Map, Default).


%%--------------------------------------------------------------------
%% @doc @equiv store(Key, Keys, SourceMap, #{})
%%--------------------------------------------------------------------
-spec store(Key :: key(), Keys :: keys(), SourceMap :: maps()) -> DestMap :: maps().
store(Key, Keys, SourceMap) ->
    store(Key, Keys, SourceMap, #{}).


%%--------------------------------------------------------------------
%% @doc Gets value from a nested maps and stores it in a provided map under
%% given key.
%% @end
%%--------------------------------------------------------------------
-spec store(Key :: key(), Keys :: keys(), SourceMap :: maps(), DestMap :: maps()) ->
    Map :: maps().
store(Key, Keys, SourceMap, DestMap) ->
    case get(Keys, SourceMap) of
        #error{reason = ?ERR_NOT_FOUND} -> DestMap;
        Value -> maps:put(Key, Value, DestMap)
    end.
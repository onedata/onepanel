%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an extension of maps module functionality.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_maps).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([merge/1]).
-export([remove_undefined/1, undefined_to_null/1]).

-type key() :: any().
-type keys() :: key() | [key()].
-type value() :: term().
-type terms() :: map().

-export_type([key/0, keys/0, value/0, terms/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Removes undefined values from the map.
%% @end
%%--------------------------------------------------------------------
-spec remove_undefined(#{K => V | undefined}) -> #{K => V}.
remove_undefined(Map) ->
    maps:filter(fun
        (_Key, undefined) -> false;
        (_Key, _Value) -> true
    end, Map).


%%--------------------------------------------------------------------
%% @doc Recursively replaces 'undefined' with 'null'.
%% @end
%%--------------------------------------------------------------------
-spec undefined_to_null(Terms :: map()) -> map().
undefined_to_null(Map) ->
    maps:map(fun
        (_Key, undefined) -> null;
        (_Key, SubMap) when is_map(SubMap) -> undefined_to_null(SubMap);
        (_Key, Value) -> Value
    end, Map).


%%-------------------------------------------------------------------
%% @doc Merges a list of maps. Rightmost maps take precedence
%% in case of repeated keys.
%% @end
%%-------------------------------------------------------------------
-spec merge
    ([]) -> #{};
    ([#{Key => Value}, ...]) -> #{Key => Value}.
merge(ListOfMaps) ->
    lists:foldr(fun maps:merge/2, #{}, ListOfMaps).

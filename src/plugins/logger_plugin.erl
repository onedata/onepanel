%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(logger_plugin).
-author("Krzysztof Trzepla").

-behaviour(logger_plugin_behaviour).

%% logger_plugin callbacks
-export([gather_metadata/0]).

%%%===================================================================
%%% logger_plugin callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Should return a list of key, value tuples to be concatenated
%% to standard log metadata.
%% @end
%%--------------------------------------------------------------------
-spec gather_metadata() -> list().
gather_metadata() ->
    [].
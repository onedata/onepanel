%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module implements logger_plugin_behaviour.
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
%% @doc {@link logger_plugin_behaviour:gather_metadata/0}
%% @end
%%--------------------------------------------------------------------
-spec gather_metadata() -> [{Key :: term(), Value :: term()}].
gather_metadata() ->
    [].
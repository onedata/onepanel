%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides functions for making rpc calls to Onepanel Nodes.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_test_rpc).
-author("Piotr Duleba").

-include_lib("ctool/include/test/test_utils.hrl").

%% API
-export([
    is_set_emergency_passphrase/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================



is_set_emergency_passphrase(ServicePlaceholderOrId) ->
    ?assertMatch(_, call_panel_node(ServicePlaceholderOrId, emergency_passphrase, is_set, [])).





%%%===================================================================
%%% Helper functions
%%%===================================================================


call_panel_node(PlaceholderOrId, Module, Function, Args) ->
    Placeholder = oct_background:to_entity_placeholder(PlaceholderOrId),
    case Placeholder of
        zone ->
            ZonePanelNodes = oct_background:get_zone_panels(),
            rpc:call(lists_utils:random_element(ZonePanelNodes), Module, Function, Args);
        _ ->
            ProviderPanelNodes = oct_background:get_provider_panels(Placeholder),
            rpc:call(lists_utils:random_element(ProviderPanelNodes), Module, Function, Args)
    end.


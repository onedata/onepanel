%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2024 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility generic functions tests.
%%% @end
%%%--------------------------------------------------------------------
-module(panel_test_utils).
-author("Bartosz Walkowicz").

-include("cert_test_utils.hrl").
-include("names.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("kernel/src/inet_dns.hrl").


%% API
-export([
    get_panel_nodes/1,
    get_worker_nodes/1
]).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_panel_nodes(oct_background:entity_selector()) -> [node()].
get_panel_nodes(zone) -> oct_background:get_zone_panels();
get_panel_nodes(ProviderSelector) -> oct_background:get_provider_panels(ProviderSelector).


-spec get_worker_nodes(oct_background:entity_selector()) -> [node()].
get_worker_nodes(zone) -> oct_background:get_zone_nodes();
get_worker_nodes(ProviderSelector) -> oct_background:get_provider_nodes(ProviderSelector).

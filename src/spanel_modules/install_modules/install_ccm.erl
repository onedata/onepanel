%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains ccm installation functions
%% @end
%% ===================================================================
-module(install_ccm).

-include("spanel_modules/install_common.hrl").
-include("spanel_modules/db_logic.hrl").

%% API
-export([install_ccm_node/4, install_ccm_nodes/1]).

install_ccm_node(CCM, CCMs, Databases, Storages) ->
  ok.

install_ccm_nodes(#configuration{ccm = CCM, ccms = CCMs, databases = Databases, storages = Storages}) ->
  install_utils:apply_on_nodes(CCMs, ?MODULE, install_ccm_node, [CCM, CCMs, Databases, Storages], ?RPC_TIMEOUT).
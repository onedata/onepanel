%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains worker installation functions
%% @end
%% ===================================================================
-module(install_worker).

-include("spanel_modules/install_common.hrl").
-include("spanel_modules/db_logic.hrl").

%% API
-export([install_worker_node/4, install_worker_nodes/1]).

install_worker_node(CCM, CCMs, Databases, Storages) ->
  ok.

install_worker_nodes(#configuration{ccm = CCM, ccms = CCMs, workers = Workers, databases = Databases, storages = Storages}) ->
  install_utils:apply_on_nodes(Workers, ?MODULE, install_worker_node, [CCM, CCMs, Databases, Storages], ?RPC_TIMEOUT).
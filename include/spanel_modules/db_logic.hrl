%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains common macros and records for dao module
%% @end
%% ===================================================================

-ifndef(DB_LOGIC_HRL).
-define(DB_LOGIC_HRL, 1).

-record(user, {username, password}).
-record(configuration, {id, ccm, ccms, workers, databases, storages}).

-endif.
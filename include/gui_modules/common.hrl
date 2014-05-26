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

-ifndef(COMMON_HRL).
-define(COMMON_HRL, 1).

-include_lib("n2o/include/wf.hrl").
-include("registered_names.hrl").
-include("custom_elements.hrl").

-define(GEN_SERVER_TIMEOUT, 5000).
-define(MIN_PASSWORD_LENGTH, 8).

-endif.

%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros and records for
%% database management module.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_DB_LOGIC_HRL).
-define(ONEPANEL_DB_LOGIC_HRL, 1).

-include("registered_names.hrl").
-include("onepanel_modules/logic/user_logic.hrl").
-include("onepanel_modules/logic/provider_logic.hrl").
-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/updater/state.hrl").

-ifdef(oneprovider).

-define(TABLES, [
    {?USER_TABLE, ?USER_RECORD, record_info(fields, ?USER_RECORD)},
    {?PROVIDER_TABLE, ?PROVIDER_RECORD, record_info(fields, ?PROVIDER_RECORD)},
    {?GLOBAL_CONFIG_TABLE, ?GLOBAL_CONFIG_RECORD, record_info(fields, ?GLOBAL_CONFIG_RECORD)},
    {?LOCAL_CONFIG_TABLE, ?LOCAL_CONFIG_RECORD, record_info(fields, ?LOCAL_CONFIG_RECORD)},
    {?UPDATER_STATE_TABLE, ?U_STATE, record_info(fields, ?U_STATE)}
]).

-endif.

-ifdef(globalregistry).

-define(TABLES, [
    {?USER_TABLE, ?USER_RECORD, record_info(fields, ?USER_RECORD)},
    {?GLOBAL_CONFIG_TABLE, ?GLOBAL_CONFIG_RECORD, record_info(fields, ?GLOBAL_CONFIG_RECORD)},
    {?LOCAL_CONFIG_TABLE, ?LOCAL_CONFIG_RECORD, record_info(fields, ?LOCAL_CONFIG_RECORD)}
]).

-endif.

-endif.
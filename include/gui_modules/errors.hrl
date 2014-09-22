%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This header file contains common macros for web pages errors.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_GUI_ERRORS_HRL).
-define(ONEPANEL_GUI_ERRORS_HRL, 1).

-define(AUTHENTICATION_ERROR, <<"authentication_error">>).
-define(INTERNAL_SERVER_ERROR, <<"internal_server_error">>).

-ifdef(provider).

-define(SOFTWARE_NOT_INSTALLED_ERROR, <<"software_not_installed_error">>).
-define(UNREGISTERED_PROVIDER_ERROR, <<"unregistered_provider_error">>).
-define(SPACE_PERMISSION_DENIED_ERROR, <<"space_permission_denied_error">>).
-define(SPACE_NOT_FOUND_ERROR, <<"space_not_found_error">>).

-endif.

-endif.

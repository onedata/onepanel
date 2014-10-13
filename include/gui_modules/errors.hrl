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

-define(INTERNAL_SERVER_ERROR, <<"internal_server_error">>).
-define(AUTHENTICATION_ERROR, <<"authentication_error">>).

-define(COMMON_ERROR_MESSAGES, [
    {
        ?INTERNAL_SERVER_ERROR,
        {
            <<"Internal server error">>,
            <<"Server encountered an unexpected error. Please contact the site administrator if the problem persists.">>
        }
    },
    {
        ?AUTHENTICATION_ERROR,
        {
            <<"Authentication error">>,
            <<"Server could not authenticate you. Please try again to log in or contact the site administrator if the problem persists.">>
        }
    }
]).

-ifdef(oneprovider).

-define(SOFTWARE_NOT_INSTALLED_ERROR, <<"software_not_installed_error">>).
-define(UNREGISTERED_PROVIDER_ERROR, <<"unregistered_provider_error">>).
-define(SPACE_PERMISSION_DENIED_ERROR, <<"space_permission_denied_error">>).
-define(SPACE_NOT_FOUND_ERROR, <<"space_not_found_error">>).

-define(ERROR_MESSAGES, [
    {
        ?SOFTWARE_NOT_INSTALLED_ERROR,
        {
            <<"Software is not installed">>,
            <<"Please complete software installation process.">>
        }
    },
    {
        ?UNREGISTERED_PROVIDER_ERROR,
        {
            <<"Unregistered provider">>,
            <<"Please complete registration process in <i>Global Registry</i>.">>
        }
    },
    {
        ?SPACE_PERMISSION_DENIED_ERROR,
        {
            <<"Permission denied">>,
            <<"You don't have permission to manage this Space.">>
        }
    },
    {
        ?SPACE_NOT_FOUND_ERROR,
        {
            <<"Space not found">>,
            <<"Requested Space could not be found on the server.">>
        }
    } | ?COMMON_ERROR_MESSAGES
]).

-endif.

-ifdef(globalregistry).

-define(ERROR_MESSAGES, ?COMMON_ERROR_MESSAGES).

-endif.

-endif.

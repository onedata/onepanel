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

-ifndef(ERRORS_HRL).
-define(ERRORS_HRL, 1).

-define(AUTHENTICATION_ERROR, "Invaild username or password.").
-define(INTERNAL_ERROR, "Internal server error. Please try again.").
-define(PASSWORDS_DONT_MATCH, "Passwords do not match.").
-define(PASSWORD_TO_SHORT, "Password is to short. Should have at least 8 characters.").

-endif.

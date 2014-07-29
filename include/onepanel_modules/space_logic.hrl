%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This header file contains common macros and records for
%% Spaces management modules.
%% @end
%% ===================================================================

-ifndef(ONEPANEL_SPACE_LOGIC_HRL).
-define(ONEPANEL_SPACE_LOGIC_HRL, 1).

%% Names of records
-define(SPACE_DETAILS, space_details).
-define(PROVIDER_DETAILS, provider_details).
-define(USER_DETAILS, user_details).

%% Names of database records
-define(PROVIDER_RECORD, ?PROVIDER_DETAILS).

%% Names of database tables
-define(PROVIDER_TABLE, providers).

%% Space details record contains following fields:
%% * id     - unique Space ID assigned by Global Registry
%% * name   - Space name
-record(?SPACE_DETAILS, {spaceId, name}).

%% Provider details record contains following fields:
%% * id                 - unique provider ID assigned by Global Registry
%% * urls               - URL addresses of all VeilCluster nodes
%% * redirectionPoint   - URL address where VeilCluster GUI is available of all VeilCluster nodes
-record(?PROVIDER_DETAILS, {providerId, urls, redirectionPoint}).

%% User details record contains following fields:
%% * id     - unique user ID assigned by Global Registry
%% * name   - username
-record(?USER_DETAILS, {userId, name}).

-endif.
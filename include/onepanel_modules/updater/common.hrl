%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-author("Rafal Slota").

-ifndef(UPDATER_COMMON_HRL).
-define(UPDATER_COMMON_HRL, 1).

-define(PACKAGE_REPOSITORY_BASE_URL, "http://onedata.org/repository").

-record(version, {major = 0, minor = 0, patch = 0}).

-record(package, {type = rpm, binary = <<>>}).

-endif.
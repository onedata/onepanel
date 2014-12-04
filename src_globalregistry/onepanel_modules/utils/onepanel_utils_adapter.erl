%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility onepanel functions.
%% @end
%% ===================================================================
-module(onepanel_utils_adapter).

-include("registered_names.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([get_application_ports/0]).


%% ====================================================================
%% API functions
%% ====================================================================

%% get_application_ports/1
%% ====================================================================
%% @doc Returns ports used by application.
-spec get_application_ports() -> Result when
    Result :: [Port :: integer()].
%% ====================================================================
get_application_ports() ->
    {ok, ReleasesDir} = application:get_env(?APP_NAME, application_default_config_dir),
    {ok, [Config]} = file:consult(filename:join([ReleasesDir, "app.config"])),
    GlobalRegistryConfig = proplists:get_value(globalregistry, Config, []),
    Ports = lists:map(fun(Port) ->
        proplists:get_value(Port, GlobalRegistryConfig)
    end, proplists:get_value(ports_in_use, GlobalRegistryConfig, [])),
    lists:usort(Ports).
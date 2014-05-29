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
-module(updater_repos).
-author("Rafal Slota").

-include("spanel_modules/updater_module/common.hrl").

%% API
-export([get_package/1]).

%% ====================================================================
%% API functions
%% ====================================================================

get_package(#version{major = MJ, minor = MI, patch = PA} = Version) ->
    case httpc:request(get, {"http://onedata.org/repository/VeilCluster-Linux-" ++ integer_to_list(MJ) ++ "." ++ integer_to_list(MI) ++ "." ++ integer_to_list(PA) ++ ".rpm", []}, [{timeout, 10000}], [{body_format, binary}, {full_result, false}]) of
        {ok, {200, Binary}} ->
            #package{type = rpm, binary = Binary};
        {ok, {Status, _}} ->
            {error, {invalid_http, Status}};
        {error, Reason} ->
            {error, Reason}
    end.


-spec list_packages(URL :: string()) -> [{URI :: string(), #version{}}].
list_packages(URL) ->
    [].


%% ====================================================================
%% Internal functions
%% ====================================================================

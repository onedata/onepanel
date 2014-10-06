%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module gives access to oneproviders package repository.
%% @end
%% ===================================================================
-module(updater_repos).
-author("Rafal Slota").

-include("registered_names.hrl").
-include("onepanel_modules/updater/common.hrl").

%% API
-export([get_package/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% get_package/1
%% ====================================================================
%% @doc Downloads oneprovider's RPM package for given version.
%% @end
-spec get_package(Version :: #version{}) -> {ok, #package{}} | {error, any()}.
%% ====================================================================
get_package(#version{} = Version) ->
    case httpc:request(get, {gen_package_url(Version, rpm), []}, [{timeout, 10000}], [{body_format, binary}, {full_result, false}]) of
        {ok, {200, Binary}} ->
            {ok, #package{type = rpm, binary = Binary}};
        {ok, {Status, _}} ->
            {error, {invalid_http, Status}};
        {error, Reason} ->
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


%% gen_package_url/2
%% ====================================================================
%% @doc Returns package's URL based on given version and package type.
%% @end
-spec gen_package_url(Version :: #version{}, PackageType :: rpm | deb) -> {ok, #package{}} | {error, any()}.
%% ====================================================================
gen_package_url(#version{major = MJ, minor = MI, patch = PA} = _Version, PackageType) ->
    {ok, URL} = application:get_env(?APP_NAME, onedata_repository_url),
    URL ++ "/oneprovider-Linux-" ++ integer_to_list(MJ) ++ "." ++ integer_to_list(MI) ++ "." ++ integer_to_list(PA) ++ "." ++ atom_to_list(PackageType).
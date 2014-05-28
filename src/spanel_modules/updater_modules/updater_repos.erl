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
-export([]).

%% ====================================================================
%% API functions
%% ====================================================================

get_package(#version{} = Version) ->
    #package{}.

install_package(#package{type = rpm, binary = Bin}) ->
    ok.

install_package(Node, #package{type = rpm, binary = Bin}) ->
    ok;
install_package(Node, #package{type = Type}) ->
    lager:warning("Unsupported package type: ~p", [Type]),
    {error, unsupported_package}.

-spec list_packages(URL :: string()) -> [{URI :: string(), #version{}}].
list_packages(URL) ->
    [].


%% ====================================================================
%% Internal functions
%% ====================================================================

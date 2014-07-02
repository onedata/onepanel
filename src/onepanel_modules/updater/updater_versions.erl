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
-module(updater_versions).
-author("Rafal Slota").

-include("onepanel_modules/updater/common.hrl").

%% API
-export([get_current/0, get_newest/0, cmp/2]).

%% ====================================================================
%% API functions
%% ====================================================================

get_current() ->
    #version{}.

get_newest() ->
    #version{}.

%% Yea...
%% cmp(#version{} = A, #version{} = A) ->
%%     0;
%% cmp(#version{} = A, #version{} = B) when A < B ->
%%     -1;
%% cmp(#version{}, #version{}) ->
%%     1.


cmp(#version{major = MJ1}, #version{major = MJ2}) when
    MJ1 < MJ2 -> -1;
cmp(#version{major = MJ1}, #version{major = MJ2}) when
    MJ1 > MJ2 -> 1;
cmp(#version{minor = MI1}, #version{minor = MI2}) when
    MI1 < MI2 -> -1;
cmp(#version{minor = MI1}, #version{minor = MI2}) when
    MI1 > MI2 -> 1;
cmp(#version{patch = P1}, #version{patch = P2}) when
    P1 < P2 -> -1;
cmp(#version{patch = P1}, #version{patch = P2}) when
    P1 > P2 -> 1;
cmp(#version{}, #version{}) ->
    0.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2013, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: Write me !
%% @end
%% ===================================================================
-module(updater).
-author("Rafal Slota").

-include("registered_names.hrl").
-include("spanel_modules/updater_module/common.hrl").

%% API
-export([start/0, get_state/0, update_to/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    case global:whereis_name(?UPDATE_SERVICE) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            {ok, _Pid} =
                supervisor:start_child(?SERVER_SUP,
                {
                    updater,
                    {updater_worker, start_link, []},
                    transient,
                    10000,
                    worker,
                    [updater_worker]
                }),
            ok
    end.


get_state() ->
    gen_server:call({global, ?UPDATE_SERVICE}, get_state).

update_to(#version{} = Vsn) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {update_to, Vsn}).


%% ====================================================================
%% Internal functions
%% ====================================================================

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
-include("onepanel_modules/updater/common.hrl").

%% API
-export([start/0, get_state/0, update_to/1, update_to/2, is_abortable/0, abort/0]).

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

update_to(#version{} = Vsn) ->
    update_to(Vsn, fun(Event, _State) -> ok end).

%% CallbackFun(Event :: enter_stage | atom(), State :: #u_state{})
update_to(#version{} = Vsn, CallbackFun) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {update_to, Vsn, CallbackFun}).

get_state() ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, get_state).

abort() ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, abort).


is_abortable() ->
    start(),
    updater_state:is_abortable( gen_server:call({global, ?UPDATE_SERVICE}, get_state) ) .


%% ====================================================================
%% Internal functions
%% ====================================================================

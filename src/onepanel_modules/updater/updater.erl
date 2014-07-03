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
-include_lib("ctool/include/logging.hrl").

%% API
-export([start/0, get_state/0, update_to/1, update_to/2, update_to/3, is_abortable/0, abort/0, set_callback/1]).

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
    update_to(#version{} = Vsn, false).
update_to(#version{} = Vsn, ForceNodeReboot) ->
    update_to(Vsn, ForceNodeReboot, fun(_Event, _State) -> ok end).

%% CallbackFun(Event :: enter_stage | update_objects | rollback_stage | error | atom(), State :: #u_state{})
update_to(#version{} = Vsn, ForceNodeReboot, CallbackFun) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {update_to, Vsn, ForceNodeReboot, CallbackFun}).


set_callback(Fun) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {set_callback, Fun}).

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

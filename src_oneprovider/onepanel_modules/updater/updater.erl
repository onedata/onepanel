%% ===================================================================
%% @author Rafal Slota
%% @copyright (C): 2014, ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc Updater service entry point (main API).
%% @end
%% ===================================================================
-module(updater).
-author("Rafal Slota").

-include("registered_names.hrl").
-include("onepanel_modules/updater/state.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start/0, get_state/0, update_to/1, update_to/2, update_to/3, is_abortable/0, abort/0, set_callback/1]).

%% ====================================================================
%% API functions
%% ====================================================================


%% start/0
%% ====================================================================
%% @doc Asynchronously starts updater gen_server or just ensures that it's already running.
%% @end
-spec start() -> ok.
%% ====================================================================
start() ->
    case global:whereis_name(?UPDATE_SERVICE) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            supervisor:delete_child(?ONEPANEL_SUP, updater),
            {ok, _Pid} =
                supervisor:start_child(?ONEPANEL_SUP,
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


%% update_to/1
%% ====================================================================
%% @doc Starts update process for specified oneprovider version.
%% @end
-spec update_to(Version :: #version{}) -> ok | {error, update_already_in_progress} | {error, any()}.
%% ====================================================================
update_to(#version{} = Vsn) ->
    update_to(#version{} = Vsn, false).


%% update_to/2
%% ====================================================================
%% @doc Starts update process for specified oneprovider version.
%%      Allows to force nodes reload after update.
%% @end
-spec update_to(Version :: #version{}, ForceNodeReboot :: boolean()) -> ok | {error, update_already_in_progress} | {error, any()}.
%% ====================================================================
update_to(#version{} = Vsn, ForceNodeReboot) ->
    update_to(Vsn, ForceNodeReboot, fun(_Event, _State) -> ok end).

%% CallbackFun(Event :: enter_stage | update_objects | rollback_stage | error | atom(), State :: #u_state{})

%% update_to/3
%% ====================================================================
%% @doc Same as update_to/2, but also allow to provide callback function.
%%      CallbackFun :: function(Event :: enter_stage | update_objects | rollback_stage | error | warning | abort, State :: #u_state{})
%% @end
-spec update_to(Version :: #version{}, ForceNodeReboot :: boolean(), CallbackFun :: function()) ->
    ok | {error, update_already_in_progress} | {error, any()}.
%% ====================================================================
update_to(#version{} = Vsn, ForceNodeReboot, CallbackFun) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {update_to, Vsn, ForceNodeReboot, CallbackFun}).


%% set_callback/1
%% ====================================================================
%% @doc Sets callback function for current update process.
%%      CallbackFun - see update_to/3
%% @end
-spec set_callback(CallbackFun :: function()) -> ok.
%% ====================================================================
set_callback(Fun) when is_function(Fun) ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, {set_callback, Fun}).


%% get_state/0
%% ====================================================================
%% @doc Returns current state of updater service. See 'updater_state' module for state manipulation functions.
%% @end
-spec get_state() -> #?U_STATE{}.
%% ====================================================================
get_state() ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, get_state).


%% abort/0
%% ====================================================================
%% @doc Stops current update process if possible.
%% @end
-spec abort() -> ok | {error, any()}.
%% ====================================================================
abort() ->
    start(),
    gen_server:call({global, ?UPDATE_SERVICE}, abort).


%% is_abortable/0
%% ====================================================================
%% @doc Checks if abort is possible at this moment.
%% @end
-spec is_abortable() -> ok | {error, any()}.
%% ====================================================================
is_abortable() ->
    start(),
    updater_state:is_abortable(gen_server:call({global, ?UPDATE_SERVICE}, get_state)).


%% ====================================================================
%% Internal functions
%% ====================================================================

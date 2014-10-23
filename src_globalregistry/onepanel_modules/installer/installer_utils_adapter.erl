%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains utility installation functions for
%% Global Registry.
%% @end
%% ===================================================================
-module(installer_utils_adapter).

-include("onepanel_modules/installer/state.hrl").
-include("onepanel_modules/installer/internals.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([finalize_installation/1]).

%% Defines how many times onepanel will try to verify software start
-define(FINALIZE_INSTALLATION_ATTEMPTS, 120).

%% Defines how long onepanel will wait before next attempt to verify software start
-define(NEXT_ATTEMPT_DELAY, 1000).

%% ====================================================================
%% API functions
%% ====================================================================

%% finalize_installation/1
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
%% Returns an error after ?FINALIZE_INSTALLATION_ATTEMPTS limit.
%% @end
-spec finalize_installation(Args :: [{Name :: atom(), Value :: term()}]) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
finalize_installation(_Args) ->
    try
        {ok, #?GLOBAL_CONFIG_RECORD{gr = GR}} = dao:get_record(?GLOBAL_CONFIG_TABLE, ?CONFIG_ID),
        GRNode = onepanel_utils:get_node(?GLOBALREGISTRY_NAME, GR),
        ok = installer_utils:set_timestamp(),
        ok = finalize_installation_loop(GRNode, ?FINALIZE_INSTALLATION_ATTEMPTS),
        ok
    catch
        _:Reason ->
            ?error("Cannot finalize installation: ~p", [Reason]),
            {error, Reason}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% finalize_installation/2
%% ====================================================================
%% @doc Waits until cluster control panel nodes are up and running.
%% Returns an error after ?FINALIZE_INSTALLATION_ATTEMPTS limit.
%% Should not be used directly, use finalize_installation/1 instead.
%% @end
-spec finalize_installation_loop(GRNode :: node(), Attempts :: integer()) -> Result when
    Result :: ok | {error, Reason :: term()}.
%% ====================================================================
finalize_installation_loop(_, 0) ->
    ?error("Cannot finalize installation: attempts limit exceeded."),
    {error, <<"Attempts limit exceeded.">>};

finalize_installation_loop(GRNode, Attempts) ->
    try
        {started, _} = rpc:call(GRNode, init, get_status, []),
        ok
    catch
        _:_ ->
            timer:sleep(?NEXT_ATTEMPT_DELAY),
            finalize_installation_loop(GRNode, Attempts - 1)
    end.
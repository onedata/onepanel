%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for interacting with cluster
%%% services via shell commands.
%%% @end
%%%--------------------------------------------------------------------
-module(service_cli).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([start/2, stop/1, restart/1, status/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Sets the system limits and starts the service using an init script
%% in a shell.
%% @end
%%--------------------------------------------------------------------
-spec start(service:name(), map()) -> ok | no_return().
start(Service, SystemLimits) ->
    Tokens = [get_script(Service), "start"],
    Tokens2 = maps:fold(fun
        (open_files, Value, Acc) -> ["ulimit", "-n", Value, ";" | Acc];
        (_, _, Acc) -> Acc
    end, Tokens, SystemLimits),
    shell_utils:ensure_success(Tokens2),
    ?info("Service ~ts started", [Service]).


%%--------------------------------------------------------------------
%% @doc Stops the service using an init script in a shell.
%% @end
%%--------------------------------------------------------------------
-spec stop(service:name()) -> ok | no_return().
stop(Service) ->
    Tokens = [get_script(Service), "stop"],
    case shell_utils:execute(Tokens) of
        {0, _, _} ->
            ?info("Service ~ts stopped", [Service]),
            ok;
        {Code, Output, StdErr} ->
            ?warning("Failed to stop service '~ts' because of~n~tp ~ts~n~ts",
                [Service, Code, Output, StdErr]),
            ok
    end.


%%--------------------------------------------------------------------
%% @doc Restarts the service
%% @end
%%--------------------------------------------------------------------
-spec restart(service:name()) -> ok | no_return().
restart(Service) ->
    Tokens = [get_script(Service), "restart"],
    shell_utils:ensure_success(Tokens).


%%--------------------------------------------------------------------
%% @doc Checks service status using given command.
%% Services started with custom binary paths require the 'ping'
%% command, otherwise 'status' can be used.
%% @end
%%--------------------------------------------------------------------
-spec status(service:name(), Command :: status | ping) -> running | stopped | missing.
status(Service, Command) ->
    Tokens = [get_script(Service), Command],
    case shell_utils:execute(Tokens) of
        {0, _, _} -> running;
        {127, _, _} -> missing;
        {_, _, _} -> stopped
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns command used for managing given service.
%% Relies on the command being present in app config in variable named
%% according to convention servicename_cmd.
%% @end
%%--------------------------------------------------------------------
-spec get_script(Service :: service:name()) ->
    shell_utils:token() | no_return().
get_script(Service) ->
    EnvName = list_to_atom(atom_to_list(Service) ++ "_cmd"),
    onepanel_env:get(EnvName).

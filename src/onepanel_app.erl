%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module is responsible for starting and stopping onepanel
%%% application.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_app).
-author("Krzysztof Trzepla").

-behaviour(application).

-include_lib("ctool/include/logging.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc This function is called whenever an application is started
%% using application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    try
        test_node_starter:maybe_start_cover(),
        Supervisor = onepanel_sup:start_link(),
        resume_service(),
        Supervisor
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start onepanel application due to: ~p",
                [Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @private @doc This function is called whenever an application has stopped.
%% It is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> term().
stop(_State) ->
    rest_listener:stop(),
    test_node_starter:maybe_stop_cover(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Restart oneprovider or onezone service if its already configured.
%% @end
%%--------------------------------------------------------------------
-spec resume_service() -> ok.
resume_service() ->
    case {service:exists(service_oneprovider:name()), service:exists(service_onezone:name())} of
        {true, _} ->
            Task = service:apply_async(service_oneprovider:name(), manage_restart, #{}),
            ?info("Resuming oneprovider (task ~s)", [Task]);
        {_, true} ->
            Task = service:apply_async(service_onezone:name(), manage_restart, #{}),
            ?info("Resuming onezone (task ~s)", [Task]);
        _ -> ok % new deployment
    end,
    ok.


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

-include("names.hrl").
-include("deployment_progress.hrl").
-include_lib("ctool/include/logging.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc This function is called whenever an application is started
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
        node_cache:init(),
        test_node_starter:maybe_start_cover(),
        case onepanel_env:legacy_config_exists(onepanel) of
            true ->
                onepanel_env:upgrade_app_config(onepanel, [
                    [onepanel, advertise_address],
                    [onepanel, generate_test_web_cert],
                    [onepanel, test_web_cert_domain],
                    [onepanel, treat_test_ca_as_trusted]
                ], true);
            false -> ok
        end,
        case onepanel_sup:start_link() of
            {ok, Supervisor} ->
                resume_service(),
                {ok, Supervisor};
            Error -> Error
        end
    catch
        _:Reason ->
            ?error_stacktrace("Cannot start onepanel application due to: ~p",
                [Reason]),
            {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc This function is called whenever an application has stopped.
%% It is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> term().
stop(_State) ->
    https_listener:stop(),
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
    % Must be consistent with condition for blocking deployment request
    % (see rest_service:is_service_configured/0)
    ClusterExists = model:exists(onepanel_deployment)
        andalso onepanel_deployment:is_set(?PROGRESS_READY),

    case ClusterExists of
        true ->
            ClusterType = onepanel_env:get_cluster_type(),
            Task = service:apply_async(ClusterType, manage_restart, #{}),
            ?info("Resuming ~s (task id ~s)", [ClusterType, Task]);
        false -> ok % new deployment, managed by REST
    end,
    ok.


%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This behaviour provides a common service API.
%%% @end
%%%--------------------------------------------------------------------
-module(service_behaviour).
-author("Krzysztof Trzepla").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Returns service name.
%%--------------------------------------------------------------------
-callback name() -> Name :: service:name().


%%--------------------------------------------------------------------
%% Returns service hosts.
%%--------------------------------------------------------------------
-callback get_hosts() -> Hosts :: [service:host()].


%%--------------------------------------------------------------------
%% Returns service nodes.
%%--------------------------------------------------------------------
-callback get_nodes() -> Nodes :: [node()].


%%--------------------------------------------------------------------
%% List of steps required to complete the action.
%%--------------------------------------------------------------------
-callback get_steps(Action :: service:action(), Args :: service:step_ctx()) ->
    Steps :: [service:step()].
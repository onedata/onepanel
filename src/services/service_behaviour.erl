%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(service_behaviour).
-author("Krzysztof Trzepla").

%%%===================================================================
%%% Behaviour functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @todo write me!
%%--------------------------------------------------------------------
-callback get_steps(Action :: service:action(), Args :: service:args()) ->
    Steps :: [service:step()].
%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains utility functions for cluster deployment tests.
%%% @end
%%%--------------------------------------------------------------------
-module(cluster_test_utils).
-author("Bartosz Walkowicz").

-include("cluster_deployment_test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([
    get_ones3_status_all/1,
    get_ones3_status/2,
    toggle_ones3_all/2,
    toggle_ones3/3,

    await_task_status/3,
    await_task_status/4
]).

-define(ATTEMPTS, 60).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_ones3_status_all(node()) -> map().
get_ones3_status_all(PanelNode) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/provider/ones3">>, #{auth => root})
    ),
    Resp.


-spec get_ones3_status(node(), binary()) -> binary().
get_ones3_status(PanelNode, Hostname) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/provider/ones3/", Hostname/binary>>, #{auth => root})
    ),
    Resp.


-spec toggle_ones3_all(node(), stop | start) -> ok.
toggle_ones3_all(PanelNode, Action) ->
    Started = case Action of
        stop -> <<"false">>;
        start -> <<"true">>
    end,

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(PanelNode, <<"/provider/ones3?started=", Started/binary>>, #{auth => root})
    ),
    ok.


-spec toggle_ones3(node(), binary(), stop | start) -> ok.
toggle_ones3(PanelNode, Hostname, Action) ->
    Started = case Action of
        stop -> <<"false">>;
        start -> <<"true">>
    end,
    Qs = <<"?started=", Started/binary>>,
    Url = <<"/provider/ones3/", Hostname/binary, Qs/binary>>,

    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(PanelNode, Url, #{auth => root})
    ),
    ok.


-spec await_task_status(node(), binary(), binary()) -> ok.
await_task_status(Node, TaskId, ExpStatus) ->
    await_task_status(Node, TaskId, ExpStatus, ?ATTEMPTS).


-spec await_task_status(node(), binary(), binary(), non_neg_integer()) -> ok.
await_task_status(Node, TaskId, ExpStatus, Attempts) ->
    ?assertMatch(
        {ok, ?HTTP_200_OK, _, #{<<"status">> := ExpStatus}},
        panel_test_rest:get(Node, <<"/tasks/", TaskId/binary>>, #{auth => root}),
        Attempts
    ),
    ok.

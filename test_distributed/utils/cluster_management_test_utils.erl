%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% This module contains cluster management utility functions for use in tests.
%%% @end
%%%--------------------------------------------------------------------
-module(cluster_management_test_utils).
-author("Bartosz Walkowicz").

-include("names.hrl").
-include("cluster_deployment_test_utils.hrl").
-include_lib("ctool/include/http/codes.hrl").
-include_lib("ctool/include/test/assertions.hrl").

%% API
-export([
    get_ones3_port/1,

    get_ones3_status_cluster_wide/1,
    get_ones3_status_on_host/2,

    toggle_ones3_cluster_wide/2,
    try_toggle_ones3_cluster_wide/2,

    toggle_ones3_on_host/3,

    await_task_status/3,
    await_task_status/4
]).

-define(ATTEMPTS, 60).


%%%===================================================================
%%% API
%%%===================================================================


-spec get_ones3_port(node()) -> non_neg_integer().
get_ones3_port(PanelNode) ->
    panel_test_rpc:call(PanelNode, onepanel_env, get, [ones3_http_port, ?APP_NAME]).


-spec get_ones3_status_cluster_wide(node()) -> map().
get_ones3_status_cluster_wide(PanelNode) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/provider/ones3">>, #{auth => root})
    ),
    Resp.


-spec get_ones3_status_on_host(node(), binary()) -> binary().
get_ones3_status_on_host(PanelNode, Hostname) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/provider/ones3/", Hostname/binary>>, #{auth => root})
    ),
    Resp.


-spec toggle_ones3_cluster_wide(node(), stop | start) -> ok.
toggle_ones3_cluster_wide(PanelNode, Action) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        try_toggle_ones3_cluster_wide(PanelNode, Action)
    ),
    ok.


-spec try_toggle_ones3_cluster_wide(node(), stop | start) -> panel_test_rest:response().
try_toggle_ones3_cluster_wide(PanelNode, Action) ->
    Started = case Action of
        stop -> <<"false">>;
        start -> <<"true">>
    end,

    panel_test_rest:patch(PanelNode, <<"/provider/ones3?started=", Started/binary>>, #{auth => root}).


-spec toggle_ones3_on_host(node(), binary(), stop | start) -> ok.
toggle_ones3_on_host(PanelNode, Hostname, Action) ->
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

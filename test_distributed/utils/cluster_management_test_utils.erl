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
    refresh_oct/1,

    get_all_hosts/1,
    get_service_hosts/2,

    get_ones3_port/1,

    get_ones3_status_cluster_wide/1,
    get_ones3_status_on_host/2,

    get_service_status_cluster_wide/2,
    get_service_status_on_host/3,

    toggle_ones3_cluster_wide/2,
    try_toggle_ones3_cluster_wide/2,

    toggle_ones3_on_host/3,

    await_task_status/3,
    await_task_status/4
]).

-type service() :: worker | manager | database | ones3.

-define(ATTEMPTS, 60).


%%%===================================================================
%%% API
%%%===================================================================


-spec refresh_oct(test_utils:config()) -> test_utils:config().
refresh_oct(Config) ->
    NewConfig1 = oct_nodes:refresh_config(Config),
    NewConfig2 = oct_nodes:connect_with_nodes(NewConfig1),
    oct_background:update_environment(NewConfig2).


-spec get_all_hosts(node()) -> [binary()].
get_all_hosts(PanelNode) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, <<"/hosts">>, #{auth => root})
    ),
    Resp.


-spec get_service_hosts(node(), service()) -> [binary()].
get_service_hosts(PanelNode, Service) ->
    maps:keys(get_service_status_cluster_wide(PanelNode, Service)).


-spec get_ones3_port(node()) -> non_neg_integer().
get_ones3_port(PanelNode) ->
    panel_test_rpc:call(PanelNode, onepanel_env, get, [ones3_http_port, ?APP_NAME]).


-spec get_ones3_status_cluster_wide(node()) -> map().
get_ones3_status_cluster_wide(PanelNode) ->
    get_service_status_cluster_wide(PanelNode, ones3).


-spec get_ones3_status_on_host(node(), binary()) -> binary().
get_ones3_status_on_host(PanelNode, Hostname) ->
    get_service_status_on_host(PanelNode, ones3, Hostname).


-spec get_service_status_cluster_wide(node(), service()) -> map().
get_service_status_cluster_wide(PanelNode, Service) ->
    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(PanelNode, service_rest_path(Service), #{auth => root})
    ),
    Resp.


-spec get_service_status_on_host(node(), service(), binary()) -> binary().
get_service_status_on_host(PanelNode, Service, Hostname) ->
    Path = str_utils:format_bin("~ts/~ts", [service_rest_path(Service), Hostname]),

    {ok, _, _, Resp} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _}, panel_test_rest:get(PanelNode, Path, #{auth => root})
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


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec service_rest_path(service()) -> binary().
service_rest_path(worker) -> <<"/provider/workers">>;
service_rest_path(manager) -> <<"/provider/managers">>;
service_rest_path(database) -> <<"/provider/databases">>;
service_rest_path(ones3) -> <<"/provider/ones3">>.

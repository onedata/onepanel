%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Test bases of Onezone/Oneprovider cluster services.
%%% @end
%%%-------------------------------------------------------------------
-module(cl_services_test_base).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include("onepanel_test_utils.hrl").

%% tests
-export([
    service_stop_start_test_base/3,
    main_service_stop_restart_test_base/3
]).

-define(ATTEMPTS, 60).


%%%===================================================================
%%% Tests
%%%===================================================================


-spec service_stop_start_test_base(
    onedata:product(),
    cluster_management_test_utils:service(),
    [node()]
) ->
    ok.
service_stop_start_test_base(Product, Service, [PanelNode1, PanelNode2]) ->
    Host1 = dns_test_utils:get_hostname(PanelNode1),
    Host2 = dns_test_utils:get_hostname(PanelNode2),

    CheckFun = fun(ExpStatusOnHost1, ExpStatusOnHost2) ->
        check_service_statuses(PanelNode1, Product, Service, #{
            Host1 => ExpStatusOnHost1,
            Host2 => ExpStatusOnHost2
        })
    end,

    CheckFun(<<"healthy">>, <<"healthy">>),

    cluster_management_test_utils:toggle_service_on_host(PanelNode1, Product, Service, Host2, stop),
    CheckFun(<<"healthy">>, <<"stopped">>),

    cluster_management_test_utils:toggle_service_cluster_wide(PanelNode1, Product, Service, start),
    CheckFun(<<"healthy">>, <<"healthy">>),

    cluster_management_test_utils:toggle_service_cluster_wide(PanelNode1, Product, Service, stop),
    CheckFun(<<"stopped">>, <<"stopped">>),

    cluster_management_test_utils:toggle_service_on_host(PanelNode1, Product, Service, Host1, start),
    CheckFun(<<"healthy">>, <<"stopped">>),

    cluster_management_test_utils:toggle_service_cluster_wide(PanelNode1, Product, Service, stop),
    CheckFun(<<"stopped">>, <<"stopped">>),

    cluster_management_test_utils:toggle_service_cluster_wide(PanelNode1, Product, Service, start),
    CheckFun(<<"healthy">>, <<"healthy">>),

    ok.


-spec main_service_stop_restart_test_base(
    onedata:product(),
    [cluster_management_test_utils:service()],
    [node()]
) ->
    ok.
main_service_stop_restart_test_base(Product, Services, [PanelNode1, PanelNode2]) ->
    MainService = case Product of
        ?ONEPROVIDER -> ?SERVICE_OP;
        ?ONEZONE -> ?SERVICE_OZ
    end,
    Host1 = dns_test_utils:get_hostname(PanelNode1),
    Host2 = dns_test_utils:get_hostname(PanelNode2),

    CheckFun = fun(ExpStatus) ->
        ExpStatusesPerHost = #{
            Host1 => ExpStatus,
            Host2 => ExpStatus
        },
        lists:foreach(fun(Service) ->
            check_service_statuses(PanelNode1, Product, Service, ExpStatusesPerHost)
        end, Services)
    end,

    CheckFun(<<"healthy">>),
    onepanel_test_utils:service_action(PanelNode1, MainService, stop),
    CheckFun(<<"stopped">>),
    onepanel_test_utils:service_action(PanelNode1, MainService, manage_restart),
    CheckFun(<<"healthy">>),

    ok.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
check_service_statuses(Node, Product, Service, StatusPerHost) ->
    ?assertEqual(
        StatusPerHost,
        cluster_management_test_utils:get_service_status_cluster_wide(Node, Product, Service),
        ?ATTEMPTS
    ),

    maps:foreach(fun(Host, ExpStatus) ->
        ?assertEqual(
            ExpStatus,
            cluster_management_test_utils:get_service_status_on_host(Node, Product, Service, Host)
        )
    end, StatusPerHost).

%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onepanel current user API endpoints (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_current_user_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_current_user_details_from_zone_test/1,
    get_current_user_details_from_krakow_test/1,

    list_current_user_clusters_from_zone_test/1,
    list_current_user_clusters_from_krakow_test/1
]).

all() -> [
    get_current_user_details_from_zone_test,
    get_current_user_details_from_krakow_test,

    list_current_user_clusters_from_zone_test,
    list_current_user_clusters_from_krakow_test
].

-record(test_user, {
    id,
    privileges,
    cluster_ids,
    token
}).
-type test_user() :: #test_user{}.


%%%===================================================================
%%% API
%%%===================================================================


get_current_user_details_from_zone_test(Config) ->
    get_current_user_details_test_base(Config, ?OZ_PANEL, zone).


get_current_user_details_from_krakow_test(Config) ->
    get_current_user_details_test_base(Config, ?OP_PANEL, krakow).


%% @private
-spec get_current_user_details_test_base(test_config:config(), atom(), oct_background:entity_selector()) ->
    boolean().
get_current_user_details_test_base(Config, PanelType, PanelEntitySelector) ->
    TestUser = ?config(test_user, Config),

    Privileges = TestUser#test_user.privileges,
    ExpMemberDetails = #{
        <<"username">> => <<"Unnamed User">>,
        <<"userId">> => TestUser#test_user.id,
        <<"clusterPrivileges">> => [str_utils:to_binary(P) || P <- Privileges]
    },

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get current user details using /user REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = build_client_spec(PanelType, PanelEntitySelector, TestUser),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"user">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(ExpMemberDetails, RespBody)
            end)
        }
    ])).


list_current_user_clusters_from_zone_test(Config) ->
    list_current_user_clusters_test_base(Config, ?OZ_PANEL, zone).


list_current_user_clusters_from_krakow_test(Config) ->
    list_current_user_clusters_test_base(Config, ?OP_PANEL, krakow).


%% @private
-spec list_current_user_clusters_test_base(test_config:config(), atom(), oct_background:entity_selector()) ->
    boolean().
list_current_user_clusters_test_base(Config, PanelType, PanelEntitySelector) ->
    TestUser = ?config(test_user, Config),

    ExpClusterIds = lists:sort(TestUser#test_user.cluster_ids),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"List current user clusters using /user/clusters REST endpoint">>,
            type = rest,
            target_nodes = panel_test_utils:get_panel_nodes(PanelEntitySelector),
            client_spec = build_client_spec(PanelType, PanelEntitySelector, TestUser),

            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"user/clusters">>
            } end,
            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                #{<<"ids">> := ClusterIds} = ?assertMatch(#{<<"ids">> := _}, RespBody),
                ?assertEqual(ExpClusterIds, lists:sort(ClusterIds))
            end)
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op",
        posthook = fun(NewConfig) ->
            [{test_user, create_test_user()} | NewConfig]
        end
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
-spec create_test_user() -> test_user().
create_test_user() ->
    OzNode = hd(oct_background:get_zone_nodes()),

    UserId = ozw_test_rpc:create_user(),
    Privileges = ?RAND_SUBLIST(privileges:cluster_privileges()),

    ClusterIds = lists:map(fun(PanelNode) ->
        ClusterId = panel_test_rpc:get_cluster_id(PanelNode),
        ozw_test_rpc:add_user_to_cluster(ClusterId, UserId, Privileges),
        ClusterId
    end, lists:flatten([
        ?RAND_ELEMENT(oct_background:get_zone_panels()),
        ?RAND_ELEMENT(oct_background:get_provider_panels(krakow))
    ])),

    Token = ozw_test_rpc:create_user_temporary_access_token(OzNode, UserId),

    #test_user{
        id = UserId,
        privileges = Privileges,
        cluster_ids = ClusterIds,
        token = Token
    }.


%% @private
-spec build_client_spec(atom(), oct_background:entity_selector(), test_user()) ->
    api_test_runner:client_spec().
build_client_spec(PanelType, PanelEntitySelector, TestUser) ->
    EntityId = oct_background:to_entity_id(PanelEntitySelector),

    #client_spec{
        correct = [
            % Use user created in init_per_suite
            #api_client{
                role = member,
                privileges = TestUser#test_user.privileges,
                token = TestUser#test_user.token
            }
        ],
        unauthorized = [
            guest,
            {user, ?ERR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(PanelType, EntityId))}
            | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
        ],
        forbidden = [
            {root, ?ERROR_NOT_FOUND},
            peer
        ]
    }.

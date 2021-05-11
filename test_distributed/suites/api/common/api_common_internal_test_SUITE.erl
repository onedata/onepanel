%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onepanel internal API endpoints (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_internal_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("ctool/include/aai/aai.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/assertions.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_krakow_details_from_paris_test/1,
    get_krakow_details_from_zone_test/1,
    get_krakow_details_from_krakow_test/1,
    get_krakow_test_image_test/1,
    get_paris_test_image_test/1,
    get_zone_test_image_test/1
]).

all() -> [
    get_krakow_details_from_paris_test,
    get_krakow_details_from_zone_test,
    get_krakow_details_from_krakow_test,
    get_krakow_test_image_test,
    get_paris_test_image_test,
    get_zone_test_image_test
].

-define(TEST_IMAGE, <<
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 1, 3, 0, 0, 0, 37,
    219, 86, 202, 0, 0, 0, 6, 80, 76, 84, 69, 0, 0, 0, 255, 255, 255, 165, 217, 159, 221, 0, 0, 0, 9, 112,
    72, 89, 115, 0, 0, 14, 196, 0, 0, 14, 196, 1, 149, 43, 14, 27, 0, 0, 0, 10, 73, 68, 65, 84, 8, 153, 99,
    96, 0, 0, 0, 2, 0, 1, 244, 113, 100, 166, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130
>>).


%%%===================================================================
%%% API
%%%===================================================================


get_krakow_details_from_paris_test(Config) ->
    get_remote_op_details_test_base(Config, ?OP_PANEL, paris, krakow).


get_krakow_details_from_zone_test(Config) ->
    get_remote_op_details_test_base(Config, ?OZ_PANEL, zone, krakow).


get_krakow_details_from_krakow_test(Config) ->
    get_remote_op_details_test_base(Config, ?OP_PANEL, krakow, krakow).


%% @private
-spec get_remote_op_details_test_base(test_config:config(), atom(), oct_background:entity_selector(), oct_background:entity_selector()) -> boolean().
get_remote_op_details_test_base(_Config, TargetPanelType, TargetEntitySelector, RemoteProviderSelector) ->
    TargetId = oct_background:to_entity_id(TargetEntitySelector),
    TargetPanelNodes = case TargetPanelType of
        ?OZ_PANEL -> oct_background:get_zone_panels();
        ?OP_PANEL -> oct_background:get_provider_panels(TargetEntitySelector)
    end,

    RemoteProviderId = oct_background:get_provider_id(RemoteProviderSelector),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get remote provider details using /providers rest endpoint">>,
            type = rest,
            target_nodes = TargetPanelNodes,
            client_spec = #client_spec{
                correct = [member],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(TargetPanelType, TargetId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [
                    peer,
                    {root, ?ERROR_NOT_FOUND}
                ]
            },
            data_spec = build_get_remote_op_details_data_spec(),
            prepare_args_fun = build_get_remote_op_details_prepare_args_fun(RemoteProviderId),

            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ExpDetails = get_expected_provider_details(RemoteProviderSelector),
                ?assertEqual(ExpDetails, RespBody)
            end)
        }
    ])).


%% @private
-spec build_get_remote_op_details_data_spec() -> api_test_runner:data_spec().
build_get_remote_op_details_data_spec() ->
    #data_spec{
        bad_values = [
            {bad_id, <<"inexistentProviderId">>, ?ERROR_NOT_FOUND}
        ]
    }.


%% @private
-spec build_get_remote_op_details_prepare_args_fun(binary()) -> api_test_runner:prepare_args_fun().
build_get_remote_op_details_prepare_args_fun(RemoteProviderId) ->
    fun(#api_test_ctx{data = Data}) ->
        {Id, _} = api_test_utils:maybe_substitute_bad_id(RemoteProviderId, Data),
        #rest_args{
            method = get,
            path = <<"providers/", Id/binary>>
        }
    end.


%% @private
-spec get_expected_provider_details(atom() | binary()) -> map().
get_expected_provider_details(Provider) ->
    ProviderId = oct_background:get_provider_id(Provider),
    OpDomain = oct_background:get_provider_domain(Provider),
    OpName = oct_background:get_provider_name(Provider),
    Localization = case oct_background:to_entity_placeholder(Provider) of
        krakow -> #{
            <<"geoLatitude">> => ?PROVIDER_KRAKOW_GEO_LATITUDE,
            <<"geoLongitude">> => ?PROVIDER_KRAKOW_GEO_LONGITUDE
        };
        paris -> #{
            <<"geoLatitude">> => ?PROVIDER_PARIS_GEO_LATITUDE,
            <<"geoLongitude">> => ?PROVIDER_PARIS_GEO_LONGITUDE
        }
    end,

    maps:merge(Localization, #{
        <<"name">> => OpName,
        <<"domain">> => OpDomain,
        <<"id">> => ProviderId,
        <<"online">> => true
    }).


get_krakow_test_image_test(Config) ->
    get_test_image_test_base(Config, ?OP_PANEL, krakow).


get_paris_test_image_test(Config) ->
    get_test_image_test_base(Config, ?OP_PANEL, paris).


get_zone_test_image_test(Config) ->
    get_test_image_test_base(Config, ?OZ_PANEL, zone).


%% @private
-spec get_test_image_test_base(test_config:config(), atom(), oct_background:entity_selector()) -> ok.
get_test_image_test_base(_Config, PanelType, EntitySelector) ->
    EntityId = oct_background:to_entity_id(EntitySelector),
    PanelNodes = case PanelType of
        ?OZ_PANEL -> oct_background:get_zone_panels();
        ?OP_PANEL -> oct_background:get_provider_panels(EntitySelector)
    end,
    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Get test image using /test_image rest endpoint">>,
            type = rest,
            target_nodes = PanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    member,
                    guest,
                    peer
                ],
                unauthorized = [
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(PanelType, EntityId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ]
            },
            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"test_image">>
            } end,

            validate_result_fun = api_test_validate:http_200_ok(fun(RespBody) ->
                ?assertEqual(?TEST_IMAGE, RespBody)
            end)
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "2op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning onepanel Emergency Passphrase API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_common_emergency_passphrase_test_SUITE).
-author("Piotr Duleba").

-include("api_test_runner.hrl").
-include("api_test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_provider_panel_emergency_passphrase_status_test/1,
    get_zone_panel_emergency_passphrase_status_test/1,

    update_emergency_passphrase_with_op_panel_test/1,
    update_emergency_passphrase_with_oz_panel_test/1
]).

all() -> [
    get_provider_panel_emergency_passphrase_status_test,
    get_zone_panel_emergency_passphrase_status_test,

    update_emergency_passphrase_with_op_panel_test,
    update_emergency_passphrase_with_oz_panel_test
].

-define(INITIAL_EMERGENCY_PASSPHRASE, <<"password">>).

-define(GET_EP_STATUS_CLIENT_SPEC(PanelType, TargetId), #client_spec{
    correct = [
        root,
        member,
        peer,
        guest
    ],
    unauthorized = [
        {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(PanelType, TargetId))}
        | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
    ]
}).

-define(UPDATE_EP_CLIENT_SPEC(PanelType, TargetId), #client_spec{
    correct = [
        root
    ],
    forbidden = [
        member,
        peer
    ],
    unauthorized = [
        {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(PanelType, TargetId))},
        guest
        | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
    ]
}).


%%%===================================================================
%%% API
%%%===================================================================


get_provider_panel_emergency_passphrase_status_test(_Config) ->
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    ProviderId = oct_background:get_provider_id(krakow),
    panel_test_rpc:set_emergency_passphrase(krakow, ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        ProviderPanelNodes, ?GET_EP_STATUS_CLIENT_SPEC(?OP_PANEL, ProviderId), true, true
    ).


get_zone_panel_emergency_passphrase_status_test(_Config) ->
    ZonePanelNodes = oct_background:get_zone_panels(),
    ZoneId = oct_background:to_entity_id(zone),
    panel_test_rpc:set_emergency_passphrase(zone, ?INITIAL_EMERGENCY_PASSPHRASE),
    api_common_emergency_passphrase_test_base:get_emergency_passphrase_status_test_base(
        ZonePanelNodes, ?GET_EP_STATUS_CLIENT_SPEC(?OZ_PANEL, ZoneId), true, true
    ).


update_emergency_passphrase_with_op_panel_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    ProviderPanelNodes = oct_background:get_provider_panels(krakow),
    api_common_emergency_passphrase_test_base:update_emergency_passphrase_test_base(
        ProviderPanelNodes, ?UPDATE_EP_CLIENT_SPEC(?OP_PANEL, ProviderId), ?INITIAL_EMERGENCY_PASSPHRASE, true
    ).


update_emergency_passphrase_with_oz_panel_test(_Config) ->
    ZonePanelNodes = oct_background:get_zone_panels(),
    api_common_emergency_passphrase_test_base:update_emergency_passphrase_test_base(
        ZonePanelNodes, ?UPDATE_EP_CLIENT_SPEC(?OZ_PANEL, <<"onezone">>), ?INITIAL_EMERGENCY_PASSPHRASE, true
    ).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).


end_per_suite(_Config) ->
    oct_background:end_per_suite().

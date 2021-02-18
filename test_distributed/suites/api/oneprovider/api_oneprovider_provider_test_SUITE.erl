%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file contains tests concerning provider basic API (REST).
%%% @end
%%%-------------------------------------------------------------------
-module(api_oneprovider_provider_test_SUITE).
-author("Bartosz Walkowicz").

-include("api_test_runner.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).

-export([
    get_provider_details/1
]).

all() -> [
    get_provider_details
].


%%%===================================================================
%%% API
%%%===================================================================


get_provider_details(Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    OpDomain = ?GET_DOMAIN_BIN(hd(OpWorkerNodes)),
    OpName = hd(binary:split(OpDomain, <<".">>)),

    ExpDetails = #{
        <<"name">> => OpName,
        <<"domain">> => OpDomain,
        <<"onezoneDomainName">> => ?GET_DOMAIN_BIN(?OZ_NODE(Config)),
        <<"id">> => ProviderId,
        % Below values are defined in k8s charts
        <<"geoLatitude">> => 50.0647,
        <<"geoLongitude">> => 19.945,
        <<"adminEmail">> => <<"getting-started@onedata.org">>,
        <<"subdomainDelegation">> => false
    },

    ?assert(api_test_runner:run_tests(Config, [
        #scenario_spec{
            name = <<"Get provider details using /provider rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, []}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },
            prepare_args_fun = fun(_) -> #rest_args{method = get, path = <<"provider">>} end,
            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                ?assertEqual({?HTTP_200_OK, ExpDetails}, {RespCode, RespBody})
            end
        }
    ])).


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).



end_per_suite(_Config) ->
    oct_background:end_per_suite().
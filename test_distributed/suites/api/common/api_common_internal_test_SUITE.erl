%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This file provides tests concerning onezone user management API (REST).
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
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).

-export([
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    get_remote_op_details_test/1

]).

all() -> [
    get_remote_op_details_test
].


%%%===================================================================
%%% API
%%%===================================================================

get_remote_op_details_test(Config) ->
    get_remote_op_details_test_base(Config, krakow).


get_remote_op_details_test_base(Config, Provider) ->
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),
    ct:pal("ProviderId: ~p", [ProviderId]),

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
            name = <<"Get provider details using /providers rest endpoint">>,
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
            prepare_args_fun = fun(_) -> #rest_args{
                method = get,
                path = <<"providers/", ProviderId/binary>>
            } end,

            validate_result_fun = fun(_, {ok, RespCode, _, RespBody}) ->
                ct:pal("Reponse: ~p", [RespBody]),
                ?assertEqual({?HTTP_200_OK, ExpDetails}, {RespCode, RespBody})
            end
        }
    ])).





%%%===================================================================
%%% Helper functions
%%%===================================================================



%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    application:start(ssl),
    hackney:start(),
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "2op"
    }).

end_per_suite(_Config) ->
    hackney:stop(),
    application:stop(ssl).

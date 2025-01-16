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
-include_lib("ctool/include/privileges.hrl").
-include_lib("ctool/include/test/test_utils.hrl").
-include_lib("onenv_ct/include/chart_values.hrl").
-include_lib("onenv_ct/include/oct_background.hrl").

%% API
-export([all/0]).
-export([
    init_per_suite/1,
    end_per_suite/1,

    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    get_provider_details_test/1,
    modify_provider_details_test/1
]).

all() -> [
    get_provider_details_test,
    modify_provider_details_test
].

-define(DEFAULT_MAIL, <<"getting-started@onedata.org">>).

-define(ATTEMPTS, 10).


%%%===================================================================
%%% API
%%%===================================================================


get_provider_details_test(_Config) ->
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    OpDomain = ?GET_DOMAIN_BIN(hd(OpWorkerNodes)),
    OpName = hd(binary:split(OpDomain, <<".">>)),

    ExpDetails = #{
        <<"name">> => OpName,
        <<"domain">> => OpDomain,
        <<"onezoneDomainName">> => ?GET_DOMAIN_BIN(hd(oct_background:get_zone_nodes())),
        <<"id">> => ProviderId,
        % Below values are defined in k8s charts
        <<"geoLatitude">> => ?PROVIDER_KRAKOW_GEO_LATITUDE,
        <<"geoLongitude">> => ?PROVIDER_KRAKOW_GEO_LONGITUDE,
        <<"adminEmail">> => ?DEFAULT_MAIL,
        <<"subdomainDelegation">> => false
    },

    ?assert(api_test_runner:run_tests([
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


modify_provider_details_test(_Config) ->
    MemRef = api_test_memory:init(),
    ProviderId = oct_background:get_provider_id(krakow),
    OpWorkerNodes = oct_background:get_provider_nodes(krakow),
    OpPanelNodes = oct_background:get_provider_panels(krakow),

    lists:foreach(fun({MemKey, DefaultValue}) ->
        api_test_memory:set(MemRef, MemKey, DefaultValue)
    end, get_default_provider_details()),

    ?assert(api_test_runner:run_tests([
        #scenario_spec{
            name = <<"Update provider details using /provider rest endpoint">>,
            type = rest,
            target_nodes = OpPanelNodes,
            client_spec = #client_spec{
                correct = [
                    root,
                    {member, [?CLUSTER_UPDATE]}
                ],
                unauthorized = [
                    guest,
                    {user, ?ERROR_TOKEN_SERVICE_FORBIDDEN(?SERVICE(?OP_PANEL, ProviderId))}
                    | ?INVALID_API_CLIENTS_AND_AUTH_ERRORS
                ],
                forbidden = [peer]
            },

            prepare_args_fun = fun(#api_test_ctx{data = Data}) ->
                #rest_args{
                    method = patch,
                    path = <<"provider">>,
                    headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
                    body = json_utils:encode(Data)
                }
            end,

            verify_fun = build_modify_provider_details_verify_fun(MemRef),
            validate_result_fun = api_test_validate:http_204_no_content(),

            data_spec = build_modify_provider_details_data_spec(OpWorkerNodes)
        }
    ])).


%% @private
build_modify_provider_details_data_spec(OpWorkerNodes) ->
    HostNames = api_test_utils:to_hostnames(OpWorkerNodes),

    #data_spec{
        optional = [<<"name">>, <<"geoLatitude">>, <<"geoLongitude">>, <<"adminEmail">>],
        correct_values = #{
            <<"name">> => [?RAND_STR()],
            <<"geoLatitude">> => [?RAND_FLOAT(0, 90)],
            <<"geoLongitude">> => [?RAND_FLOAT(0, 180)],
            <<"adminEmail">> => [?RAND_EMAIL_ADDRESS()]
        },
        bad_values = [
            {<<"name">>, 1, ?ERROR_BAD_VALUE_BINARY(<<"name">>)},
            {<<"name">>, <<"0">>, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_NAME, HostNames)},
            {<<"geoLatitude">>, <<"Nan">>, ?ERROR_BAD_VALUE_FLOAT(<<"geoLatitude">>)},
            {<<"geoLatitude">>, 91, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"latitude">>, -90, 90), HostNames)},
            {<<"geoLongitude">>, <<"Nan">>, ?ERROR_BAD_VALUE_FLOAT(<<"geoLongitude">>)},
            {<<"geoLongitude">>, 191, ?ERROR_ON_NODES(?ERROR_BAD_VALUE_NOT_IN_RANGE(<<"longitude">>, -180, 180), HostNames)},
            {<<"adminEmail">>, 1, ?ERROR_BAD_VALUE_BINARY(<<"adminEmail">>)},
            {<<"adminEmail">>, ?RAND_STR(), ?ERROR_ON_NODES(?ERROR_BAD_VALUE_EMAIL, HostNames)}
        ]
    }.


%% @private
build_modify_provider_details_verify_fun(MemRef) ->
    fun(ExpResult, #api_test_ctx{data = Data}) ->
        Keys = [<<"name">>, <<"geoLatitude">>, <<"geoLongitude">>, <<"adminEmail">>],
        PrevValues = lists:map(fun(Key) -> api_test_memory:get(MemRef, Key) end, Keys),

        ExpValues = [ExpName, ExpLatitude, ExpLongitude, ExpMail] = case ExpResult of
            expected_success ->
                lists:map(
                    fun({Key, PrevValue}) -> maps:get(Key, Data, PrevValue) end,
                    lists:zip(Keys, PrevValues)
                );

            expected_failure ->
                PrevValues
        end,

        lists:foreach(fun({Key, NewValue}) ->
            api_test_memory:set(MemRef, Key, NewValue)
        end, lists:zip(Keys, ExpValues)),

        ExpDetails = #{
            <<"name">> => ExpName,
            <<"geoLatitude">> => ExpLatitude,
            <<"geoLongitude">> => ExpLongitude,
            <<"adminEmail">> => ExpMail,
            <<"domain">> => oct_background:get_provider_domain(krakow),
            <<"onezoneDomainName">> => oct_background:get_zone_domain(),
            <<"id">> => oct_background:get_provider_id(krakow),
            % Below values are defined in k8s charts
            <<"subdomainDelegation">> => false
        },

        ?assertEqual(ExpDetails, get_provider_details(), ?ATTEMPTS),
        true

    end.


%%%===================================================================
%%% SetUp and TearDown functions
%%%===================================================================


init_per_suite(Config) ->
    oct_background:init_per_suite(Config, #onenv_test_config{
        onenv_scenario = "1op"
    }).



end_per_suite(_Config) ->
    oct_background:end_per_suite().


init_per_testcase(_, Config) ->
    Config.


end_per_testcase(modify_provider_details_test, Config) ->
    update_provider_details(maps:from_list(get_default_provider_details())),
    Config;

end_per_testcase(_, Config) ->
    Config.


%%%===================================================================
%%% Helper functions
%%%===================================================================


%% @private
get_provider_details() ->
    {ok, _, _, Details} = ?assertMatch(
        {ok, ?HTTP_200_OK, _, _},
        panel_test_rest:get(krakow, <<"/provider">>, #{auth => root})
    ),
    Details.


%% @private
update_provider_details(Data) ->
    ?assertMatch(
        {ok, ?HTTP_204_NO_CONTENT, _, _},
        panel_test_rest:patch(krakow, <<"/provider">>, #{auth => root, json => Data})
    ).


%% @private
get_default_provider_details() ->
    [
        {<<"name">>, oct_background:get_provider_name(krakow)},
        {<<"geoLatitude">>, ?PROVIDER_KRAKOW_GEO_LATITUDE},
        {<<"geoLongitude">>, ?PROVIDER_KRAKOW_GEO_LONGITUDE},
        {<<"adminEmail">>, ?DEFAULT_MAIL}
    ].

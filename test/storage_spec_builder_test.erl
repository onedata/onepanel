%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests of storage_spec_builder: the storage specs it builds from REST
%%% requests.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_spec_builder_test).
-author("Bartosz Walkowicz").

-include_lib("opw_panel_contracts/include/storage/common.hrl").
-include_lib("opw_panel_contracts/include/storage/s3.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(QOS_PARAMETERS, #{<<"priority">> => <<"2">>, <<"region">> => <<"eu">>}).


%%%===================================================================
%%% Tests
%%%===================================================================


create_spec_passes_qos_parameters_on_as_given_test() ->
    #storage_create_spec{qos_parameters = QosParameters} = storage_spec_builder:build_create_spec(
        <<"some-storage">>,
        #{type => <<"posix">>, mountPoint => <<"/mnt/st">>, lumaFeed => <<"auto">>, qosParameters => ?QOS_PARAMETERS}
    ),

    ?assertEqual(?QOS_PARAMETERS, QosParameters).


update_spec_passes_qos_parameters_on_as_given_test() ->
    #storage_update_spec{qos_parameters = QosParameters} = storage_spec_builder:build_update_spec(
        <<"posix">>, #{qosParameters => ?QOS_PARAMETERS}
    ),

    ?assertEqual(?QOS_PARAMETERS, QosParameters).


%% The parameters of an update replace all the previous ones, so an update that
%% doesn't name them must leave them out rather than send an empty map.
update_spec_without_qos_parameters_leaves_them_out_test() ->
    #storage_update_spec{qos_parameters = QosParameters} = storage_spec_builder:build_update_spec(
        <<"posix">>, #{name => <<"new-name">>}
    ),

    ?assertEqual(undefined, QosParameters).


%% An update that names no LUMA setting must not send a LUMA spec: op-worker
%% would take it for a change of the LUMA config.
update_spec_without_luma_parameters_leaves_luma_out_test() ->
    #storage_update_spec{luma = Luma} = storage_spec_builder:build_update_spec(
        <<"posix">>, #{name => <<"new-name">>}
    ),

    ?assertEqual(undefined, Luma).


update_spec_with_a_luma_parameter_carries_it_test() ->
    #storage_update_spec{luma = Luma} = storage_spec_builder:build_update_spec(
        <<"posix">>, #{lumaFeedUrl => <<"https://luma.example.com">>}
    ),

    ?assertEqual(#luma_spec_diff{url = <<"https://luma.example.com">>}, Luma).


%% op-worker refuses an S3 storage at s3.amazonaws.com without a region, so
%% the REST model must not fill in a default one.
create_spec_of_s3_without_region_carries_no_region_test() ->
    #{<<"some-storage">> := Params} = onepanel_parser:parse(
        #{<<"some-storage">> => #{
            <<"type">> => <<"s3">>,
            <<"hostname">> => <<"https://s3.amazonaws.com">>,
            <<"bucketName">> => <<"some-bucket">>
        }},
        rest_model:storage_create_request_model()
    ),
    #storage_create_spec{configuration = #s3_helper_configuration{region = Region}} =
        storage_spec_builder:build_create_spec(<<"some-storage">>, Params),

    ?assertEqual(undefined, Region).


%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2026 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests of the QoS parameters in the storage specs built from REST
%%% requests. They go to op-worker as the request had them, as strings:
%%% op-worker stores as numbers those that a QoS expression reads as numbers.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_spec_builder_test).
-author("Bartosz Walkowicz").

-include_lib("opw_panel_contracts/include/storage/common.hrl").
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

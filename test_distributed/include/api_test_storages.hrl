%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of macros and records used in storage API (REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Piotr Duleba").

-ifndef(API_TEST_STORAGES_HRL).
-define(API_TEST_STORAGES_HRL, 1).
-include("api_test_runner.hrl").

-record(add_storage_test_spec, {
    storage_type = undefined :: api_oneprovider_storages_test_base:storage_type(),
    args_correctness = undefined :: api_oneprovider_storages_test_base:args_correctness(),
    skip_storage_detection = undefined :: api_oneprovider_storages_test_base:skip_storage_detection(),

    data_spec_fun = api_oneprovider_storages_test_base:data_spec_builder(),
    prepare_args_fun = api_oneprovider_storages_test_base:prepare_args_fun_builder()
}).

-define(STORAGE_DETECTION_FILE_SIZE, 10000).

-define(STORAGE_DATA_KEY(StorageName, Key), iolist_to_binary([StorageName, <<".">>, Key])).
-define(SUPPORT_SIZE, 10000000).
-define(ATTEMPTS, 60).
-define(STORAGE_TYPES, [
    <<"cephrados">>,
    <<"glusterfs">>,
    <<"http">>,
    <<"localceph">>,
    <<"nulldevice">>,
    <<"posix">>,
    <<"s3">>,
    <<"swift">>,
    <<"webdav">>,
    <<"xrootd">>
]).
-define(STORAGE_TIMEOUT, 5000).
-define(STORAGE_QOS_PARAMETERS, #{
    <<"key">> => <<"value">>
}).

-endif.

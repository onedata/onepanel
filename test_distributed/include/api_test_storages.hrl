%%%-------------------------------------------------------------------
%%% @author Piotr Duleba
%%% @copyright (C) 2021 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of macros and records used in storage tests API (REST) tests.
%%% @end
%%%-------------------------------------------------------------------
-author("Piotr Duleba").

-ifndef(API_TEST_STORAGES_HRL).
-define(API_TEST_STORAGES_HRL, 1).

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

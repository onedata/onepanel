%%%-------------------------------------------------------------------
%%% @author Piotr Dulęba
%%% @copyright (C) 2020 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Definitions of macros and records used in api_test_utils module.
%%% @end
%%%-------------------------------------------------------------------

-record(placeholder_substitute, {
    value = undefined :: undefined | atom() | list() | map(),
    posthook = fun() -> ok end
}).

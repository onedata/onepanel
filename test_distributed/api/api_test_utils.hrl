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
    key = undefined :: undefined | binary(),
    placeholder = undefined :: undefined | atom(),
    value = undefined :: undefined | atom() | list() | map(),
    additional_fun = fun() -> ok end
}).

-type placeholder_substitute() :: #placeholder_substitute{}.

-export_type([
    placeholder_substitute/0
]).

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

% This record is used to replace placeholders in maps.
% It is mainly used in function api_tests_utils:substitute_placeholders\2, where
% it is passed as a value in nested map, with placeholders as a keys.
-record(placeholder_substitute, {
    % value that placeholder will be replaced with
    value = undefined :: undefined | term(),

    % callback function, that will be executed after placeholder replacement
    posthook = fun() -> ok end
}).

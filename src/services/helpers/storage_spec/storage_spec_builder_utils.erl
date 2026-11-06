%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2025 Onedata (onedata.org)
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Common utilities for storage specification builders.
%%% @end
%%%--------------------------------------------------------------------
-module(storage_spec_builder_utils).
-author("Bartosz Walkowicz").

%% API
-export([
    binary_to_storage_path_type/1,
    storage_path_type_to_binary/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec binary_to_storage_path_type(binary()) -> flat | canonical.
binary_to_storage_path_type(<<"flat">>) -> flat;
binary_to_storage_path_type(<<"canonical">>) -> canonical.


-spec storage_path_type_to_binary(flat | canonical) -> binary().
storage_path_type_to_binary(flat) -> <<"flat">>;
storage_path_type_to_binary(canonical) -> <<"canonical">>.

%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an interface to 'service_oneprovider_nif' native
%%% library.
%%% @end
%%%--------------------------------------------------------------------
-module(service_oneprovider_nif).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([init/0]).
-export([create_csr/1]).

-on_load(init/0).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads the NIF native library used for CSR generation.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | no_return().
init() ->
    LibPath = onepanel_utils:get_nif_library_path("service_oneprovider_nif"),
    case erlang:load_nif(LibPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, {upgrade, _}} -> ok;
        {error, Reason} -> ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Creates Certificate Signing Requests.
%% @end
%%--------------------------------------------------------------------
-spec create_csr(Password :: binary()) ->
    {ok, Key :: binary(), Csr :: binary()} | {error, Reason :: binary(), binary()}.
create_csr(_Password) ->
    erlang:nif_error({?ERR_NIF_NOT_LOADED, ?MODULE}).
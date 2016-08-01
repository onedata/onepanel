%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module provides an interface to 'onepanel_user_nif' native library.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_user_nif).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([init/0]).
-export([hash_password/2, check_password/2]).

-on_load(init/0).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads the NIF native library used for password hashing and verification.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | no_return().
init() ->
    LibPath = onepanel_utils:get_nif_library_path("onepanel_user_nif"),
    case erlang:load_nif(LibPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, {upgrade, _}} -> ok;
        {error, Reason} -> ?throw(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Returns a password hash using bcrypt algorithm.
%% @end
%%--------------------------------------------------------------------
-spec hash_password(Password :: onepanel_user:password(),
    WorkFactor :: non_neg_integer()) -> Hash :: onepanel_user:password_hash().
hash_password(_Password, _WorkFactor) ->
    erlang:nif_error({?ERR_NIF_NOT_LOADED, ?MODULE}).


%%--------------------------------------------------------------------
%% @doc Checks password against original password hash.
%% @end
%%--------------------------------------------------------------------
-spec check_password(Password :: onepanel_user:password(),
    Hash :: onepanel_user:password_hash()) -> Valid :: boolean().
check_password(_Password, _Hash) ->
    erlang:nif_error({?ERR_NIF_NOT_LOADED, ?MODULE}).
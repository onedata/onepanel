%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module manages the emergency passphrase. The passphrase is used
%%% to access Onepanel without authenticating as a Onezone user
%%% belonging to the cluster.
%%% @end
%%%--------------------------------------------------------------------
-module(emergency_passphrase).
-author("Wojciech Geisler").

-include("validation.hrl").
-include("modules/kv_keys.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([is_set/0, get_hash/0]).
-export([set/1, change/2]).
-export([verify/1]).
-export([migrate_from_users/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Ensures given passphrase is valid and sets it as the new root passphrase.
%% @end
%%--------------------------------------------------------------------
-spec set(Passphrase :: binary()) -> ok | #error{}.
set(Passphrase) ->
    case validate(Passphrase) of
        ok -> set_hash(onedata_passwords:create_hash(Passphrase));
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc Checks whether there is any passphrase set.
%% @end
%%--------------------------------------------------------------------
-spec is_set() -> boolean().
is_set() ->
    onepanel_kv:exists(?KV_EMERGENCY_PASSPHRASE).


%%--------------------------------------------------------------------
%% @doc Returns hashed emergency passphrase.
%% @end
%%--------------------------------------------------------------------
-spec get_hash() -> {ok, binary()} | #error{}.
get_hash() ->
    onepanel_kv:find(?KV_EMERGENCY_PASSPHRASE).


%%--------------------------------------------------------------------
%% @doc
%% Sets new passphrase after ensuring old one is provided correctly.
%% @end
%%--------------------------------------------------------------------
-spec change(OldPassphrase :: binary() | undefined, NewPassphrase :: binary()) ->
    ok | #error{}.
change(OldPassphrase, NewPassphrase) ->
    case not is_set() orelse verify(OldPassphrase) of
        true -> set(NewPassphrase);
        false ->
            ?info("Attempt to change emergency passphrase failed due to" ++
            "incorrect previous passphrase given"),
            ?make_error(?ERR_INVALID_PASSPHRASE)
    end.


%%--------------------------------------------------------------------
%% @doc Checks whether given passphrase matches the currently set.
%% @end
%%--------------------------------------------------------------------
-spec verify(Passphrase :: binary() | undefined) -> boolean().
verify(PlaintextPassphrase) when is_binary(PlaintextPassphrase) ->
    case get_hash() of
        {ok, Hash} ->
            onedata_passwords:verify(PlaintextPassphrase, Hash);
        #error{reason = ?ERR_NOT_FOUND} ->
            false
    end;

verify(_) ->
    false.


%%--------------------------------------------------------------------
%% @doc Handles upgrade from versions featuring onepanel_users
%% by settings passphrase equal to an admin's password.
%% If user named 'admin' exists their password is used, otherwise
%% any other user's with role=admin.
%% @end
%%--------------------------------------------------------------------
-spec migrate_from_users() -> ok.
migrate_from_users() ->
    case is_set() of
        true -> ok;
        false ->
            case find_admin_user() of
                {ok, Record} ->
                    Username = onepanel_user:get_username(Record),
                    PassphraseHash = onepanel_user:get_password_hash(Record),
                    set_hash(PassphraseHash),
                    ?info("Set passphrase of user '~s' as the emergency passphrase",
                        [Username]),
                    ok;
                #error{reason = ?ERR_NOT_FOUND} ->
                    ok
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec set_hash(PassphraseHash :: binary()) -> ok.
set_hash(PassphraseHash) ->
    onepanel_kv:set(?KV_EMERGENCY_PASSPHRASE, PassphraseHash).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validates given passphrase against length and allowed characters
%% constraints.
%% @end
%%--------------------------------------------------------------------
-spec validate(Passphrase :: binary()) -> ok | #error{}.
validate(Passphrase) when size(Passphrase) < ?PASSPHRASE_MIN_LENGTH ->
    ?make_error(?ERR_INVALID_NEW_PASSPHRASE);

validate(Passphrase) when is_binary(Passphrase) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finds user whose passphrase should be set as emergency passphrase.
%% @end
%%--------------------------------------------------------------------
-spec find_admin_user() -> {ok, onepanel_user:record()} | #error{}.
find_admin_user() ->
    case onepanel_user:get(<<"admin">>) of
        {ok, #onepanel_user{role = admin} = Admin} ->
            {ok, Admin};
        _ -> case onepanel_user:get_by_role(admin) of
            [Admin | _] ->
                {ok, Admin};
            _ ->
                ?make_error(?ERR_NOT_FOUND)
        end
    end.
%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-module(onedata_user).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("db/models.hrl").

%% API
-export([new/3, authenticate/2, hash_password/1, change_password/2]).

%% Model callbacks
-export([fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

-define(UUID_LEN, 32).

%%%===================================================================
%%% Model callbacks
%%%===================================================================

fields() ->
    record_info(fields, ?MODULE).

create(Record) ->
    model_logic:create(?MODULE, Record).

save(Record) ->
    model_logic:save(?MODULE, Record).

update(Key, Diff) ->
    model_logic:update(?MODULE, Key, Diff).

get(Key) ->
    model_logic:get(?MODULE, Key).

exists(Key) ->
    model_logic:exists(?MODULE, Key).

delete(Key) ->
    model_logic:delete(?MODULE, Key).

%%%===================================================================
%%% API functions
%%%===================================================================

new(Username, Password, Role) ->
    case {verify_username(Username), verify_password(Password)} of
        {ok, ok} ->
            create(#onedata_user{
                username = Username, password_hash = hash_password(Password),
                role = Role, uuid = gen_uuid()
            });
        {{error, Reason}, _} -> {error, Reason};
        {_, {error, Reason}} -> {error, Reason}
    end.

authenticate(Username, Password) ->
    case onedata_user:get(Username) of
        {ok, #onedata_user{password_hash = Hash} = User} ->
            case hash_password(Password) of
                Hash -> {ok, User};
                _ -> {error, invalid_username_or_password}
            end;
        _ -> {error, invalid_username_or_password}
    end.

hash_password(Password) ->
    crypto:hash(sha512, Password).

change_password(Username, NewPassword) ->
    case verify_password(NewPassword) of
        ok ->
            onedata_user:update(Username, #{
                password_hash => hash_password(NewPassword)
            });
        {error, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates random UUID.
%% @end
%%--------------------------------------------------------------------
-spec gen_uuid() -> binary().
gen_uuid() ->
    http_utils:base64url_encode(crypto:rand_bytes(?UUID_LEN)).

verify_username(<<>>) ->
    {error, empty_username};
verify_username(_) ->
    ok.

verify_password(Password) ->
    {ok, MinPasswordLength} = onepanel:get_env(min_password_length),
    case size(Password) < MinPasswordLength of
        true -> {error, {short_password, MinPasswordLength}};
        false -> ok
    end.
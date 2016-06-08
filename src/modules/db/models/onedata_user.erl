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

%% Model behaviour callbacks
-export([fields/0, create/1, save/1, update/2, get/1, exists/1, delete/1]).

%% API
-export([new/3, authenticate/2, hash_password/1, change_password/2]).

-type name() :: binary().
-type password() :: binary().
-type password_hash() :: binary().
-type role() :: admin | regular.
-type uuid() :: binary().

-export_type([name/0, password/0, password_hash/0, role/0, uuid/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @see model_behaviour:fields/0
%%--------------------------------------------------------------------
-spec fields() -> list(atom()).
fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:create/1
%%--------------------------------------------------------------------
-spec create(Record :: model_behaviour:record()) ->
    ok | {error, Reason :: term()}.
create(Record) ->
    model_logic:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:save/1
%%--------------------------------------------------------------------
-spec save(Record :: model_behaviour:record()) -> ok | {error, Reason :: term()}.
save(Record) ->
    model_logic:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:update/2
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | {error, Reason :: term()}.
update(Key, Diff) ->
    model_logic:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:get/1
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: model_behaviour:record()} | {error, Reason :: term()}.
get(Key) ->
    model_logic:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:exists/1
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | {error, Reason :: term()}.
exists(Key) ->
    model_logic:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc @see model_behaviour:delete/1
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | {error, Reason :: term()}.
delete(Key) ->
    model_logic:delete(?MODULE, Key).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec new(Username :: name(), Password :: password(), Role :: role()) ->
    ok | {error, Reason :: term()}.
new(Username, Password, Role) ->
    case {verify_username(Username), verify_password(Password)} of
        {Username, Password} ->
            create(#onedata_user{
                username = Username, password_hash = hash_password(Password),
                role = Role, uuid = onepanel_utils:gen_uuid()
            });
        {{error, Reason}, _} -> {error, Reason};
        {_, {error, Reason}} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Username :: name(), Password :: password()) ->
    {ok, User :: #onedata_user{}} | {error, Reason :: term()}.
authenticate(Username, Password) ->
    case onedata_user:get(Username) of
        {ok, #onedata_user{password_hash = Hash} = User} ->
            case hash_password(Password) of
                Hash -> {ok, User};
                _ -> {error, invalid_username_or_password}
            end;
        _ -> {error, invalid_username_or_password}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec hash_password(Password :: password()) -> Hash :: password_hash().
hash_password(Password) ->
    crypto:hash(sha512, Password).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec change_password(Username :: name(), NewPassword :: password()) ->
    ok | {error, Reason :: term()}.
change_password(Username, NewPassword) ->
    case verify_password(NewPassword) of
        NewPassword ->
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
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec verify_username(Username :: name()) -> ok | {error, Reason :: term()}.
verify_username(<<>>) ->
    {error, empty_username};
verify_username(Username) ->
    try
        check_pattern(check_nonempty(Username), <<":">>)
    catch
        throw:Reason -> {error, {username, Reason}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec verify_password(Username :: name()) -> ok | {error, Reason :: term()}.
verify_password(Password) ->
    MinLength = onepanel:get_env(min_password_length),
    try
        check_pattern(check_length(Password, MinLength), <<":">>)
    catch
        throw:Reason -> {error, {password, Reason}}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_nonempty(Binary :: binary()) -> Binary :: binary() | no_return().
check_nonempty(<<>>) ->
    throw(empty);

check_nonempty(Binary) ->
    Binary.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_pattern(Binary :: binary(), Pattern :: binary()) ->
    Binary :: binary() | no_return().
check_pattern(Binary, Pattern) ->
    case binary:matches(Binary, Pattern) of
        [] -> Binary;
        _ -> throw({invalid_character, ":"})
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec check_length(Binary :: binary(), MinLength :: non_neg_integer()) ->
    Binary :: binary() | no_return().
check_length(Binary, MinLength) ->
    case size(Binary) < MinLength of
        true -> throw({too_short, MinLength});
        false -> Binary
    end.
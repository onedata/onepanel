%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains user management functions.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_user).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").

-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/oz/oz_users.hrl").

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, create/1, save/1, update/2,
    upgrade/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([get_username/1, get_password_hash/1, get_by_role/1,
    any_user_exists/0, delete_all/0]).

-type name() :: binary().
-type password() :: binary().
-type password_hash() :: binary().
-type role() :: admin | regular.
-type uuid() :: binary().
-type record() :: #onepanel_user{}.

-export_type([name/0, password/0, password_hash/0, role/0, uuid/0, record/0]).

%%%===================================================================
%%% Model behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_fields/0}
%% @end
%%--------------------------------------------------------------------
-spec get_fields() -> list(atom()).
get_fields() ->
    record_info(fields, ?MODULE).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get_record_version/0}
%% @end
%%--------------------------------------------------------------------
-spec get_record_version() -> model_behaviour:version().
get_record_version() ->
    1.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:upgrade/2}
%% @end
%%--------------------------------------------------------------------
-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    no_return().
upgrade(1, _Record) ->
    error(?ERROR_NOT_SUPPORTED).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:seed/0}
%% @end
%%--------------------------------------------------------------------
-spec seed() -> any().
seed() ->
    ok.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:create/1}
%% @end
%%--------------------------------------------------------------------
-spec create(Record :: record()) ->
    {ok, name()} | {error, _} | no_return().
create(Record) ->
    model:create(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:save/1}
%% @end
%%--------------------------------------------------------------------
-spec save(Record :: record()) -> ok | no_return().
save(Record) ->
    model:save(?MODULE, Record).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:update/2}
%% @end
%%--------------------------------------------------------------------
-spec update(Key :: model_behaviour:key(), Diff :: model_behaviour:diff()) ->
    ok | no_return().
update(Key, Diff) ->
    model:update(?MODULE, Key, Diff).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:get/1}
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: model_behaviour:key()) ->
    {ok, Record :: record()} | {error, _} | no_return().
get(Key) ->
    model:get(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:exists/1}
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: model_behaviour:key()) ->
    boolean() | no_return().
exists(Key) ->
    model:exists(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:delete/1}
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: model_behaviour:key()) -> ok | no_return().
delete(Key) ->
    model:delete(?MODULE, Key).


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:list/0}
%% @end
%%--------------------------------------------------------------------
-spec list() -> Records :: [record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec get_password_hash(record()) -> password_hash().
get_password_hash(#onepanel_user{password_hash = PasswordHash}) ->
    PasswordHash.


-spec get_username(record()) -> name().
get_username(#onepanel_user{username = Username}) ->
    Username.


-spec any_user_exists() -> boolean().
any_user_exists() ->
    model:size(?MODULE) > 0.


%%--------------------------------------------------------------------
%% @doc Removes all onepanel_user records.
%% @end
%%--------------------------------------------------------------------
-spec delete_all() -> ok | {error, _}.
delete_all() ->
    model:clear(?MODULE).


%%--------------------------------------------------------------------
%% @doc Returns a list of users with the specified role.
%% @end
%%--------------------------------------------------------------------
-spec get_by_role(Role :: role()) -> Users :: [#onepanel_user{}].
get_by_role(Role) ->
    case model:exists(?MODULE) of
        true -> model:select(?MODULE,
            [{#onepanel_user{role = Role, _ = '_'}, []}]);
        false -> []
    end.

%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2017 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains session management functions.
%%% It implements {@link model_behaviour} behaviour.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_session).
-author("Krzysztof Trzepla").

-behaviour(model_behaviour).

-include("modules/errors.hrl").
-include("modules/models.hrl").

%% Model behaviour callbacks
-export([get_fields/0, seed/0, create/1, save/1, update/2, get/1, exists/1,
    delete/1, list/0]).

%% API
-export([get_id/1, get_username/1, create/2, is_active/1, mark_active/1]).

-type id() :: binary().
-type record() :: #onepanel_session{}.

-export_type([id/0]).

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
-spec create(Record :: onepanel_user:name() | record()) ->
    {ok, id()} | #error{} | no_return().
create(#onepanel_session{} = Record) ->
    model:create(?MODULE, Record);
create(<<_/binary>> = Username) ->
    Expire = get_expiration_time(),
    create(Username, Expire).


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
    {ok, Record :: record()} | #error{} | no_return().
get(Key) ->
    case model:get(?MODULE, Key) of
        {ok, Session} ->
            case is_active(Session) of
                true ->
                    {ok, Session};
                false ->
                    delete(Key),
                    ?make_error(?ERR_NOT_FOUND, ?MODULE, get, 1, [?MODULE, Key])
            end;
        #error{} = Error ->
            Error
    end.


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
-spec list() -> Records :: [model_behaviour:record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns session ID.
%% @end
%%--------------------------------------------------------------------
-spec get_id(Session :: #onepanel_session{}) -> id().
get_id(#onepanel_session{id = Id}) ->
    Id.


%%--------------------------------------------------------------------
%% @doc Returns name of a user associated with the session.
%% @end
%%--------------------------------------------------------------------
-spec get_username(Session :: #onepanel_session{}) -> id().
get_username(#onepanel_session{username = Username}) ->
    Username.


%%--------------------------------------------------------------------
%% @doc Creates user session.
%% @end
%%--------------------------------------------------------------------
-spec create(Username :: onepanel_user:name(), Expire :: non_neg_integer()) ->
    {ok, id()} | #error{} | no_return().
create(Username, Expire) ->
    create(#onepanel_session{
        id = onepanel_utils:gen_uuid(),
        username = Username,
        expire = Expire
    }).


%%--------------------------------------------------------------------
%% @doc Checks whether user session is active, i.e. it has not expired.
%% @end
%%--------------------------------------------------------------------
-spec is_active(Session :: #onepanel_session{}) -> boolean().
is_active(#onepanel_session{expire = Expire}) ->
    Expire > erlang:system_time(milli_seconds).


%%--------------------------------------------------------------------
%% @doc Marks session active, i.e. updates expiration time.
%% @end
%%--------------------------------------------------------------------
-spec mark_active(Id :: id()) -> ok | no_return().
mark_active(Id) ->
        catch update(Id, #{expire => get_expiration_time()}),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Returns future time point that occurs 'session_ttl'
%% milliseconds from now.
%% @end
%%--------------------------------------------------------------------
-spec get_expiration_time() -> TimePoint :: non_neg_integer().
get_expiration_time() ->
    TTL = onepanel_env:get(session_ttl),
    erlang:system_time(milli_seconds) + TTL.

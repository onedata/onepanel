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
-include("http/rest.hrl").
-include("authentication.hrl").

-include_lib("ctool/include/logging.hrl").

%% Model behaviour callbacks
-export([get_fields/0, get_record_version/0, seed/0, create/1, upgrade/2,
    save/1, update/2, get/1, exists/1, delete/1, list/0]).

%% API
-export([create/2, get_id/1, get_username/1, find_by_valid_token/1, is_active/1]).
-export([remove_expired_tokens/1, ensure_fresh_token/1]).

-type id() :: binary().
-type rest_api_token() :: binary().
-type record() :: #onepanel_session{}.

-export_type([id/0, rest_api_token/0]).

-define(NOW(), time_utils:system_time_seconds()).
-define(TOKEN_TTL, onepanel_env:get(rest_token_ttl)).

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
    2.


%%--------------------------------------------------------------------
%% @doc {@link model_behaviour:upgrade/2}
%% @end
%%--------------------------------------------------------------------
-spec upgrade(PreviousVsn :: model_behaviour:version(), PreviousRecord :: tuple()) ->
    {NewVsn :: model_behaviour:version(), NewRecord :: tuple()}.
upgrade(1, Record) ->
    {onepanel_session,
        Id,
        Username,
        _Expiration} = Record,
    % Session cannot be automatically upgraded for use with gui_session.
    % Set its last_refresh to 0 to trigger deletion in nearest cleanup.
    {2, {onepanel_session,
        Id,
        Username,
        _LastRefresh = 0,
        _Nonce = <<"">>,
        _PreviousNonce = <<"">>,
        _RestApiToken = <<"">>
    }}.


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
    {ok, Record :: record()} | #error{} | no_return().
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
-spec list() -> Records :: [model_behaviour:record()] | no_return().
list() ->
    model:list(?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Creates user session.
%% @end
%%--------------------------------------------------------------------
-spec create(id(), record()) ->
    ok | #error{} | no_return().
create(Id, Record) ->
    Record2 = Record#onepanel_session{id = Id, rest_tokens = [generate_api_token(Id)]},
    case create(Record2) of
        {ok, Id} -> ok;
        #error{} = Error -> Error
    end.


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
-spec get_username(Session :: #onepanel_session{}) -> onepanel_user:name().
get_username(#onepanel_session{username = Username}) ->
    Username.

%%--------------------------------------------------------------------
%% @doc Checks whether user session is active, i.e. it has not expired.
%% @end
%%--------------------------------------------------------------------
-spec is_active(Session :: #onepanel_session{}) -> boolean().
is_active(#onepanel_session{last_refresh = LastRefresh}) ->
    not gui_session:is_expired(LastRefresh).


%%--------------------------------------------------------------------
%% @doc Removes expired tokens from a session record.
%% @end
%%--------------------------------------------------------------------
-spec remove_expired_tokens(Session :: record()) ->
    record().
remove_expired_tokens(#onepanel_session{rest_tokens = Tokens} = Session) ->
    Valid = lists:filter(fun is_token_still_valid/1, Tokens),
    Session#onepanel_session{rest_tokens = Valid}.


%%--------------------------------------------------------------------
%% @doc Find session bound to given token.
%% Expired token is treated as unbound to any session.
%% @end
%%--------------------------------------------------------------------
-spec find_by_valid_token(rest_api_token()) ->
    {ok, record()} | #error{}.
find_by_valid_token(RestApiToken) ->
    SessionId = token_to_session_id(RestApiToken),
    case onepanel_session:get(SessionId) of
        {ok, #onepanel_session{rest_tokens = Tokens} = Session} ->
            case lists:keyfind(RestApiToken, 1, Tokens) of
                {RestApiToken, _} = Found ->
                    case is_token_still_valid(Found) of
                        true -> {ok, Session};
                        false -> ?make_error(?ERR_NOT_FOUND)
                    end;
                _ -> ?make_error(?ERR_NOT_FOUND)
            end;
        _ ->
            ?make_error(?ERR_NOT_FOUND)
    end.


%%--------------------------------------------------------------------
%% @doc Checks if the newest token is expired or near expiration
%% and generates new one.
%% @end
%%--------------------------------------------------------------------
-spec ensure_fresh_token(Session :: record()) -> record().
ensure_fresh_token(Session) ->
    Id = Session#onepanel_session.id,
    Tokens = Session#onepanel_session.rest_tokens,

    ShouldUpdate = case Tokens of
        [] -> true;
        [{_Newest, Expires} | _] -> is_token_near_expiration(Expires)
    end,

    case ShouldUpdate of
        true ->
            NewTokens = [generate_api_token(Id) | Tokens],
            Session2 = Session#onepanel_session{rest_tokens = NewTokens},
            ok = save(Session2),
            Session2;
        false -> Session
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec generate_api_token(SessionId :: id()) ->
    {rest_api_token(), Expires :: non_neg_integer()}.
generate_api_token(SessionId) ->
    UUID = onepanel_utils:gen_uuid(),
    Expires = ?NOW() + ?TOKEN_TTL,
    Token = onepanel_utils:join([?ONEPANEL_TOKEN_PREFIX, SessionId, UUID],
        <<?ONEPANEL_TOKEN_SEPARATOR>>),
    {Token, Expires}.


%% @private
-spec token_to_session_id(rest_api_token()) -> onepanel_session:id().
token_to_session_id(Token) ->
    [<<?ONEPANEL_TOKEN_PREFIX>>, SessionId, _] =
        string:split(Token, ?ONEPANEL_TOKEN_SEPARATOR, all),
    SessionId.


%% @private
-spec is_token_still_valid({Token :: rest_api_token(), Expires} | Expires) ->
    boolean()
    when Expires :: non_neg_integer().
is_token_still_valid(Expires) when is_integer(Expires) ->
    ?NOW() =< Expires;

is_token_still_valid({_Token, Expires}) ->
    is_token_still_valid(Expires).


%%--------------------------------------------------------------------
%% @private
%% @doc Decides if a token expires to soon to be reused.
%% @end
%%--------------------------------------------------------------------
-spec is_token_near_expiration(ExpiresAt :: non_neg_integer()) -> boolean().
is_token_near_expiration(ExpiresAt) ->
    ExpiresAt - ?NOW() < (?TOKEN_TTL div 2).

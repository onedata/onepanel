%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc Module verifying authentication of REST requests.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_auth).
-author("Wojciech Geisler").

-include("authentication.hrl").
-include("http/rest.hrl").
-include("modules/errors.hrl").
-include("modules/models.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([authenticate/2, authenticate_by_onezone_auth_token/1,
    authenticate_by_basic_auth/1, authenticate_by_onepanel_auth_token/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Authenticates user using provided authorization methods.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(Req :: cowboy_req:req(), Methods :: [fun()]) ->
    {{true, Client :: #client{}} | false, Req :: cowboy_req:req()}.
authenticate(Req, []) ->
    {false, Req};
authenticate(Req, [AuthMethod | AuthMethods]) ->
    try AuthMethod(Req) of
        {#client{} = Client, Req2} ->
            {{true, Client}, Req2};
        {#error{} = Error, Req2} ->
            {false, rest_replier:handle_error(Req2, error, Error)};
        {ignore, Req2} ->
            authenticate(Req2, AuthMethods)
    catch
        Type:Reason ->
            {false, rest_replier:handle_error(Req, Type, ?make_stacktrace(Reason))}
    end.


%%--------------------------------------------------------------------
%% @doc Authenticates user using basic authorization method.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_basic_auth(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | #error{} | ignore.
authenticate_by_basic_auth(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Basic ", Base64/binary>> ->
            {check_basic_credentials(Base64), Req};
        _ ->
            {ignore, Req}
    end.


%%--------------------------------------------------------------------
%% @doc Authenticates user using REST API token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_onepanel_auth_token(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | #error{} | ignore.
authenticate_by_onepanel_auth_token(Req) ->
    case find_auth_token(cowboy_req:headers(Req)) of
        <<?ONEPANEL_TOKEN_PREFIX, ?ONEPANEL_TOKEN_SEPARATOR, _/binary>> = OnepanelToken ->
            case onepanel_session:find_by_valid_auth_token(OnepanelToken) of
                {ok, #onepanel_session{username = ?LOCAL_SESSION_USERNAME}} ->
                    {root_client(), Req};
                #error{reason = ?ERR_NOT_FOUND} ->
                    {?make_error(?ERR_INVALID_AUTH_TOKEN), Req}
            end;
        _ ->
            {ignore, Req}
    end.


%%--------------------------------------------------------------------
%% @doc Authenticates user using REST API token.
%% @end
%%--------------------------------------------------------------------
-spec authenticate_by_onezone_auth_token(Req :: cowboy_req:req()) ->
    {Result, Req :: cowboy_req:req()}
    when Result :: #client{} | #error{} | ignore.
authenticate_by_onezone_auth_token(Req) ->
    case find_auth_token(cowboy_req:headers(Req)) of
        undefined ->
            {ignore, Req};
        AccessToken ->
            {onezone_tokens:authenticate_user(AccessToken), Req}
    end.


%%%==================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec check_basic_credentials(Credentials :: binary() | [binary()]) ->
    #client{} | #error{}.
check_basic_credentials(<<Base64/binary>>) ->
    Decoded = base64:decode(Base64),
    case binary:split(Decoded, <<":">>) of
        [Passphrase] ->
            check_emergency_passphrase(Passphrase);
        [?LOCAL_USERNAME, Passphrase] ->
            check_emergency_passphrase(Passphrase);
        [_Username, _Password] ->
            ?make_error(?ERR_INVALID_USERNAME_OR_PASSWORD)
    end.


%% @private
-spec check_emergency_passphrase(Passphrase :: binary()) ->
    #client{} | #error{}.
check_emergency_passphrase(Passphrase) ->
    case emergency_passphrase:verify(Passphrase) of
        true -> root_client();
        false -> ?make_error(?ERR_INVALID_PASSPHRASE)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Extracts authentication token from request headers.
%% @end
%%--------------------------------------------------------------------
-spec find_auth_token(cowboy:http_headers()) -> Token :: binary() | undefined.
find_auth_token(#{<<"x-auth-token">> := Token}) ->
    Token;
find_auth_token(#{<<"macaroon">> := Token}) ->
    Token;
find_auth_token(#{<<"authorization">> := <<"Bearer ", Token/binary>>}) ->
    Token;
find_auth_token(_) ->
    undefined.


-spec root_client() -> #client{}.
root_client() ->
    #client{role = root, zone_auth = onezone_client:root_auth()}.

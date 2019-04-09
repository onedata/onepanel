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
            {check_onepanel_token(OnepanelToken), Req};
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
            {zone_tokens:authenticate_user(AccessToken), Req}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec check_basic_credentials(Credentials :: binary() | [binary()]) ->
    #client{} | #error{}.
check_basic_credentials(<<Base64/binary>>) ->
    Decoded = base64:decode(Base64),
    check_basic_credentials(binary:split(Decoded, <<":">>));

check_basic_credentials([Username, Password]) ->
    case onepanel_user:authenticate_by_basic_auth(Username, Password) of
        {ok, User} -> user_to_client(User);
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @private @doc Verifies auth token generated by onepanel.
%% @end
%%--------------------------------------------------------------------
-spec check_onepanel_token(Token :: binary()) -> #client{} | #error{}.
check_onepanel_token(Token) ->
    case onepanel_user:authenticate_by_auth_token(Token) of
        {ok, User} -> user_to_client(User);
        Error -> Error
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


%% @private
-spec user_to_client(#onepanel_user{}) -> #client{}.
user_to_client(#onepanel_user{username = Username, role = admin}) ->
    (root_client())#client{role = admin,
        user = #user_details{id = <<>>, name = Username}};

user_to_client(#onepanel_user{username = Username, role = Role}) ->
    #client{user = #user_details{id = <<>>, name = Username}, role = Role}.


%% @private
-spec root_client() -> #client{}.
root_client() ->
    ZoneAuth = try zone_client:root_auth() catch _:_ -> undefined end,
    #client{role = root, zone_auth = ZoneAuth}.

%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @author Bartosz Walkowicz
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% TODO VFS-5621
%%% This module handles translation of request results to REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(rest_translator).
-author("Lukasz Opiola").
-author("Bartosz Walkowicz").
-author("Wojciech Geisler").

-include("http/rest.hrl").
-include_lib("ctool/include/http/headers.hrl").

%% API
-export([error_response/1]).
-export([make_location_header/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec error_response(errors:error()) -> #rest_resp{}.
error_response({error, _} = Error) ->
    #rest_resp{
        code = errors:to_http_code(Error),
        headers = #{?HDR_CONTENT_TYPE => <<"application/json">>},
        body = #{<<"error">> => errors:to_json(Error)}
    }.


%%--------------------------------------------------------------------
%% @doc
%% Creates headers map containing a Location header constructed
%% from given tokens.
%% @end
%%--------------------------------------------------------------------
-spec make_location_header(PathTokens :: [binary()]) -> cowboy:http_headers().
% Make sure there is no leading slash (so filename can be used for joining path)
make_location_header([<<"/", Path/binary>> | Tail]) ->
    make_location_header([Path | Tail]);

make_location_header(PathTokens) ->
    Prefix = https_listener:get_prefix(),
    Location = filename:join([Prefix | PathTokens]),
    #{?HDR_LOCATION => unicode:characters_to_binary(Location)}.

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
-include("middleware/middleware.hrl").
-include("names.hrl").
-include_lib("ctool/include/errors.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").
-include_lib("ctool/include/http/headers.hrl").

%% API
-export([response/2, error_response/1]).
-export([make_location_header/1]).


%%%===================================================================
%%% API
%%%===================================================================


-spec response(_, middleware:result()) -> #rest_resp{}.
response(_, {error, _} = Error) ->
    error_response(Error);

response(#onp_req{operation = create}, ok) ->
    % No need for translation, 'ok' means success with no response data
    ?NO_CONTENT_REPLY;
response(#onp_req{operation = create}, {ok, value, GRI, Result}) ->
    #gri{type = Model} = GRI,
    Translator = entity_type_to_translator(Model),
    Translator:create_response(GRI, value, Result);

response(#onp_req{operation = create}, {ok, resource, {GRI, Result}}) ->
    #gri{type = Model} = GRI,
    Translator = entity_type_to_translator(Model),
    Translator:create_response(GRI, resource, {GRI, Result});

response(#onp_req{operation = create} = OnpReq, {ok, value, Result}) ->
    #onp_req{gri = GRI = #gri{type = Model}} = OnpReq,
    Translator = entity_type_to_translator(Model),
    Translator:create_response(GRI, value, Result);

response(#onp_req{operation = get} = OnpReq, {ok, Data}) ->
    #onp_req{gri = GRI = #gri{type = Model}} = OnpReq,
    Translator = entity_type_to_translator(Model),
    Translator:get_response(GRI, Data);
response(#onp_req{operation = get} = OnpReq, {ok, value, Data}) ->
    response(OnpReq, {ok, Data});

response(#onp_req{operation = update}, ok) ->
    ?NO_CONTENT_REPLY;
response(#onp_req{operation = update} = OnpReq, {ok, value, Result}) ->
    #onp_req{gri = GRI = #gri{type = Model}} = OnpReq,
    Translator = entity_type_to_translator(Model),
    Translator:update_response(GRI, Result);

response(#onp_req{operation = delete}, ok) ->
    ?NO_CONTENT_REPLY.


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


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
-spec entity_type_to_translator(atom()) -> module().
entity_type_to_translator(onp_cluster) -> cluster_rest_translator;
entity_type_to_translator(onp_host) -> host_rest_translator;
entity_type_to_translator(onp_panel) -> onepanel_rest_translator;
entity_type_to_translator(onp_provider) -> provider_rest_translator;
entity_type_to_translator(onp_service) -> service_rest_translator;
entity_type_to_translator(onp_space) -> space_rest_translator;
entity_type_to_translator(onp_storage) -> storage_rest_translator;
entity_type_to_translator(onp_user) -> user_rest_translator;
entity_type_to_translator(onp_zone) -> zone_rest_translator;
entity_type_to_translator(_) -> error(not_implemented).

%%%-------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C): 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module contains helper functions for modules implementing
%%% rest_module_behavior.
%%% @see rest_module_behavior
%%% @end
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([get_method/1, get_bindings/1, get_params/2, get_args/2]).
-export([handle_errors/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts REST method from binary to an atom representation.
%%--------------------------------------------------------------------
-spec get_method(Req :: cowboy_req:req()) ->
    {Method :: rest_handler:method_type(), Req :: cowboy_req:req()}.
get_method(Req) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, Req2} -> {'POST', Req2};
        {<<"PATCH">>, Req2} -> {'PATCH', Req2};
        {<<"GET">>, Req2} -> {'GET', Req2};
        {<<"PUT">>, Req2} -> {'PUT', Req2};
        {<<"DELETE">>, Req2} -> {'DELETE', Req2}
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_bindings(Req :: cowboy_req:req()) ->
    {Bindings :: rest_handler:bindings(), Req :: cowboy_req:req()}.
get_bindings(Req) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    NewBindings = lists:map(fun
        ({host, Value}) -> {host, erlang:binary_to_list(Value)};
        ({Key, Value}) -> {Key, Value}
    end, Bindings),
    {maps:from_list(NewBindings), Req2}.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_params(Req :: cowboy_req:req(), ParamsSpec :: rest_handler:spec()) ->
    {Params :: rest_handler:params(), Req :: cowboy_req:req()}.
get_params(Req, ParamsSpec) ->
    {Params, Req2} = cowboy_req:qs_vals(Req),
    NewParams = lists:map(fun
        ({Key, true}) -> {Key, <<"true">>};
        ({Key, Value}) -> {Key, Value}
    end, Params),
    try
        {onepanel_parser:parse(NewParams, ParamsSpec), Req2}
    catch
        throw:#error{reason = {?ERR_MISSING_KEY, Keys}} = Error ->
            ?throw(Error#error{reason = {?ERR_MISSING_PARAM, Keys}})
    end.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_args(Data :: rest_handler:data(), ArgsSpec :: rest_handler:spec()) ->
    Args :: rest_handler:args() | no_return().
get_args(Data, ArgsSpec) ->
    onepanel_parser:parse(Data, ArgsSpec).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec handle_errors(Req :: cowboy_req:req(), Type :: atom(), Reason :: term()) ->
    Req :: cowboy_req:req() | no_return().
handle_errors(Req, Type, Reason) ->
    {Name, Description} = onepanel_errors:translate(Type, Reason),
    Body = json_utils:encode([
        {<<"error">>, Name},
        {<<"description">>, Description}
    ]),
    cowboy_req:set_resp_body(Body, Req).
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
-export([get_method/1, get_bindings/1, get_params/1, get_args/2]).
-export([handle_errors/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Converts REST method from binary to an atom representation.
%%--------------------------------------------------------------------
-spec get_method(Req :: cowboy_req:req()) ->
    {Method :: rest_handler:method(), Req :: cowboy_req:req()}.
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
-spec get_params(Req :: cowboy_req:req()) ->
    {Params :: rest_handler:params(), Req :: cowboy_req:req()}.
get_params(Req) ->
    {Params, Req2} = cowboy_req:qs_vals(Req),
    {maps:from_list(Params), Req2}.


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec get_args(Data :: rest_handler:data(), ArgsSpec :: rest_handler:args_spec()) ->
    Args :: rest_handler:args() | no_return().
get_args(Data, ArgsSpec) ->
    maps:fold(fun(Key, {Type, Required}, Args) ->
        BinKey = erlang:atom_to_binary(Key, utf8),
        case {lists:keyfind(BinKey, 1, Data), Required} of
            {{BinKey, Value}, _} ->
                maps:put(Key, convert(BinKey, Value, Type), Args);
            {_, false} ->
                Args;
            {_, _} ->
                ?throw({?ERR_MISSING_REQUIRED_KEY, BinKey})
        end
    end, #{}, ArgsSpec).


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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec convert(Key :: binary(), Value :: term(), Type :: atom()) ->
    Term :: term().
convert(Key, Value, Type) when is_integer(Value) ->
    convert(Key, erlang:integer_to_binary(Value), Type);

convert(Key, Value, Type) when is_float(Value) ->
    convert(Key, erlang:float_to_binary(Value), Type);

convert(_Key, Value, string) ->
    Value;

convert(Key, Value, atom = Type) ->
    convert(Key, Value, Type, fun(V) -> erlang:binary_to_atom(V, utf8) end);

convert(Key, Value, integer = Type) ->
    convert(Key, Value, Type, fun(V) -> erlang:binary_to_integer(V) end);

convert(Key, Value, float = Type) ->
    convert(Key, Value, Type, fun(V) -> erlang:binary_to_float(V) end).


%%--------------------------------------------------------------------
%% @doc
%% @todo write me!
%% @end
%%--------------------------------------------------------------------
-spec convert(Key :: binary(), Value :: binary(), Type :: atom(), Fun :: fun()) ->
    Term :: term() | no_return().
convert(Key, Value, Type, Fun) ->
    try
        Fun(Value)
    catch
        _:_ -> ?throw({?ERR_INVALID_VALUE_TYPE, Key,
            erlang:atom_to_binary(Type, utf8)})
    end.
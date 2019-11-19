%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains error handling functions.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_errors).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").

%% API
-export([translate/2, format_error/1]).

-define(FORMAT_NONEMPTY(Variable), case Variable of
    undefined -> "";
    [] -> "";
    _ -> io_lib:format("~ts: ~tp~n", [??Variable, Variable])
end).


%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Converts error to a human-readable description.
%% @end
%%--------------------------------------------------------------------
-spec translate(Type :: atom(), Reason :: term()) ->
    {Name :: binary(), Description :: binary()} | no_return().
% DO NOT modify this error message as it is used to identify the error in GUI
translate(_Type, {error, {error, {Code, Error, Description}}})
    when is_integer(Code), is_binary(Error), is_binary(Description) ->
    {<<"Operation Error">>, Error};

translate(_Type, {error, _} = Error) ->
    try
        #{<<"id">> := Id, <<"description">> := Desc} = describe_common_error(Error),
        {<<"Operation error">>, str_utils:format_bin("~ts: ~ts", [Id, Desc])}
    catch _:_ ->
        ?error("~ts", [format_error(Error)]),
        {<<"Internal Error">>, <<"Server encountered an unexpected error.">>}
    end;

translate(Type, Reason) ->
    ?error("Type: ~tp~nReason: ~tp~n", [Type, Reason]),
    erlang:raise(Type, Reason, []).


-spec format_error({error, _}) -> list().
format_error({error, Reason}) ->
    io_lib:format("~tp", [Reason]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private @doc Attempts to match the error to one of the standard
%% errors defined in ctool and return the error description map.
%% Raises on failure.
%% @end
%%--------------------------------------------------------------------
-spec describe_common_error({error, _} | errors:error() | errors:reason()) ->
    errors:as_json() | no_return().
describe_common_error({error, _} = Error) ->
    errors:to_json(Error);

describe_common_error(Error) ->
    errors:to_json({error, Error}).

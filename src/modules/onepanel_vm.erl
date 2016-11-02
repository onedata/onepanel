%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This module contains utility functions for an Erlang virtual machine
%%% configuration file (vm.args) management.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_vm).
-author("Krzysztof Trzepla").

-include("modules/errors.hrl").

%% API
-export([read/2, write/3, get/2, set/3]).

-type key() :: string() | binary().
-type value() :: term().

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Returns value of an Erlang virtual machine property from a file.
%% Throws an exception if value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec read(Key :: key(), Path :: file:name_all()) ->
    {ok, Value :: binary()} | #error{} | no_return().
read(Key, Path) ->
    case file:read_file(Path) of
        {ok, Content} -> get(Key, Content);
        {error, Reason} -> ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Modifies an Erlang virtual machine configuration file (vm.args) by
%% reading its contents, evaluating {@link set/3} on it and writing updated
%% content back to the file.
%% @end
%%--------------------------------------------------------------------
-spec write(Key :: key(), Value :: value(), Path :: file:name_all()) ->
    ok | no_return().
write(Key, Value, Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, NewContent} = set(Key, Value, Content),
            case file:write_file(Path, NewContent) of
                ok -> ok;
                {error, Reason} -> ?throw_error(Reason)
            end;
        {error, Reason} ->
            ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Returns value of an Erlang virtual machine property from a file content.
%% Throws an exception if value has not been found.
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: key(), Content :: binary()) ->
    {ok, Value :: binary()} | #error{} | no_return().
get(Key, Content) ->
    Pattern = <<"^\\-", Key/binary, ".*">>,
    Options = [{capture, first, binary}, multiline],
    BinKey = onepanel_utils:convert(Key, binary),
    KeySize = size(BinKey),
    case re:run(Content, Pattern, Options) of
        {match, [<<"-", BinKey:KeySize/binary, Value/binary>>]} ->
            {ok, onepanel_utils:trim(Value, both)};
        nomatch ->
            ?make_error(?ERR_NOT_FOUND);
        {error, Reason} ->
            ?throw_error(Reason)
    end.


%%--------------------------------------------------------------------
%% @doc Sets value of an Erlang virtual machine property in provided file content.
%% @end
%%--------------------------------------------------------------------
-spec set(Key :: key(), Value :: value(), Content :: binary()) ->
    {ok, NewContent :: binary()} | no_return().
set(Key, Value, Content) ->
    BinKey = onepanel_utils:convert(Key, binary),
    BinValue = onepanel_utils:convert(Value, binary),
    case get(BinKey, Content) of
        {ok, _} ->
            Pattern = <<"^\\-", BinKey/binary, ".*">>,
            Options = [{return, binary}, multiline],
            {ok, re:replace(Content, Pattern,
                <<"-", BinKey/binary, " ", BinValue/binary>>, Options)};
        #error{reason = ?ERR_NOT_FOUND} ->
            {ok, <<Content/binary, "\n-", BinKey/binary, " ", BinValue/binary>>}
    end.
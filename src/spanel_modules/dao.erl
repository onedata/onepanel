%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains database management functions
%% @end
%% ===================================================================
-module(dao).

-include("spanel_modules/dao.hrl").

%% API
-export([init/0]).

init() ->
  try
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    {atomic, ok} = mnesia:create_table(users, [
      {attributes,record_info(fields, user)},
      {record_name, user},
      {disc_copies, [node()]}
    ]),
    {atomic, ok} = mnesia:create_table(configurations, [
      {attributes, record_info(fields, configuration)},
      {record_name, configuration},
      {disc_copies, [node()]}
    ]),
    ok
  catch
     _:_ -> error
  end.
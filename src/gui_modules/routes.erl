%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module provides mapping of onepanel paths to modules
%% that will render the pages.
%% @end
%% ===================================================================

-module(routes).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.

init(State, Ctx) ->
  Path = wf:path(Ctx#context.req),
  RequestedPage = case Path of
                    <<"/ws", Rest/binary>> -> Rest;
                    Other -> Other
                  end,
  {ok, State, Ctx#context{path = Path, module = route(RequestedPage)}}.

route(<<"/">>) -> page_installation;
route(<<"/installation">>) -> page_installation;
route(<<"/login">>) -> page_login;
route(<<"/logout">>) -> page_logout;
route(<<"/error">>) -> page_error;
route(<<"/about">>) -> page_about;
route(<<"/manage_account">>) -> page_manage_account;
route(_) -> page_404.
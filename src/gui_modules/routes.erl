%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module provides mapping of Onepanel paths to modules
%% that will render the pages.
%% @end
%% ===================================================================

-module(routes).
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/2
%% ====================================================================
%% @doc Initializes routing state and context.
-spec init(State :: term(), Ctx :: #context{}) -> Result when
    Result :: {ok, State :: term(), Ctx :: #context{}}.
%% ====================================================================
init(State, Ctx) ->
    Path = wf:path(Ctx#context.req),
    RequestedPage = case Path of
                        <<"/ws", Rest/binary>> -> Rest;
                        Other -> Other
                    end,
    {ok, State, Ctx#context{path = Path, module = route(RequestedPage)}}.


%% finish/2
%% ====================================================================
%% @doc Finalizes routing state and context.
-spec finish(State :: term(), Ctx :: #context{}) -> Result when
    Result :: {ok, State :: term(), Ctx :: #context{}}.
%% ====================================================================
finish(State, Ctx) -> {ok, State, Ctx}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% route/1
%% ====================================================================
%% @doc Returns modules that renders pages for given resource.
-spec route(Resource :: string()) -> Result when
    Result :: module().
%% ====================================================================
route(<<"/">>) -> page_installation;
route(<<"/installation">>) -> page_installation;
route(<<"/hosts_selection">>) -> page_hosts_selection;
route(<<"/main_ccm_selection">>) -> page_main_ccm_selection;
route(<<"/ulimits">>) -> page_ulimits;
route(<<"/add_storage">>) -> page_add_storage;
route(<<"/installation_summary">>) -> page_installation_summary;
route(<<"/installation_success">>) -> page_installation_success;
route(<<"/login">>) -> page_login;
route(<<"/logout">>) -> page_logout;
route(<<"/about">>) -> page_about;
route(<<"/manage_account">>) -> page_manage_account;
route(<<"/validate_login">>) -> page_validate_login;
route(<<"/registration">>) -> page_registration;
route(<<"/connection_check">>) -> page_connection_check;
route(_) -> page_404.
%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module provides mapping of onepanel paths to modules
%% that will render the pages.
%% @end
%% ===================================================================
-module(routes).

-include("gui_modules/common.hrl").
-include_lib("n2o/include/wf.hrl").

-export([init/2, finish/2]).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/2
%% ====================================================================
%% @doc Initializes routing state and context.
%% @end
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
%% @end
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
%% @end
-spec route(Resource :: string()) -> Result when
    Result :: module().
%% ====================================================================
%% Root page
route(?PAGE_ROOT) -> page_installation;

%% Management pages
route(?PAGE_LOGIN) -> page_login;
route(?PAGE_LOGIN_VALIDATION) -> page_login_validation;
route(?PAGE_LOGOUT) -> page_logout;
route(?PAGE_ABOUT) -> page_about;
route(?PAGE_ERROR) -> page_error;
route(?PAGE_ACCOUNT_SETTINGS) -> page_account_settings;
route(?PAGE_PRIVACY_POLICY) -> page_privacy_policy;

%% Installation pages
route(?PAGE_INSTALLATION) -> page_installation;
route(?PAGE_HOST_SELECTION) -> page_hosts_selection;
route(?PAGE_APP_PORTS_CHECK) -> page_app_ports_check;
route(?PAGE_SYSTEM_LIMITS) -> page_system_limits;
route(?PAGE_INSTALLATION_SUMMARY) -> page_installation_summary;
route(?PAGE_INSTALLATION_SUCCESS) -> page_installation_success;

%% Undefined pages
route(_) -> page_404.
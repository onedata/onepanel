%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains useful functions commonly used in control_panel modules.
%% @end
%% ===================================================================

-module(gui_utils).
-include_lib("n2o/include/wf.hrl").
-include("gui_modules/custom_elements.hrl").

-export([get_requested_page/0]).
-export([user_logged_in/0]).
-export([redirect_to_login/1, redirect_from_login/0]).
-export([top_menu/1, top_menu/2, logotype_footer/1]).
-export([update/2, insert_bottom/2, insert_before/2]).
-export([to_list/1]).
-export([script_for_enter_submission/2]).
-export([comet/1, init_comet/2, comet_supervisor/2, is_comet_process/0, flush/0]).

%% ====================================================================
%% API functions
%% ====================================================================

%% user_logged_in/0
%% ====================================================================
%% @doc Checks if the client has a valid login session.
%% @end
-spec user_logged_in() -> boolean().
%% ====================================================================
user_logged_in() ->
  (wf:user() /= undefined).

%% get_requested_page/0
%% ====================================================================
%% @doc Returns the page requested by the client.
%% @end
-spec get_requested_page() -> binary().
%% ====================================================================
get_requested_page() ->
  Path = wf:path(?REQ),
  case Path of
    <<"/ws", Page/binary>> -> Page;
    <<Page/binary>> -> Page
  end.

%% redirect_to_login/1
%% ====================================================================
%% @doc Redirects to login page. Can remember the source page, so that
%% a user can be redirected back after logging in.
%% @end
-spec redirect_to_login(boolean()) -> ok.
%% ====================================================================
redirect_to_login(SaveSourcePage) ->
  PageName = get_requested_page(),
  case SaveSourcePage of
    false -> wf:redirect(<<"/login">>);
    true -> wf:redirect(<<"/login?x=", PageName/binary>>)
  end.

%% redirect_from_login/0
%% ====================================================================
%% @doc Redirects back from login page to the originally requested page.
%% If it hasn't been stored before, redirects to index page.
%% @end
-spec redirect_from_login() -> ok.
%% ====================================================================
redirect_from_login() ->
  case wf:q(<<"x">>) of
    undefined -> wf:redirect(<<"/">>);
    TargetPage -> wf:redirect(TargetPage)
  end.

%% top_menu/1
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% @end
-spec top_menu(ActiveTabID :: any()) -> list().
%% ====================================================================
top_menu(ActiveTabID) ->
  top_menu(ActiveTabID, []).

%% top_menu/2
%% ====================================================================
%% @doc Convienience function to render top menu in GUI pages.
%% Item with ActiveTabID will be highlighted as active.
%% Submenu body (list of n2o elements) will be concatenated below the main menu.
%% @end
-spec top_menu(ActiveTabID :: any(), SubMenuBody :: any()) -> list().
%% ====================================================================
top_menu(ActiveTabID, SubMenuBody) ->
  % Define menu items with ids, so that proper tab can be made active via function parameter
  % see old_menu_captions()
  MenuCaptions =
    [
      {installation_tab, #li{body = [
        #link{style = <<"padding: 18px;">>, url = <<"/installation">>, body = <<"Installation">>}
      ]}}
    ],

  MenuIcons =
    [
      {manage_account_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Manage account">>,
        url = <<"/manage_account">>, body = [list_to_binary(wf:user()), #span{class = <<"fui-user">>,
          style = <<"margin-left: 10px;">>}]}}},
      {about_tab, #li{body = #link{style = <<"padding: 18px;">>, title = <<"About">>,
        url = <<"/about">>, body = #span{class = <<"fui-info">>}}}},
      {logout_button, #li{body = #link{style = <<"padding: 18px;">>, title = <<"Log out">>,
        url = <<"/logout">>, body = #span{class = <<"fui-power">>}}}}
    ],

  MenuCaptionsProcessed = lists:map(
    fun({TabID, ListItem}) ->
      case TabID of
        ActiveTabID -> ListItem#li{class = <<"active">>};
        _ -> ListItem
      end
    end, MenuCaptions),

  MenuIconsProcessed = lists:map(
    fun({TabID, ListItem}) ->
      case TabID of
        ActiveTabID -> ListItem#li{class = <<"active">>};
        _ -> ListItem
      end
    end, MenuIcons),

  #panel{class = <<"navbar navbar-fixed-top">>, body = [
    #panel{class = <<"navbar-inner">>, style = <<"border-bottom: 2px solid gray;">>, body = [
      #panel{class = <<"container">>, body = [
        #list{class = <<"nav pull-left">>, body = MenuCaptionsProcessed},
        #list{class = <<"nav pull-right">>, body = MenuIconsProcessed}
      ]}
    ]}
  ] ++ SubMenuBody}.

%% logotype_footer/1
%% ====================================================================
%% @doc Convienience function to render logotype footer, coming after page content.
%% @end
-spec logotype_footer(MarginTop :: integer()) -> list().
%% ====================================================================
logotype_footer(MarginTop) ->
  Height = integer_to_binary(MarginTop + 82),
  Margin = integer_to_binary(MarginTop),
  [
    #panel{style = <<"position: relative; height: ", Height/binary, "px;">>, body = [
      #panel{style = <<"text-align: center; z-index: -1; margin-top: ", Margin/binary, "px;">>, body = [
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/innow-gosp-logo.png">>},
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/plgrid-plus-logo.png">>},
        #image{style = <<"margin: 10px 100px;">>, image = <<"/images/unia-logo.png">>}
      ]}
    ]}
  ].

%% script_for_enter_submission/2
%% ====================================================================
%% @doc Generates snippet of javascript, which can be directly used with wf:wire.
%% It intercepts enter keypresses on given input element and performs a click() on given submit button.
%% @end
-spec script_for_enter_submission(string(), string()) -> string().
%% ====================================================================
script_for_enter_submission(InputID, ButtonToClickID) ->
  wf:f("$('#~s').bind('keydown', function (e){ if (e.which == 13) { e.preventDefault(); $('#~s').click(); } });", [InputID, ButtonToClickID]).

%% update/2
%% ====================================================================
%% @doc Updates contents of a DOM element.
%% @end
-spec update(term(), term()) -> ok.
%% ====================================================================
update(Target, Elements) -> wf:wire(#jquery{target = Target, method = ["html"], args = [Elements], format = "'~s'"}).

%% insert_bottom/2
%% ====================================================================
%% @doc Appends an element to a DOM element.
%% @end
-spec insert_bottom(term(), term()) -> ok.
%% ====================================================================
insert_bottom(Target, Elements) ->
  wf:wire(#jquery{target = Target, method = ["append"], args = [Elements], format = "'~s'"}).

%% insert_before/2
%% ====================================================================
%% @doc Inserts an element before a DOM element.
%% @end
-spec insert_before(term(), term()) -> ok.
%% ====================================================================
insert_before(Target, Elements) ->
  wf:wire(#jquery{target = Target, method = ["before"], args = [Elements], format = "'~s'"}).

%% to_list/1
%% ====================================================================
%% @doc Converts any term to list.
%% @end
-spec to_list(term()) -> list().
%% ====================================================================
to_list(undefined) -> [];
to_list(Term) when is_list(Term) -> Term;
to_list(Term) when is_binary(Term) -> binary_to_list(Term);
to_list(Term) ->
  try
    wf:to_list(Term)
  catch _:_ ->
    lists:flatten(io_lib:format("~p", [Term]))
  end.

%% comet/1
%% ====================================================================
%% @doc Spawns an asynchronous process connected to the calling process.
%% IMPORTANT! The calling process must be the websocket process of n2o framework.
%% In other words, it should be called from event/1 function of page module.
%% Allows flushing actions to the main process (async updates).
%% Every instance of comet will get a supervisor to make sure it won't go rogue
%% after the calling process has finished.
%% @end
-spec comet(function()) -> {ok, pid()} | no_return().
%% ====================================================================
comet(CometFun) ->
  % Prevent comet and supervisor from killing the calling process on crash
  process_flag(trap_exit, true),
  % Spawn comet process, _link so it will die if the calling process craches
  CometPid = spawn_link(?MODULE, init_comet, [self(), CometFun]),
  % Spawn comet supervisor, _link so it will die if the calling process craches
  spawn_link(?MODULE, comet_supervisor, [self(), CometPid]),
  {ok, CometPid}.


%% init_comet/2
%% ====================================================================
%% @doc Internal function used to initialize an asynchronous "comet" process.
%% @end
-spec init_comet(pid(), fun()) -> no_return().
%% ====================================================================
init_comet(OwnerPid, Fun) ->
  timer:sleep(100), % defer the comet process so that n2o websocket process can initialize
  put(ws_process, OwnerPid),
  wf_context:init_context([]),
  Fun().


%% comet_supervisor/2
%% ====================================================================
%% @doc Internal function evaluated by comet supervisor. The supervisor will
%% kill the comet process whenever comet creator process finishes.
%% @end
-spec comet_supervisor(pid(), pid()) -> no_return().
%% ====================================================================
comet_supervisor(CallingPid, CometPid) ->
  MonitorRef = erlang:monitor(process, CallingPid),
  receive
    {'DOWN', MonitorRef, _, _, _} -> exit(CometPid, kill)
  end.


%% is_comet_process/0
%% ====================================================================
%% @doc Returns true if calling process is a comet process.
%% @end
-spec is_comet_process() -> boolean().
%% ====================================================================
is_comet_process() ->
  get(ws_process) /= undefined.


%% flush/0
%% ====================================================================
%% @doc Flushes accumulated events to websocket process, causing page update.
%% @end
-spec flush() -> ok.
%% ====================================================================
flush() ->
  Actions = wf_context:actions(),
  wf_context:clear_actions(),
  case Actions of
    [] ->
      skip;
    undefined ->
      skip;
    _ ->
      get(ws_process) ! {flush, Actions}
  end,
  ok.
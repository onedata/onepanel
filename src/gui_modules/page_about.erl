%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license 
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code
%% @end
%% ===================================================================

-module(page_about).
-compile(export_all).
-include("gui_modules/common.hrl").

-define(LICENSE_FILE, "LICENSE.txt").
-define(CONTACT_EMAIL, "support@onedata.org").

%% Template points to the template file, which will be filled with content
main() -> case gui_utils:user_logged_in() of
            true ->
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]};
            false ->
              gui_utils:redirect_to_login(true),
              #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}]}
          end.

%% Page title
title() -> <<"About">>.

%% This will be placed in the template instead of {{body}} tag
body() ->
  #panel{style = <<"position: relative;">>, body = [
    gui_utils:top_menu(about_tab),
    #panel{style = <<"margin-top: 60px; padding: 20px;">>, body = [
      #panel{id = <<"about_table">>, body = about_table()}
    ]}
  ]}.

about_table() ->
  #table{style = <<"border-width: 0px; width: auto">>, body = [
    #tr{cells = [
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Contact">>}},
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #link{style = <<"font-size: 18px; padding: 5px 0;">>, body = <<?CONTACT_EMAIL>>, url = <<"mailto:", ?CONTACT_EMAIL>>}}
    ]},

    #tr{cells = [
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Acknowledgements">>}},
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #p{body = <<"This research was supported in part by PL-Grid Infrastructure.">>}}
    ]},

    #tr{cells = [
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"License">>}},
      #td{style = <<"border-width: 0px; padding: 10px 10px">>,
        body = #p{style = <<"white-space: pre; font-size: 100%; line-height: normal">>, body = get_license()}}
    ]},

    #tr{cells = [
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body =
      #label{class = <<"label label-large label-inverse">>, style = <<"cursor: auto;">>, body = <<"Team">>}},
      #td{style = <<"border-width: 0px; padding: 10px 10px">>, body = get_team()}
    ]}
  ]}.

% content of LICENSE.txt file
get_license() ->
  case file:read_file(?LICENSE_FILE) of
    {ok, File} -> binary:bin_to_list(File);
    {error, Error} -> Error
  end.

% HTML list with team members
get_team() ->
  Members = [<<"Łukasz Dutka">>, <<"Jacek Kitowski">>, <<"Dariusz Król">>, <<"Tomasz Lichoń">>, <<"Darin Nikolow">>,
    <<"Łukasz Opioła">>, <<"Tomasz Pałys">>, <<"Bartosz Polnik">>, <<"Paweł Salata">>, <<"Michał Sitko">>,
    <<"Rafał Słota">>, <<"Renata Słota">>, <<"Beata Skiba">>, <<"Krzysztof Trzepla">>, <<"Michał Wrzeszcz">>],
  #list{numbered = false, body =
  lists:map(
    fun(Member) ->
      #li{style = <<"font-size: 18px; padding: 5px 0;">>, body = Member}
    end, Members)
  }.

event(init) -> ok.
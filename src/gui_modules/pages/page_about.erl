%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page contains information about the project, licence and contact for support.
%% @end
%% ===================================================================

-module(page_about).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-define(LICENSE_FILE, "LICENSE.txt").
-define(CONTACT_EMAIL, "support@onedata.org").
-define(MEMBERS, [<<"Łukasz Dutka"/utf8>>, <<"Jacek Kitowski"/utf8>>, <<"Dariusz Król"/utf8>>, <<"Tomasz Lichoń"/utf8>>, <<"Darin Nikolow"/utf8>>,
    <<"Łukasz Opioła"/utf8>>, <<"Tomasz Pałys"/utf8>>, <<"Bartosz Polnik"/utf8>>, <<"Paweł Salata"/utf8>>, <<"Michał Sitko"/utf8>>,
    <<"Rafał Słota"/utf8>>, <<"Renata Słota"/utf8>>, <<"Beata Skiba"/utf8>>, <<"Krzysztof Trzepla"/utf8>>, <<"Michał Wrzeszcz"/utf8>>]).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
        _ ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"About">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{style = <<"position: relative;">>, body = [
        onepanel_gui_utils:top_menu(about_tab),
        #panel{style = <<"margin-top: 60px; padding: 20px;">>, body = [
            #panel{id = <<"about_table">>, body = about_table()}
        ]}
    ] ++ onepanel_gui_utils:logotype_footer(120)}.


%% about_table/0
%% ====================================================================
%% @doc Renders the body of about table
-spec about_table() -> Result when
    Result :: #table{}.
%% ====================================================================
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


%% get_license/0
%% ====================================================================
%% @doc Returns content of LICENSE.txt file
-spec get_license() -> Result when
    Result :: binary().
%% ====================================================================
get_license() ->
    case file:read_file(?LICENSE_FILE) of
        {ok, File} -> File;
        {error, Reason} ->
            ?error("Cannot get license file ~s: ~p", [?LICENSE_FILE, Reason]),
            <<"">>
    end.


%% get_team/0
%% ====================================================================
%% @doc Returns HTML list with team members
-spec get_team() -> Result when
    Result :: list().
%% ====================================================================
get_team() ->
    #list{numbered = false, body = lists:map(
        fun(Member) ->
            #li{style = <<"font-size: 18px; padding: 5px 0;">>, body = Member}
        end, ?MEMBERS)
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    ok;

event(terminate) ->
    ok.
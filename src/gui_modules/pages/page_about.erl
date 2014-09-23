%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page contains information about the project, licence and contact for support.
%% @end
%% ===================================================================
-module(page_about).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

-define(LICENSE_FILE, "LICENSE.txt").
-define(CONTACT_EMAIL, "support@onedata.org").
-define(MEMBERS, [<<"Łukasz Dutka"/utf8>>, <<"Jacek Kitowski"/utf8>>, <<"Dariusz Król"/utf8>>, <<"Tomasz Lichoń"/utf8>>, <<"Darin Nikolow"/utf8>>,
    <<"Łukasz Opioła"/utf8>>, <<"Bartosz Polnik"/utf8>>, <<"Paweł Salata"/utf8>>, <<"Michał Sitko"/utf8>>, <<"Rafał Słota"/utf8>>,
    <<"Renata Słota"/utf8>>, <<"Beata Skiba"/utf8>>, <<"Krzysztof Trzepla"/utf8>>, <<"Michał Wrzeszcz"/utf8>>, <<"Konrad Zemek"/utf8>>]).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
%% @end
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]};
        _ ->
            gui_jq:redirect_to_login(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"About">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils_adapter:top_menu(about_tab),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"About">>
            },
            about_table()
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% about_table/0
%% ====================================================================
%% @doc Renders the body of about table.
%% @end
-spec about_table() -> Result when
    Result :: #table{}.
%% ====================================================================
about_table() ->
    DescriptionStyle = <<"border-width: 0; vertical-align: top; text-align: right; padding: 1em 1em;">>,
    MainStyle = <<"border-width: 0;  text-align: left; padding: 1em 1em;">>,
    #table{
        style = <<"border-width: 0; width: auto;">>,
        body = lists:map(fun({DescriptionBody, MainBody}) ->
            #tr{
                cells = [
                    #td{
                        style = DescriptionStyle,
                        body = #label{
                            class = <<"label label-large label-inverse">>,
                            style = <<"cursor: auto;">>,
                            body = DescriptionBody
                        }
                    },
                    #td{
                        style = MainStyle,
                        body = #p{
                            style = <<"margin: 0;">>,
                            body = MainBody
                        }
                    }
                ]
            }
        end, [
            {<<"Version">>, version()},
            {<<"Contact">>, contact()},
            {<<"Privacy policy">>, privacy_policy()},
            {<<"Acknowledgements">>, acknowledgements()},
            {<<"License">>, license()},
            {<<"Team">>, team()}
        ])
    }.


%% version/0
%% ====================================================================
%% @doc Renders application version.
%% @end
-spec version() -> Result when
    Result :: #p{}.
%% ====================================================================
version() ->
    #p{
        style = <<"margin: 0;">>,
        body = onepanel_utils:get_application_version()
    }.


%% contact/0
%% ====================================================================
%% @doc Renders support email.
%% @end
-spec contact() -> Result when
    Result :: #link{}.
%% ====================================================================
contact() ->
    #link{
        style = <<"font-size: large;">>,
        body = <<?CONTACT_EMAIL>>, url = <<"mailto:",
        ?CONTACT_EMAIL>>
    }.


%% privacy_policy/0
%% ====================================================================
%% @doc Renders privacy policy.
%% @end
-spec privacy_policy() -> Result when
    Result :: #link{}.
%% ====================================================================
privacy_policy() ->
    #link{
        style = <<"font-size: large;">>,
        body = <<"Learn about privacy policy">>,
        url = ?PAGE_PRIVACY_POLICY
    }.


%% acknowledgements/0
%% ====================================================================
%% @doc Renders acknowledgements.
%% @end
-spec acknowledgements() -> Result when
    Result :: #link{}.
%% ====================================================================
acknowledgements() ->
    #p{
        style = <<"margin: 0;">>,
        body = <<"This research was supported in part by PL-Grid Infrastructure.">>
    }.


%% license/0
%% ====================================================================
%% @doc Renders application license.
%% @end
-spec license() -> Result when
    Result :: binary().
%% ====================================================================
license() ->
    Content = case file:read_file(?LICENSE_FILE) of
                  {ok, File} -> File;
                  {error, Reason} ->
                      ?error("Cannot get license file ~s: ~p", [?LICENSE_FILE, Reason]),
                      <<"">>
              end,
    #p{
        style = <<"margin: 0; white-space: pre; font-size: initial; line-height: initial">>,
        body = Content
    }.


%% team/0
%% ====================================================================
%% @doc Renders list of applications developers.
%% @end
-spec team() -> Result when
    Result :: list().
%% ====================================================================
team() ->
    #list{
        numbered = false,
        body = lists:map(
            fun(Member) ->
                #li{
                    style = <<"font-size: large; line-height: 1.5em">>,
                    body = Member
                }
            end, ?MEMBERS)
    }.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    ok;

event(terminate) ->
    ok.
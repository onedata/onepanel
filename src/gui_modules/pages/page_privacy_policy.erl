%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This file contains n2o website code.
%% This page contains the privacy policy.
%% @end
%% ===================================================================
-module(page_privacy_policy).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1]).

-define(PRIVACY_POLICY_FILE, "PRIVACY_POLICY.html").

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
    #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}, {custom, <<"">>}]}.


%% title/0
%% ====================================================================
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() -> <<"Privacy policy">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    #panel{
        style = <<"padding: 2em 5em;">>,
        body = [
            #h3{
                style = <<"margin-bottom: 30px;">>,
                body = <<"Privacy policy">>
            },
            #panel{
                body = privacy_policy_file()
            },
            #link{
                class = <<"btn btn-inverse btn-wide">>,
                style = <<"float: right; margin: 3em 0 1.5em;">>,
                url = ?PAGE_ROOT,
                body =
                <<"Main page">>
            }
        ]
    }.


%% privacy_policy_file/1
%% ====================================================================
%% @doc Returns content of PRIVACY_POLICY.html file.
%% @end
-spec privacy_policy_file() -> Result when
    Result :: binary().
%% ====================================================================
privacy_policy_file() ->
    case file:read_file(?PRIVACY_POLICY_FILE) of
        {ok, File} ->
            File;
        {error, Reason} ->
            ?error("Cannot get privacy policy file ~s: ~p", [?PRIVACY_POLICY_FILE, Reason]),
            <<"">>
    end.


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
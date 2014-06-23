%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This file contains n2o website code.
%% The page handles users' logging out.
%% @end
%% ===================================================================

-module(page_logout).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() -> #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body()}]}.


%% title/0
%% ====================================================================
%% @doc Page title.
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() -> <<"Logout page">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    wf:user(undefined),
    wf:session(user_doc, undefined),
    %wf:logout(), % Not yet implemented in n2o stable realease
    #panel{style = <<"position: relative;">>, body = [
        #panel{class = <<"alert alert-success login-page">>, body = [
            #h3{class = <<"">>, body = <<"Logout successful">>},
            #p{class = <<"login-info">>, body = <<"Come back soon.">>},
            #button{postback = to_login, class = <<"btn btn-primary btn-block">>, body = <<"Login page">>}
        ]}
    ] ++ gui_utils:logotype_footer(120)}.


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) -> ok;

event(to_login) -> gui_utils:redirect_to_login(false).
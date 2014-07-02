%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains n2o website code.
%% The page handles users' logging in.
%% @end
%% ===================================================================

-module(page_registration).
-export([main/0, event/1]).
-include("gui_modules/common.hrl").
-include("onepanel_modules/db_logic.hrl").

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
        false ->
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
    <<"Registration">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
-spec body() -> Result when
    Result :: #panel{} | no_return().
%% ====================================================================
body() ->
    #panel{style = <<"position: relative;">>, body = [
        onepanel_gui_utils:top_menu(registration_tab),
        #panel{id = <<"error_message">>, style = <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>,
            class = <<"dialog dialog-danger">>},

        #panel{id = <<"step_1">>, style = <<"margin-top: 150px; text-align: center;">>, body = [
            #panel{style = <<"width: 50%; margin: 0 auto;">>, body = check_connection_body()}
        ]},

        #panel{id = <<"step_3">>, style = <<"margin-top: 150px; text-align: center; display: none;">>, body = [
            #panel{id = <<"registration_info">>, style = <<"width: 50%; margin: 0 auto;">>, body = []}
        ]}
    ] ++ onepanel_gui_utils:logotype_footer(120)}.


%% check_connection_body/0
%% ====================================================================
%% @doc Checks connection to Global Registry and in case of an error
%% renders appropriate message.
-spec check_connection_body() -> Result
    when Result :: [#panel{}].
%% ====================================================================
check_connection_body() ->
    #panel{class = <<"alert alert-info">>, body = [
        #h3{body = <<"Checking connection to Global Registry...">>}
    ]}.


%% registration_success/2
%% ====================================================================
%% @doc Renders registration message with provider ID.
-spec registration_success(Message :: binary(), ProviderId :: binary()) -> Result
    when Result :: [#panel{}].
%% ====================================================================
registration_success(Message, ProviderId) ->
    [
        #panel{class = <<"alert alert-success">>, body = [
            #h3{body = Message},
            #p{body = <<"Your provider ID: ", ProviderId/binary>>},
            #link{postback = to_main_page, class = <<"btn btn-primary">>, body = <<"OK">>}
        ]}
    ].


%% registration_success/2
%% ====================================================================
%% @doc Renders registration error message when cannot connect to
%% Global Registry.
-spec registration_failure() -> Result
    when Result :: [#panel{}].
%% ====================================================================
registration_failure() ->
    [
        #panel{class = <<"alert alert-danger">>, body = [
            #h3{body = <<"Cannot connect to Global Registry.">>},
            #p{body = <<"Check your network configuration or try again later.">>},
            #link{postback = to_main_page, style = <<"width: 80px;">>, class = <<"btn btn-danger">>, body = <<"OK">>}
        ]}
    ].


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    timer:sleep(2000),
    case install_utils:get_provider_id() of
        undefined ->
            case gr_adapter:check_ip_address() of
                ok ->
                    onepanel_gui_utils:change_step(1, 1);
                _ ->
                    gui_jq:update(<<"registration_info">>, registration_failure()),
                    onepanel_gui_utils:change_step(1, 2)
            end;
        ProviderId ->
            gui_jq:update(<<"registration_info">>, registration_success(<<"You are already registerd in Global Registry.">>, ProviderId)),
            onepanel_gui_utils:change_step(1, 2)
    end,
    ok;

event(to_main_page) ->
    gui_jq:redirect(<<"/">>);

event(terminate) ->
    ok.
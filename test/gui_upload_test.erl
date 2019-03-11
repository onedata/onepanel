%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for GUI package upload.
%%% @end
%%%--------------------------------------------------------------------
-module(gui_upload_test).
-author("Wojciech Geisler").

-ifdef(TEST).

-include("modules/errors.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test generators
%%%===================================================================

gui_setup_test_() ->
    {foreach,
        fun prepare/0, fun stop/1,
        [
            fun missing_gui_is_uploaded/0,
            fun existing_gui_is_not_uploaded/0
        ]
    }.

%%%===================================================================
%%% Test functions
%%%===================================================================

missing_gui_is_uploaded() ->
    service_oneprovider:set_up_service_in_onezone(),

    ?assertMatch({provider, "/clusters/" ++ _Id, patch, _, _Body, _}, pop_request()),
    ?assertMatch({provider, "/gui-upload/op_panel", post, _, _Body, _}, pop_request()).


existing_gui_is_not_uploaded() ->
    self() ! gui_uploaded,
    service_oneprovider:set_up_service_in_onezone(),
    ?assertMatch({provider, "/clusters/" ++ _Id, patch, _, _Body, _}, pop_request()),
    ?assertMatch(timeout, pop_request()).


%%%===================================================================
%%% Test fixtures
%%%===================================================================

prepare() ->
    ssl:start(),
    hackney:start(),

    meck:new([https_listener, gui, onepanel_app, clusters], [passthrough]),
    meck:expect(onepanel_app, get_build_and_version, fun() -> {"build", "app"} end),
    meck:expect(https_listener, gui_package_path, fun() -> "/some/path" end),
    meck:expect(clusters, get_id, fun() -> "clusterId" end),
    meck:expect(gui, package_hash, fun("/some/path") ->
        <<"d83ba80420ec99bcb143df16a00c39a56c140341e4446ae9b5e8b5a6d18116ed">>
    end),

    mock_oz_request(fun
        (provider, "/clusters" ++ _Id, patch, _Headers, _Body, _Opts) ->
            % different response before and after gui upload
            receive
                gui_uploaded ->
                    self() ! gui_uploaded, % persist state
                    {ok, 204, #{}, <<>>}
            after
                0 -> {ok, 400, #{}, <<>>}
            end;
        (provider, "/gui-upload/op_panel", post, _Headers, _Body, _Opts) ->
            self() ! gui_uploaded,
            {ok, 200, #{}, <<>>};
        (Auth, URN, Method, Headers, Body, Opts) ->
            error({unexpected_request, {Auth, URN, Method, Headers, Body, Opts}})
    end),
    ok.

mock_oz_request(ResponseFun) ->
    meck:new([oz_endpoint], [passthrough]),

    meck:expect(oz_endpoint, request, fun(Auth, URN, Method, Headers, Body, Opts) ->
        self() ! {request, {Auth, URN, Method, Headers, Body, Opts}},
        ResponseFun(Auth, URN, Method, Headers, Body, Opts)
    end).


stop(_) ->
    remove_msgs(),
    ?assert(meck:validate([oz_endpoint, https_listener, gui])),
    meck:unload().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc @equiv pop_request(timer:seconds(5))
%% @end
%%--------------------------------------------------------------------
-spec pop_request() -> Any :: term() | timeout.
pop_request() ->
    pop_request(0).


%%--------------------------------------------------------------------
%% @doc Returns first message from process message queue.
%% @end
%%--------------------------------------------------------------------
-spec pop_request(Timeout :: timeout()) -> Any :: term() | timeout.
pop_request(Timeout) ->
    receive
        {request, Any} -> Any
    after
        Timeout -> timeout
    end.


%%--------------------------------------------------------------------
%% @doc Removes all message from process message queue.
%% @end
%%--------------------------------------------------------------------
-spec remove_msgs() -> ok.
remove_msgs() ->
    case pop_request(0) of
        timeout -> ok;
        _ -> pop_request(0)
    end.

-endif.


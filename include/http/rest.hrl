%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Common definitions for REST handlers.
%%% @end
%%%--------------------------------------------------------------------
-author("Krzysztof Trzepla").

-ifndef(ONEPANEL_HANDLERS_REST_HRL).
-define(ONEPANEL_HANDLERS_REST_HRL, 1).

-record(client, {
    id :: onepanel_user:uuid(),
    name :: onepanel_user:name(),
    role :: onepanel_user:role()
}).

-record(rmethod, {
    type :: rest_handler:method_type(),
    args_spec = #{} :: rest_handler:spec(),
    params_spec = #{} :: rest_handler:spec(),
    noauth = false :: boolean()
}).

-record(rstate, {
    version :: rest_handler:version(),
    module :: module(),
    resource :: rest_handler:resource(),
    methods :: [rest_handler:method()],
    client :: rest_handler:client(),
    bindings = #{} :: rest_handler:bindings(),
    params = #{} :: rest_handler:params()
}).

-endif.
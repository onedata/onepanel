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
-ifndef(ONEPANEL_HANDLERS_REST_HRL).
-define(ONEPANEL_HANDLERS_REST_HRL, 1).

-record(rmethod, {
    type :: undefined | rest_handler:method_type(),
    args_spec = #{} :: rest_handler:spec(),
    params_spec = #{} :: rest_handler:spec(),
    noauth = false :: boolean()
}).

-record(rstate, {
    version :: undefined | rest_handler:version(),
    module :: undefined | module(),
    resource :: undefined | rest_handler:resource(),
    methods :: undefined | [rest_handler:method()],
    client :: undefined | rest_handler:client(),
    bindings = #{} :: rest_handler:bindings(),
    params = #{} :: rest_handler:params()
}).

-endif.
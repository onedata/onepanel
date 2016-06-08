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
    id :: onedata_user:uuid(),
    name :: onedata_user:name(),
    role :: onedata_user:role()
}).

-record(rstate, {
    module :: module(),
    resource :: rest_handler:resource(),
    methods :: [rest_handler:method()],
    noauth :: [rest_handler:method()],
    client :: rest_handler:client(),
    ctx :: #{}
}).

-endif.
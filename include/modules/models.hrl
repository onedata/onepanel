%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc @todo write me!
%%% @end
%%%--------------------------------------------------------------------
-author("Krzysztof Trzepla").

-ifndef(ONEPANEL_MODELS_HRL).
-define(ONEPANEL_MODELS_HRL, 1).

-define(MODELS, [onedata_user, service]).

-record(onedata_user, {
    username :: onedata_user:name(),
    password_hash :: onedata_user:password_hash(),
    role :: onedata_user:role(),
    uuid :: onedata_user:uuid()
}).

-record(service, {
    name :: module(),
    hosts = []:: [service:host()],
    ctx = #{} :: #{}
}).

-endif.
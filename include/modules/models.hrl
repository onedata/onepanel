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

-define(MODELS, [onepanel_user, service]).

-record(onepanel_user, {
    username :: onepanel_user:name(),
    password_hash :: onepanel_user:password_hash(),
    role :: onepanel_user:role(),
    uuid :: onepanel_user:uuid()
}).

-record(service, {
    name :: module(),
    hosts = []:: [service:host()],
    ctx = #{} :: #{}
}).

-endif.
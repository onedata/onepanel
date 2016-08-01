%%%--------------------------------------------------------------------
%%% @author Krzysztof Trzepla
%%% @copyright (C) 2016 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains models definitions.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_MODELS_HRL).
-define(ONEPANEL_MODELS_HRL, 1).

-define(MODELS, [onepanel_user, service]).

-record(onepanel_user, {
    username :: onepanel_user:name() | '_',
    password_hash :: onepanel_user:password_hash() | '_',
    role :: onepanel_user:role() | '_',
    uuid :: onepanel_user:uuid() | '_'
}).

-record(service, {
    name :: module(),
    hosts = []:: [service:host()],
    ctx = #{} :: #{}
}).

-endif.
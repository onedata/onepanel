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

-define(MODELS, [onepanel_user, onepanel_session, onepanel_milestones, service]).

-record(onepanel_user, {
    username :: onepanel_user:name() | '_',
    password_hash :: onepanel_user:password_hash() | '_',
    role :: onepanel_user:role() | '_',
    uuid :: onepanel_user:uuid() | '_'
}).

-record(onepanel_session, {
    id :: onepanel_session:id(),
    username :: onepanel_user:name(),
    expire :: non_neg_integer()
}).

-record(service, {
    name :: module(),
    hosts = [] :: [service:host()],
    ctx = #{} :: maps:map()
}).

-record(onepanel_milestones, {
    key :: atom(),
    configured :: gb_sets:set()
}).

-endif.
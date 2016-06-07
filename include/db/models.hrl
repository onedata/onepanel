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

-define(MODELS, [example_model, onedata_user, service]).
-define(INTERNAL_MODELS, [db_meta]).

-record(example_model, {
    field1 :: integer(),
    field2 :: binary(),
    field3 :: atom()
}).

-record(db_meta, {
    id = <<>> :: binary(),
    created :: integer()
}).

-record(onedata_user, {
    username :: onedata_user:name(),
    password_hash :: onedata_user:password_hash(),
    role :: onedata_user:role(),
    uuid :: onedata_user:uuid()
}).

-record(service, {
    name :: module(),
    hosts = []:: [service:host()],
    params = #{} :: #{}
}).

-endif.
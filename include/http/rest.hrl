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

-include_lib("ctool/include/http/codes.hrl").

% Name definitions
-define(REST_HANDLER_MODULE, rest_handler).


% GRI with bindings that is converted to proper GRI when bindings are resolved.
-record(b_gri, {
    type :: undefined | atom(), % gs_protocol:entity_type(),
    id :: undefined | rest_handler:binding(),
    aspect :: undefined | atom() | {atom(), term()}, % gs_protocol:aspect(),
    scope = private :: private | protected | shared | public | auto % gs_protocol:scope()
}).


-record(rest_req, {
    method = 'GET' :: rest_handler:method(),
    produces = [<<"application/json">>] :: [binary()],
    b_gri :: rest_handler:bound_gri(),
    data_spec = #{} :: rest_handler:spec()
}).


-record(rest_resp, {
    code = ?HTTP_200_OK :: integer(),
    headers = #{} :: #{binary() => binary()},
    body = {binary, <<"">>} :: json_utils:json_term() | {binary, binary()}
}).


% Convenience macros used in rest_req, they will be processed before passed
% further to internal logic.
-define(BINDING(__KEY), {binding, __KEY}).


% Convenience macros used for constructing REST replies
-define(OK_REPLY(Body), #rest_resp{code = ?HTTP_200_OK, body = Body}).
-define(CREATED_REPLY(PathTokens, Body), #rest_resp{
    code = ?HTTP_201_CREATED,
    headers = rest_translator:make_location_header(PathTokens),
    body = Body
}).
-define(ASYNC_TASK_REPLY(TaskId), #rest_resp{
    code = ?HTTP_202_ACCEPTED,
    headers = rest_translator:make_location_header([<<"tasks">>, TaskId]),
    body = #{<<"taskId">> => TaskId}
}).
-define(NO_CONTENT_REPLY, #rest_resp{code = ?HTTP_204_NO_CONTENT}).

-endif.
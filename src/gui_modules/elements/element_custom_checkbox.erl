%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains element definition for HTML custom checkbox.
%% @end
%% ===================================================================

-module(element_custom_checkbox).
-include("gui_modules/common.hrl").
-compile(export_all).

reflect() -> record_info(fields, custom_checkbox).

render_element(Record) ->
    Id = case Record#custom_checkbox.id of undefined -> wf:temp_id(); I -> I end,
    case Record#custom_checkbox.postback of
        undefined -> ignore;
        Postback ->
            wf:wire(#event{type = change, postback = Postback, target = Id, source = Record#custom_checkbox.source, delegate = Record#custom_checkbox.delegate})
    end,
    Label = [wf_tags:emit_tag(<<"input">>, [], [
        {<<"name">>, Record#custom_checkbox.name},
        {<<"id">>, Id},
        {<<"type">>, <<"checkbox">>},
        {<<"data-toggle">>, <<"checkbox">>},
        {<<"value">>, Record#custom_checkbox.value},
        {<<"checked">>, if Record#custom_checkbox.checked == true -> <<"checked">>; true -> undefined end}]
    ++ case Record#custom_checkbox.disabled of true -> [{<<"disabled">>, <<"disabled">>}]; _ -> [] end
    ),
        case Record#custom_checkbox.body of undefined -> []; B -> B end],
    wf_tags:emit_tag(<<"label">>, wf:render(Label), [
        {<<"class">>, Record#custom_checkbox.class},
        {<<"style">>, Record#custom_checkbox.style},
        {<<"for">>, Id}]).

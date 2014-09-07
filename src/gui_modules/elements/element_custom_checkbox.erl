%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module contains element definition for HTML custom checkbox.
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
        Postback -> wf:wire(#event{
            type = click,
            postback = Postback,
            target = Id,
            source = Record#custom_checkbox.source,
            delegate = Record#custom_checkbox.delegate
        })
    end,

    Span = #span{
        class = <<"icons">>,
        body = [
            #span{class = <<"first-icon fui-checkbox-unchecked">>},
            #span{class = <<"second-icon fui-checkbox-checked">>}
        ]
    },

    Input = wf_tags:emit_tag(<<"input">>, [], [
        {<<"id">>, Id},
        {<<"title">>, Record#custom_checkbox.title},
        {<<"autofocus">>, Record#custom_checkbox.autofocus},
        {<<"checked">>, case Record#custom_checkbox.checked of true -> <<"checked">>; _ -> undefined end},
        {<<"data-toggle">>, <<"checkbox">>},
        {<<"disabled">>, case Record#custom_checkbox.disabled of true -> <<"disabled">>;  _ -> undefined end},
        {<<"name">>, Record#custom_checkbox.name},
        {<<"type">>, <<"checkbox">>},
        {<<"value">>, Record#custom_checkbox.value} | Record#custom_checkbox.data_fields
    ]),

    Checked = case Record#custom_checkbox.checked of
                  true -> <<" checked">>;
                  _ -> <<"">>
              end,

    Disabled = case Record#custom_checkbox.disabled of
                   true -> <<" disabled">>;
                   _ -> <<"">>
               end,

    wf_tags:emit_tag(<<"label">>, wf:render([Span, Input]), [
        {<<"id">>, Id},
        {<<"class">>, <<(Record#custom_checkbox.class)/binary, Checked/binary, Disabled/binary>>},
        {<<"style">>, Record#custom_checkbox.style},
        {<<"for">>, Id}
    ]).

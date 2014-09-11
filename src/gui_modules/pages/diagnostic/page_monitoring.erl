%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows for viewing online software monitoring.
%% @end
%% ===================================================================
-module(page_monitoring).

-include("registered_names.hrl").
-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").

-export([main/0, event/1, comet_loop/1]).

-define(TIME_RANGES, [<<"last 5 minutes">>, <<"last 15 minutes">>, <<"last hour">>, <<"last 24 hours">>, <<"last 7 days">>, <<"last 30 days">>, <<"last 365 days">>]).
-define(SUMMARY_CHART_TYPES, [<<"CPU utilization">>, <<"memory usage">>, <<"network throughput">>, <<"network transfer">>, <<"Erlang ports transfer">>, <<"storage IO transfer">>]).
-define(HOST_CHART_TYPES, [<<"CPU utilization">>, <<"memory usage">>, <<"network throughput">>, <<"network transfer">>, <<"Erlang ports transfer">>]).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {counter, nodes, node, time_range, chart_type, charts = []}).

%% Drawn chart details
-define(CHART, chart).
-record(?CHART, {id, node, time_range, type, update_period}).

%% ====================================================================
%% API functions
%% ====================================================================

%% main/0
%% ====================================================================
%% @doc Template points to the template file, which will be filled with content.
%% @end
-spec main() -> Result when
    Result :: #dtl{}.
%% ====================================================================
main() ->
    case gui_ctx:user_logged_in() of
        true ->
            case installer_utils:get_workers() of
                [] ->
                    page_error:redirect_with_error(?SOFTWARE_NOT_INSTALLED_ERROR),
                    #dtl{file = "bare", app = ?SOFTWARE_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]};
                _ ->
                    #dtl{file = "bare", app = ?SOFTWARE_NAME, bindings = [{title, title()}, {body, body()}, {custom, custom()}]}
            end;
        false ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?SOFTWARE_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc This will be placed instead of {{title}} tag in template.
%% @end
-spec title() -> binary().
%% ====================================================================
title() -> <<"Monitoring">>.


%% body/0
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body() -> Result when
    Result :: #panel{}.
%% ====================================================================
body() ->
    Header = onepanel_gui_utils:top_menu(diagnostics_tab, monitoring_link, monitoring_submenu()),
    Main = #panel{
        style = <<"margin-top: 114px;">>,
        body = #table{
            id = <<"charts_table">>,
            class = <<"table table-stripped">>,
            style = <<"width: 100%;">>
        }
    },
    onepanel_gui_utils:body(Header, Main).


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
%% @end
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/js/jsapi.js' type='text/javascript' charset='utf-8'></script>",
    "<script type='text/javascript'>",
    "google.load('visualization', '1.1', {'packages': ['corechart']});",
    "</script>",
    "<script src='/js/charts.js' type='text/javascript' charset='utf-8'></script>">>.


%% monitoring_submenu/0
%% ====================================================================
%% @doc Submenu that will end up concatenated to top menu.
%% @end
-spec monitoring_submenu() -> [#panel{}].
%% ====================================================================
monitoring_submenu() ->
    MarginStyle = <<"margin: 10px 15px;">>,
    [
        #panel{
            class = <<"navbar-inner">>,
            style = <<"border-bottom: 1px solid gray;">>,
            body = #panel{
                class = <<"container">>,
                style = <<"text-align: center">>,
                body = [lists:map(fun({ButtonStyle, SpanId, SpanBody, ListId, ListBody}) ->
                    #panel{
                        class = <<"btn-group">>,
                        style = MarginStyle,
                        body = [
                            <<"<i class=\"dropdown-arrow dropdown-arrow-inverse\"></i>">>,
                            #button{
                                class = <<"btn btn-inverse btn-small dropdown-toggle">>,
                                style = ButtonStyle,
                                data_fields = [{<<"data-toggle">>, <<"dropdown">>}],
                                body = [
                                    #span{
                                        id = SpanId,
                                        class = <<"filter-option pull-left">>,
                                        body = SpanBody
                                    },
                                    #span{
                                        class = <<"caret pull-right">>
                                    }
                                ]
                            },
                            #list{
                                id = ListId,
                                class = <<"dropdown-menu dropdown-inverse">>,
                                style = <<"overflow-y: auto; max-height: 15em;">>,
                                body = ListBody
                            }
                        ]
                    }
                end, [
                    {<<"width: 15em;">>, <<"host_label">>, <<"<b>Host</b>">>, <<"node_dropdown">>, node_dropdown(undefined, [])},
                    {<<"width: 20em;">>, <<"time_range_label">>, <<"<b>Time range</b>">>, <<"time_range_dropdown">>, time_range_dropdown(undefined)},
                    {<<"width: 20em;">>, <<"chart_type_label">>, <<"<b>Chart type</b>">>, <<"chart_type_dropdown">>, chart_type_dropdown(undefined, ?HOST_CHART_TYPES)}
                ])] ++ [
                    #panel{
                        class = <<"btn-group">>,
                        style = MarginStyle,
                        body = #button{
                            id = <<"add_chart_button">>,
                            class = <<"btn btn-primary btn-small">>,
                            style = <<"font-weight: bold;">>,
                            postback = {message, add_chart},
                            disabled = true,
                            body = <<"Add chart">>
                        }
                    }
                ]
            }
        }
    ].


%% node_dropdown/2
%% ====================================================================
%% @doc Renders the body of host dropdown and highlights the current choice.
%% @end
-spec node_dropdown(ActiveNode :: summary | node(), Nodes :: [node()]) -> Result when
    Result :: [#li{}].
%% ====================================================================
node_dropdown(ActiveNode, Nodes) ->
    lists:map(fun({Node, Index}) ->
        Class = case Node of
                    ActiveNode -> <<"active">>;
                    _ -> <<"">>
                end,
        ID = <<"host_li_", (integer_to_binary(Index))/binary>>,
        #li{
            id = ID,
            actions = gui_jq:postback_action(ID, {message, {set_node, Node}}),
            class = Class,
            body = #link{
                style = <<"text-align: left;">>,
                body = get_hostname(Node)
            }
        }
    end, lists:zip(Nodes, tl(lists:seq(0, length(Nodes))))).


%% time_range_dropdown/2
%% ====================================================================
%% @doc Renders the body of time range dropdown and highlights the current choice.
%% @end
-spec time_range_dropdown(ActiveTimeRange :: binary()) -> [#li{}].
%% ====================================================================
time_range_dropdown(ActiveTimeRange) ->
    lists:map(fun({TimeRange, Index}) ->
        Class = case TimeRange of
                    ActiveTimeRange -> <<"active">>;
                    _ -> <<"">>
                end,
        ID = <<"time_range_li_", (integer_to_binary(Index))/binary>>,
        #li{
            id = ID,
            actions = gui_jq:postback_action(ID, {message, {set_time_range, TimeRange}}),
            class = Class,
            body = #link{
                style = <<"text-align: left;">>,
                body = TimeRange
            }
        }
    end, lists:zip(?TIME_RANGES, tl(lists:seq(0, length(?TIME_RANGES))))).


%% chart_type_dropdown/2
%% ====================================================================
%% @doc Renders the body of chart type dropdown and highlights the current choice.
%% @end
-spec chart_type_dropdown(ActiveChartType :: binary(), ChartTypes :: [binary()]) -> [#li{}].
%% ====================================================================
chart_type_dropdown(ActiveChartType, ChartTypes) ->
    lists:map(fun({ChartType, Index}) ->
        Class = case ChartType of
                    ActiveChartType -> <<"active">>;
                    _ -> <<"">>
                end,
        ID = <<"chart_type_li_", (integer_to_binary(Index))/binary>>,
        #li{
            id = ID,
            actions = gui_jq:postback_action(ID, {message, {set_chart_type, ChartType, ChartTypes}}),
            class = Class,
            body = #link{
                style = <<"text-align: left;">>,
                body = ChartType
            }
        }
    end, lists:zip(ChartTypes, tl(lists:seq(0, length(ChartTypes))))).


%% reset_dropdowns/1
%% ====================================================================
%% @doc Resets dropdowns to show default labels without selection.
%% @end
-spec reset_dropdowns(Nodes :: [node()]) -> ok.
%% ====================================================================
reset_dropdowns(Nodes) ->
    gui_jq:update(<<"host_label">>, <<"<b>Host</b>">>),
    gui_jq:update(<<"node_dropdown">>, node_dropdown(undefined, Nodes)),
    gui_jq:update(<<"time_range_label">>, <<"<b>Time range</b>">>),
    gui_jq:update(<<"time_range_dropdown">>, time_range_dropdown(undefined)),
    gui_jq:update(<<"chart_type_label">>, <<"<b>Chart type</b>">>),
    gui_jq:update(<<"chart_type_dropdown">>, chart_type_dropdown(undefined, ?HOST_CHART_TYPES)).


%% create_chart/4
%% ====================================================================
%% @doc Creates new chart and displays it on page.
%% @end
-spec create_chart(Counter :: integer(), Node :: summary | node(), TimeRange :: binary(), ChartType :: binary()) -> {ok, #?CHART{}} | error.
%% ====================================================================
create_chart(Counter, Node, TimeRange, ChartType) ->
    try
        UpdatePeriod = get_update_period(Node, TimeRange),
        {IdJSON, TypeJSON, TitleJSON, VAxisTitleJSON, HeaderJSON, BodyJSON} = get_json_data(Counter, Node, TimeRange, ChartType, UpdatePeriod),
        RowID = <<"row_", (integer_to_binary(Counter))/binary>>,
        gui_jq:insert_top(<<"charts_table">>, #tr{id = RowID,
            cells = [
                #th{body = #panel{id = <<"chart_", (integer_to_binary(Counter))/binary>>}},
                #th{body = #link{postback = {message, {delete_chart, Counter}}, title = <<"Remove">>, class = <<"glyph-link">>,
                    body = #span{class = <<"fui-cross">>, style = <<"font-size: 20px;">>}},
                    style = <<"width: 10px;">>}
            ]}),
        gui_jq:wire("createChart(" ++ IdJSON ++ "," ++ TypeJSON ++ "," ++ TitleJSON ++ "," ++ VAxisTitleJSON ++ "," ++ HeaderJSON ++ "," ++ BodyJSON ++ ");"),
        {ok, #?CHART{id = Counter, node = Node, time_range = TimeRange, type = ChartType, update_period = UpdatePeriod}}
    catch
        _:Reason ->
            ?error("Cannot create chart ~p: ~p", [{Node, TimeRange, ChartType}, Reason]),
            error
    end.


%% update_chart/1
%% ====================================================================
%% @doc Updates specified chart on page.
%% @end
-spec update_chart(Chart :: #?CHART{}) -> ok | error.
%% ====================================================================
update_chart(#?CHART{id = ID, node = Node, time_range = TimeRange, type = ChartType, update_period = UpdatePeriod} = Chart) ->
    try
        {IdJSON, _, _, _, _, BodyJSON} = get_json_data(ID, Node, TimeRange, ChartType, UpdatePeriod),
        gui_jq:wire("updateChart(" ++ IdJSON ++ "," ++ BodyJSON ++ ");"),
        ok
    catch
        _:Reason ->
            ?error("Cannot update chart ~p: ~p", [{Chart#?CHART.id, Chart#?CHART.time_range, Chart#?CHART.type}, Reason]),
            error
    end.


%% get_json_data/4
%% ====================================================================
%% @doc Returns data in json format applicable for Google Charts API.
%% @end
-spec get_json_data(Id :: integer(), Node :: summary | node(), TimeRange :: binary(), ChartType :: binary(), UpdatePeriod :: integer()) -> Result when
    Result :: {IdJSON, TypeJSON, TitleJSON, VAxisTitleJSON, HeaderJSON, BodyJSON},
    IdJSON :: string(),
    TypeJSON :: string(),
    TitleJSON :: string(),
    VAxisTitleJSON :: string(),
    HeaderJSON :: string(),
    BodyJSON :: string().
%% ====================================================================
get_json_data(Id, summary, TimeRange, ChartType, UpdatePeriod) ->
    get_json_data(Id, summary, {global, ?CCM}, get_cluster_stats, TimeRange, ChartType, UpdatePeriod);

get_json_data(Id, Node, TimeRange, ChartType, UpdatePeriod) ->
    get_json_data(Id, Node, {?NODE_MANAGER_NAME, Node}, get_node_stats, TimeRange, ChartType, UpdatePeriod).


%% get_json_data/7
%% ====================================================================
%% @doc Returns data in json format applicable for Google Charts API.
%% Should not be used directly, use get_json_data/4 instead.
%% @end
-spec get_json_data(Id :: integer(), Node :: summary | node(), Targer :: term(), Command :: atom(), TimeRange :: binary(), ChartType :: binary(), UpdatePeriod :: integer()) -> Result when
    Result :: {IdJSON, TypeJSON, TitleJSON, VAxisTitleJSON, HeaderJSON, BodyJSON},
    IdJSON :: string(),
    TypeJSON :: string(),
    TitleJSON :: string(),
    VAxisTitleJSON :: string(),
    HeaderJSON :: string(),
    BodyJSON :: string().
%% ====================================================================
get_json_data(Id, Node, Target, Command, TimeRange, ChartType, UpdatePeriod) ->
    {MegaSecs, Secs, _} = erlang:now(),
    EndTime = trunc(((1000000 * MegaSecs + Secs - 2 * UpdatePeriod) / UpdatePeriod) * UpdatePeriod),
    StartTime = EndTime - time_range_to_integer(TimeRange),
    {ok, {Header, Body}} = gen_server:call(Target, {Command, StartTime, EndTime, chart_type_to_columns(ChartType)}),
    HeaderJSON = "{ cols: [{label: 'Time', type: 'string'}, " ++
        string:join(lists:map(fun(Column) ->
            header_to_json(Column)
        end, Header), ", ") ++ "], rows: []}",
    BodyJSON = "[" ++ string:join(lists:map(fun({Timestamp, Row}) ->
        "['" ++ integer_to_list(1000 * Timestamp) ++ "', " ++
            string:join(lists:map(fun(Value) ->
                value_to_list(Value)
            end, Row), ", ") ++ "]"
    end, Body), ", ") ++ "]",
    IdJSON = "'" ++ integer_to_list(Id) ++ "'",
    TypeJSON = chart_type_to_google_chart_type(ChartType),
    TitleJSON = "'" ++ binary_to_list(get_hostname(Node)) ++ " / " ++ binary_to_list(TimeRange) ++ " / " ++ binary_to_list(ChartType) ++ "'",
    VAxisTitleJSON = chart_type_to_v_axis_title(ChartType),
    {IdJSON, TypeJSON, TitleJSON, VAxisTitleJSON, HeaderJSON, BodyJSON}.


%% validate_page_state/1
%% ====================================================================
%% @doc Checks whether user has selected all requiered options to draw chart.
%% @end
-spec validate_page_state(PageState :: #?STATE{}) -> ok | error.
%% ====================================================================
validate_page_state(#?STATE{node = undefined}) ->
    error_message(<<"Please select host.">>),
    error;

validate_page_state(#?STATE{time_range = undefined}) ->
    error_message(<<"Please select time range.">>),
    error;

validate_page_state(#?STATE{chart_type = undefined}) ->
    error_message(<<"Please select chart type.">>),
    error;

validate_page_state(_) ->
    ok.


%% validate_chart/4
%% ====================================================================
%% @doc Checks whether chart can be drawn on page. Chart can be drawn
%% when it is not already available on page.
%% @end
-spec validate_chart(Node :: summary | node(), TimeRange :: binary(), ChartType :: binary(), Charts :: [{Key :: integer(), #?CHART{}}]) -> ok | error.
%% ====================================================================
validate_chart(_, _, _, []) ->
    ok;

validate_chart(Node, TimeRange, ChartType, [{_, #?CHART{node = Node, time_range = TimeRange, type = ChartType}} | _]) ->
    error;

validate_chart(Node, TimeRange, ChartType, [_ | Charts]) ->
    validate_chart(Node, TimeRange, ChartType, Charts).


%% get_update_period/2
%% ====================================================================
%% @doc Returns period in seconds that say how often graph should be updated.
%% @end
-spec get_update_period(Node :: summary | node(), TimeRange :: binary()) -> integer().
%% ====================================================================
get_update_period(summary, TimeRange) ->
    {ok, Period} = application:get_env(?APP_NAME, summary_chart_update_delay),
    Period * get_period_multiplication(TimeRange);

get_update_period(_, TimeRange) ->
    {ok, Period} = application:get_env(?APP_NAME, host_chart_update_delay),
    Period * get_period_multiplication(TimeRange).


%% get_hostname/1
%% ====================================================================
%% @doc Returns node's hostname.
%% In case of 'summary' atom, "summary" binary is returned.
%% @end
-spec get_hostname(Node :: node() | summary) -> Hostname :: binary().
%% ====================================================================
get_hostname(summary) ->
    <<"summary">>;

get_hostname(Node) ->
    [_, Host] = binary:split(atom_to_binary(Node, latin1), <<"@">>),
    Host.


%% get_period_multiplication/1
%% ====================================================================
%% @doc Returns update period multiplication in terms of Round Robin Archive size.
%% @end
-spec get_period_multiplication(TimeRange :: binary()) -> integer().
%% ====================================================================
get_period_multiplication(<<"last 5 minutes">>) -> 1;
get_period_multiplication(<<"last 15 minutes">>) -> 1;
get_period_multiplication(<<"last hour">>) -> 1;
get_period_multiplication(<<"last 24 hours">>) -> 24;
get_period_multiplication(<<"last 7 days">>) -> 7 * 24;
get_period_multiplication(<<"last 30 days">>) -> 30 * 24;
get_period_multiplication(<<"last 365 days">>) -> 365 * 24.


%% time_range_to_integer/1
%% ====================================================================
%% @doc Maps time ranges to integers.
%% @end
-spec time_range_to_integer(TimeRange :: binary()) -> integer().
%% ====================================================================
time_range_to_integer(<<"last 5 minutes">>) -> 5 * 60;
time_range_to_integer(<<"last 15 minutes">>) -> 15 * 60;
time_range_to_integer(<<"last hour">>) -> 60 * 60;
time_range_to_integer(<<"last 24 hours">>) -> 24 * 60 * 60;
time_range_to_integer(<<"last 7 days">>) -> 7 * 24 * 60 * 60;
time_range_to_integer(<<"last 30 days">>) -> 30 * 24 * 60 * 60;
time_range_to_integer(<<"last 365 days">>) -> 365 * 24 * 60 * 60.


%% chart_type_to_columns/1
%% ====================================================================
%% @doc Maps chart types to columns that will be fetched from Round Robin Database.
%% @end
-spec chart_type_to_columns(ChartType :: binary()) -> [binary() | {starts_with, binary()}].
%% ====================================================================
chart_type_to_columns(<<"CPU utilization">>) -> {name, [<<"cpu">>]};
chart_type_to_columns(<<"memory usage">>) -> {name, [<<"mem">>]};
chart_type_to_columns(<<"network throughput">>) -> {starts_with, [<<"net_rx_pps">>, <<"net_tx_pps">>]};
chart_type_to_columns(<<"network transfer">>) -> {starts_with, [<<"net_rx_b">>, <<"net_tx_b">>]};
chart_type_to_columns(<<"Erlang ports transfer">>) -> {name, [<<"ports_rx_b">>, <<"ports_tx_b">>]};
chart_type_to_columns(<<"storage IO transfer">>) -> {name, [<<"storage_read_b">>, <<"storage_write_b">>]}.


%% chart_type_to_google_chart_type/1
%% ====================================================================
%% @doc Maps chart types to Google API chart types.
%% @end
-spec chart_type_to_google_chart_type(ChartType :: binary()) -> string().
%% ====================================================================
chart_type_to_google_chart_type(<<"CPU utilization">>) -> "'LineChart'";
chart_type_to_google_chart_type(<<"memory usage">>) -> "'LineChart'";
chart_type_to_google_chart_type(_) -> "'AreaChart'".


%% chart_type_to_v_axis_title/1
%% ====================================================================
%% @doc Maps chart type to chart's vertical axis label.
%% @end
-spec chart_type_to_v_axis_title(ChartType :: binary()) -> string().
%% ====================================================================
chart_type_to_v_axis_title(<<"CPU utilization">>) -> "'Utilization [%]'";
chart_type_to_v_axis_title(<<"memory usage">>) -> "'Usage [%]'";
chart_type_to_v_axis_title(<<"network throughput">>) -> "'packets / sec'";
chart_type_to_v_axis_title(<<"network transfer">>) -> "'bytes'";
chart_type_to_v_axis_title(<<"Erlang ports transfer">>) -> "'bytes'";
chart_type_to_v_axis_title(<<"storage IO transfer">>) -> "'bytes'".


%% header_to_json/1
%% ====================================================================
%% @doc Maps column header name to chart's legend label.
%% @end
-spec header_to_json(Header :: binary()) -> string().
%% ====================================================================
header_to_json(<<"cpu">>) -> "{label: 'CPU', type: 'number'}";
header_to_json(<<"core", Core/binary>>) -> "{label: 'Core " ++ binary_to_list(Core) ++ "', type: 'number'}";
header_to_json(<<"mem">>) -> "{label: 'Memory', type: 'number'}";
header_to_json(<<"ports_rx_b">>) -> "{label: 'RX bytes', type: 'number'}";
header_to_json(<<"ports_tx_b">>) -> "{label: 'TX bytes', type: 'number'}";
header_to_json(<<"storage_read_b">>) -> "{label: 'read bytes', type: 'number'}";
header_to_json(<<"storage_write_b">>) -> "{label: 'written bytes', type: 'number'}";
header_to_json(<<"net_rx_b">>) -> "{label: 'RX bytes', type: 'number'}";
header_to_json(<<"net_tx_b">>) -> "{label: 'TX bytes', type: 'number'}";
header_to_json(<<"net_rx_b_", Interface/binary>>) ->
    "{label: '" ++ binary_to_list(Interface) ++ " RX bytes', type: 'number'}";
header_to_json(<<"net_tx_b_", Interface/binary>>) ->
    "{label: '" ++ binary_to_list(Interface) ++ " TX bytes', type: 'number'}";
header_to_json(<<"net_rx_pps">>) -> "{label: 'RX pps', type: 'number'}";
header_to_json(<<"net_tx_pps">>) -> "{label: 'TX pps', type: 'number'}";
header_to_json(<<"net_rx_pps_", Interface/binary>>) ->
    "{label: '" ++ binary_to_list(Interface) ++ " RX pps', type: 'number'}";
header_to_json(<<"net_tx_pps_", Interface/binary>>) ->
    "{label: '" ++ binary_to_list(Interface) ++ " TX pps', type: 'number'}";
header_to_json(_) -> throw(<<"Unknown column.">>).


%% value_to_list/1
%% ====================================================================
%% @doc For integer and float returns string representation. For rest returns "null".
%% @end
-spec value_to_list(Value :: term()) -> string().
%% ====================================================================
value_to_list(Value) when is_integer(Value) ->
    integer_to_list(Value);

value_to_list(Value) when is_float(Value) ->
    float_to_list(Value);

value_to_list(_) ->
    "null".


%% error_message/1
%% ====================================================================
%% @doc Displays error message before main table.
-spec error_message(Message :: binary()) -> ok.
%% ====================================================================
error_message(Message) ->
    gui_jq:insert_before(<<"charts_table">>, #panel{
        id = <<"error_message">>,
        style = <<"width: 100%; margin: 0 auto; position: relative;">>,
        body = [
            #panel{
                style = <<"margin: 0 auto;">>,
                class = <<"dialog dialog-danger">>,
                body = Message
            },
            #link{
                title = <<"Close">>,
                style = <<"position: absolute; top: 1em; right: 1em;">>,
                class = <<"glyph-link">>,
                postback = {close_message, <<"error_message">>},
                body = #span{
                    class = <<"fui-cross">>
                }
            }
        ]
    }).


%% ====================================================================
%% Events handling
%% ====================================================================

%% comet_loop/1
%% ====================================================================
%% @doc Adds, updates and removes monitoring charts. 
%% Handles messages that change chart preferences.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{counter = Counter, nodes = Nodes, node = Node, time_range = TimeRange, chart_type = ChartType, charts = Charts} = PageState) ->
    NewPageState = try
        receive
            {set_node, NewNode, ChartTypes} ->
                case lists:member(ChartType, ChartTypes) of
                    true -> gui_jq:update(<<"chart_type_dropdown">>, chart_type_dropdown(ChartType, ChartTypes));
                    _ -> self() ! {set_chart_type, undefined, ChartTypes}
                end,
                gui_jq:update(<<"host_label">>, <<"Host: <b>", (get_hostname(NewNode))/binary, "</b>">>),
                gui_jq:update(<<"node_dropdown">>, node_dropdown(NewNode, Nodes)),
                PageState#?STATE{node = NewNode};

            {set_time_range, NewTimeRange} ->
                gui_jq:update(<<"time_range_label">>, <<"Time range: <b>", NewTimeRange/binary, "</b>">>),
                gui_jq:update(<<"time_range_dropdown">>, time_range_dropdown(NewTimeRange)),
                PageState#?STATE{time_range = NewTimeRange};

            {set_chart_type, NewChartType, ChartTypes} ->
                case NewChartType of
                    undefined -> gui_jq:update(<<"chart_type_label">>, <<"<b>Chart type</b>">>);
                    _ -> gui_jq:update(<<"chart_type_label">>, <<"Chart type: <b>", NewChartType/binary, "</b>">>)
                end,
                gui_jq:update(<<"chart_type_dropdown">>, chart_type_dropdown(NewChartType, ChartTypes)),
                PageState#?STATE{chart_type = NewChartType};

            add_chart ->
                gui_jq:remove(<<"error_message">>),
                gui_comet:flush(),
                case validate_page_state(PageState) of
                    ok ->
                        case validate_chart(Node, TimeRange, ChartType, Charts) of
                            ok ->
                                case create_chart(Counter, Node, TimeRange, ChartType) of
                                    {ok, Chart} ->
                                        reset_dropdowns(Nodes),
                                        erlang:send_after(1000 * Chart#chart.update_period, self(), {update_chart, Counter}),
                                        PageState#?STATE{counter = Counter + 1, node = undefined, time_range = undefined, chart_type = undefined, charts = [{Counter, Chart} | Charts]};
                                    _ ->
                                        error_message(<<"There has been an error during chart creation. Please try again.">>),
                                        PageState
                                end;
                            _ -> error_message(<<"Chart already added.">>),
                                PageState
                        end;
                    _ -> PageState
                end;

            {update_chart, Id} ->
                try
                    case proplists:get_value(Id, Charts) of
                        undefined ->
                            ok;
                        Chart ->
                            ok = update_chart(Chart),
                            erlang:send_after(1000 * Chart#chart.update_period, self(), {update_chart, Id})
                    end
                catch
                    _:_ -> ok
                end,
                PageState;

            {delete_chart, Id} ->
                gui_jq:wire(<<"deleteChart(\"", (integer_to_binary(Id))/binary, "\");">>),
                PageState#?STATE{charts = proplists:delete(Id, Charts)}

        end
                   catch Type:Msg ->
                       ?error("Comet process exception: ~p:~p", [Type, Msg]),
                       error_message(<<"There has been an error in comet process. Please refresh the page.">>),
                       {error, Msg}
                   end,
    gui_jq:hide(<<"main_spinner">>),
    gui_comet:flush(),
    ?MODULE:comet_loop(NewPageState).


%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    try
        [_ | _] = Nodes = gen_server:call({global, ?CCM}, get_nodes),
        gui_jq:prop(<<"add_chart_button">>, <<"disabled">>, <<"">>),
        gui_jq:update(<<"node_dropdown">>, node_dropdown(undefined, [summary | Nodes])),
        {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{counter = 1, nodes = [summary | Nodes]}) end),
        put(?COMET_PID, Pid)
    catch
        _:Reason ->
            ?error("Cannot initialize page ~p: ~p", [?MODULE, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch application configuration.<br>Please try again later.">>)
    end;

event({message, {set_node, summary}}) ->
    get(comet_pid) ! {set_node, summary, ?SUMMARY_CHART_TYPES};

event({message, {set_node, Node}}) ->
    get(comet_pid) ! {set_node, Node, ?HOST_CHART_TYPES};

event({message, Message}) ->
    get(?COMET_PID) ! Message,
    gui_jq:show(<<"main_spinner">>);

event({close_message, MessageId}) ->
    gui_jq:remove(MessageId);

event(terminate) ->
    ok.
%% ===================================================================
%% @author Krzysztof Trzepla
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc This module contains n2o website code.
%% This page allows to manage provider spaces.
%% @end
%% ===================================================================

-module(page_spaces_settings).
-export([main/0, event/1, api_event/3, comet_loop/1]).

-include("gui_modules/common.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("ctool/include/global_registry/gr_users.hrl").
-include_lib("ctool/include/global_registry/gr_spaces.hrl").
-include_lib("ctool/include/global_registry/gr_providers.hrl").

%% Common page CCS styles
-define(MESSAGE_STYLE, <<"position: fixed; width: 100%; top: 55px; z-index: 1; display: none;">>).
-define(CONTENT_COLUMN_STYLE, <<"padding-right: 0">>).
-define(NAVIGATION_COLUMN_STYLE, <<"border-left-width: 0; width: 20px; padding-left: 0;">>).
-define(DESCRIPTION_STYLE, <<"border-width: 0; text-align: right; width: 10%; padding-left: 0; padding-right: 0;">>).
-define(MAIN_STYLE, <<"border-width: 0;  text-align: left; padding-left: 1em; width: 90%;">>).
-define(LABEL_STYLE, <<"margin: 0 auto;">>).
-define(PARAGRAPH_STYLE, <<"margin: 0 auto;">>).
-define(TABLE_STYLE, <<"border-width: 0; width: 100%; border-collapse: inherit;">>).

%% ID of table row of last added space
-define(ID, id).

%% Comet process pid
-define(COMET_PID, comet_pid).

%% Comet process state
-define(STATE, comet_state).
-record(?STATE, {}).

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
            ProviderId = provider_logic:get_provider_id(),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, title()}, {body, body(ProviderId)}, {custom, custom()}]};
        false ->
            gui_jq:redirect_to_login(true),
            #dtl{file = "bare", app = ?APP_NAME, bindings = [{title, <<"">>}, {body, <<"">>}, {custom, <<"">>}]}
    end.


%% title/0
%% ====================================================================
%% @doc Page title.
%% @end
-spec title() -> Result when
    Result :: binary().
%% ====================================================================
title() ->
    <<"Spaces setting">>.


%% custom/0
%% ====================================================================
%% @doc This will be placed instead of {{custom}} tag in template.
%% @end
-spec custom() -> binary().
%% ====================================================================
custom() ->
    <<"<script src='/js/bootbox.min.js' type='text/javascript' charset='utf-8'></script>">>.


%% body/1
%% ====================================================================
%% @doc This will be placed instead of {{body}} tag in template.
%% @end
-spec body(ProviderId :: binary() | undefined) -> Result when
    Result :: #panel{}.
%% ====================================================================
body(undefined) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = #panel{
            style = <<"width: 50%; margin: 0 auto;">>,
            class = <<"alert alert-info">>,
            body = [
                #h3{
                    body = <<"Unregistered">>
                },
                #p{
                    body = <<"Please complete registration process in Global Registry.">>
                },
                #button{
                    postback = to_account_page,
                    class = <<"btn btn-info confirm">>,
                    style = <<"width: 80px; font-weight: bold;">>,
                    body = <<"OK">>
                }
            ]
        }
    },
    onepanel_gui_utils:body(Header, Main);

body(_) ->
    Header = onepanel_gui_utils:top_menu(spaces_tab, spaces_settings_link),
    Main = #panel{
        style = <<"margin-top: 10em; text-align: center;">>,
        body = [
            #h6{
                style = <<"font-size: x-large; margin-bottom: 3em;">>,
                body = <<"Spaces settings">>
            },
            #panel{
                style = <<"margin-bottom: 3em;">>,
                body = [
                    #button{
                        id = <<"create_space_button">>,
                        postback = create_space,
                        class = <<"btn btn-primary btn-small">>,
                        style = <<"font-weight: bold; margin-right: 1em;">>,
                        body = <<"Create Space">>
                    },
                    #image{
                        id = <<"settings_spinner">>,
                        image = <<"/images/spinner.gif">>,
                        style = <<"width: 1.5em; visibility: hidden;">>
                    },
                    #button{
                        id = <<"support_space_button">>,
                        postback = support_space,
                        class = <<"btn btn-primary btn-small">>,
                        style = <<"font-weight: bold; margin-left: 1em">>,
                        body = <<"Support Space">>
                    }
                ]
            },
            #table{
                class = <<"table table-bordered table-striped">>,
                style = <<"width: 50%; margin: 0 auto; table-layout: fixed;">>,
                body = #tbody{
                    id = <<"spaces">>,
                    body = spaces_table_collapsed(<<"spaces">>)
                }
            }
        ]
    },
    onepanel_gui_utils:body(Header, Main).


%% spaces_table_collapsed/1
%% ====================================================================
%% @doc Renders collapsed Spaces settings table.
%% @end
-spec spaces_table_collapsed(TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_collapsed(TableId) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                id = <<"space_all_spinner">>,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button(<<"Expand All">>, {spaces_table_expand, TableId, <<"space_all_spinner">>})
            }
        ]
    },
    try
        {ok, SpaceIds} = cacheable_call(gr_providers, get_spaces, [provider]),
        Rows = lists:map(fun({SpaceId, Counter}) ->
            RowId = <<"space_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = space_row_collapsed(SpaceId, RowId)
            }
        end, lists:zip(SpaceIds, tl(lists:seq(0, length(SpaceIds))))),
        gui_ctx:put(?ID, length(Rows)),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch supported Spaces: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch supported Spaces.<br>Please try again later.">>),
            clear_call(gr_providers, get_spaces, [provider]),
            [Header]
    end.


%% spaces_table_expanded/1
%% ====================================================================
%% @doc Renders expanded Spaces settings table.
%% @end
-spec spaces_table_expanded(TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
spaces_table_expanded(TableId) ->
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Spaces">>
            },
            #th{
                id = <<"space_all_spinner">>,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button(<<"Collapse All">>, {spaces_table_collapse, TableId, <<"space_all_spinner">>})
            }
        ]
    },
    try
        {ok, SpaceIds} = cacheable_call(gr_providers, get_spaces, [provider]),
        Rows = lists:map(fun({SpaceId, Counter}) ->
            RowId = <<"space_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = space_row_expanded(SpaceId, RowId)
            }
        end, lists:zip(SpaceIds, tl(lists:seq(0, length(SpaceIds))))),
        gui_ctx:put(?ID, length(Rows)),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch supported Spaces: ~p", [Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch supported Spaces.<br>Please try again later.">>),
            clear_call(gr_providers, get_spaces, [provider]),
            [Header]
    end.


%% space_row_collapsed/2
%% ====================================================================
%% @doc Renders collapsed Space settings row.
%% @end
-spec space_row_collapsed(SpaceId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_collapsed(SpaceId, RowId) ->
    try
        {ok, #space_details{name = Name}} = cacheable_call(gr_providers, get_space_details, [provider, SpaceId]),
        SpinnerId = <<RowId/binary, "_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = #table{
                    style = ?TABLE_STYLE,
                    body = [
                        #tr{
                            cells = [
                                #td{
                                    style = ?DESCRIPTION_STYLE,
                                    body = #label{
                                        style = ?LABEL_STYLE,
                                        class = <<"label label-large label-inverse">>,
                                        body = <<"Name">>
                                    }
                                },
                                #td{
                                    style = ?MAIN_STYLE,
                                    body = #p{
                                        style = ?PARAGRAPH_STYLE,
                                        body = Name
                                    }
                                }
                            ]
                        }
                    ]
                }
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button({space_row_expand, SpaceId, RowId, SpinnerId})
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch details of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch details of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_providers, get_space_details, [provider, SpaceId]),
            space_row_collapsed(SpaceId, RowId)
    end.


%% space_row_expanded/2
%% ====================================================================
%% @doc Renders expanded Space settings row.
%% @end
-spec space_row_expanded(SpaceId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
space_row_expanded(SpaceId, RowId) ->
    try
        {ok, #space_details{name = Name}} = cacheable_call(gr_providers, get_space_details, [provider, SpaceId]),
        SpinnerId = <<RowId/binary, "_spinner">>,
        ProvidersTableId = <<RowId/binary, "_providers">>,
        UsersTableId = <<RowId/binary, "_users">>,
        CancelSupportButtonId = <<RowId/binary, "_cancel_button">>,
        CancelSupportSpinnerId = <<RowId/binary, "_cancel_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = [
                    #table{
                        style = ?TABLE_STYLE,
                        body = lists:map(fun({Description, Main}) ->
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = Description
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = Main
                                        }
                                    }
                                ]
                            }
                        end, [{<<"Name">>, Name}, {<<"Space ID">>, SpaceId}])
                    },
                    #table{
                        class = <<"table table-bordered table-striped">>,
                        style = <<"width: 100%; margin: 0 auto; margin-bottom: 1em; table-layout: fixed;">>,
                        body = #tbody{
                            id = ProvidersTableId,
                            body = providers_table_collapsed(SpaceId, ProvidersTableId)
                        }
                    },
                    #table{
                        class = <<"table table-bordered table-striped">>,
                        style = <<"width: 100%; margin: 0 auto; margin-bottom: 1em; table-layout: fixed;">>,
                        body = #tbody{
                            id = UsersTableId,
                            body = users_table_collapsed(SpaceId, UsersTableId)
                        }
                    },
                    #button{
                        id = CancelSupportButtonId,
                        postback = {revoke_space_support, SpaceId, RowId, CancelSupportButtonId, CancelSupportSpinnerId},
                        class = <<"btn btn-danger btn-small">>,
                        style = <<"font-weight: bold;">>,
                        body = <<"Cancel support">>
                    },
                    #image{
                        id = CancelSupportSpinnerId,
                        image = <<"/images/spinner.gif">>,
                        style = <<"width: 2em; padding-left: 1em; display: none;">>
                    }
                ]
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button({space_row_collapse, SpaceId, RowId, SpinnerId})
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch details of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch details of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_providers, get_space_details, [provider, SpaceId]),
            space_row_collapsed(SpaceId, RowId)
    end.


%% add_space_row/2
%% ====================================================================
%% @doc Adds collapsed Space settings row to Spaces settings table.
%% @end
-spec add_space_row(SpaceId :: binary(), RowId :: binary()) -> Result when
    Result :: ok.
%% ====================================================================
add_space_row(SpaceId, RowId) ->
    Row = #tr{
        id = RowId,
        cells = space_row_collapsed(SpaceId, RowId)
    },
    gui_jq:insert_bottom(<<"spaces">>, Row).


%% providers_table_collapsed/2
%% ====================================================================
%% @doc Renders collapsed providers table for given Space.
%% @end
-spec providers_table_collapsed(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_collapsed(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Providers">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button(<<"Expand All">>, {providers_table_expand, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, ProviderIds} = cacheable_call(gr_spaces, get_providers, [provider, SpaceId]),
        Rows = lists:map(fun({ProviderId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = provider_row_collapsed(SpaceId, ProviderId, RowId)
            }
        end, lists:zip(ProviderIds, tl(lists:seq(0, length(ProviderIds))))),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch providers of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch providers of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_providers, [provider, SpaceId]),
            [Header]
    end.


%% providers_table_expanded/1
%% ====================================================================
%% @doc Renders expanded providers table for given Space.
%% @end
-spec providers_table_expanded(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
providers_table_expanded(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Providers">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button(<<"Collapse All">>, {providers_table_collapse, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, ProviderIds} = cacheable_call(gr_spaces, get_providers, [provider, SpaceId]),
        Rows = lists:map(fun({ProviderId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = provider_row_expanded(SpaceId, ProviderId, RowId)
            }
        end, lists:zip(ProviderIds, tl(lists:seq(0, length(ProviderIds))))),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch providers of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch providers of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_providers, [provider, SpaceId]),
            [Header]
    end.


%% provider_row_collapsed/3
%% ====================================================================
%% @doc Renders collapsed provider row for given Space.
%% @end
-spec provider_row_collapsed(SpaceId :: binary(), ProviderId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_collapsed(SpaceId, ProviderId, RowId) ->
    SpinnerId = <<RowId/binary, "_spinner">>,
    [
        #td{
            style = ?CONTENT_COLUMN_STYLE,
            body = #table{
                style = ?TABLE_STYLE,
                body = [
                    #tr{
                        cells = [
                            #td{
                                style = ?DESCRIPTION_STYLE,
                                body = #label{
                                    style = ?LABEL_STYLE,
                                    class = <<"label label-large label-inverse">>,
                                    body = <<"Provider ID">>
                                }
                            },
                            #td{
                                style = ?MAIN_STYLE,
                                body = #p{
                                    style = ?PARAGRAPH_STYLE,
                                    body = ProviderId
                                }
                            }
                        ]
                    }
                ]
            }
        },
        #td{
            id = SpinnerId,
            style = ?NAVIGATION_COLUMN_STYLE,
            body = expand_button({provider_row_expand, SpaceId, ProviderId, RowId, SpinnerId})
        }
    ].


%% provider_row_expanded/3
%% ====================================================================
%% @doc Renders expanded provider row for given Space.
%% @end
-spec provider_row_expanded(SpaceId :: binary(), ProviderId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
provider_row_expanded(SpaceId, ProviderId, RowId) ->
    try
        {ok, #provider_details{urls = URLs, redirection_point = RedirectionPoint}} =
            cacheable_call(gr_spaces, get_provider_details, [provider, SpaceId, ProviderId]),
        SpinnerId = <<RowId/binary, "_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = [
                    #table{
                        style = ?TABLE_STYLE,
                        body = [
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"Provider ID">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = ProviderId
                                        }
                                    }
                                ]
                            },
                            #tr{
                                cells = [
                                    #td{
                                        style = <<(?DESCRIPTION_STYLE)/binary, " vertical-align: top;">>,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"URLs">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #list{
                                            style = <<"list-style-type: none; margin: 0 auto;">>,
                                            body = lists:map(fun(URL) ->
                                                #li{body = #p{
                                                    style = ?PARAGRAPH_STYLE,
                                                    body = URL}
                                                }
                                            end, URLs)
                                        }
                                    }
                                ]
                            },
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = <<"Redirection point">>
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = RedirectionPoint
                                        }
                                    }
                                ]
                            }
                        ]
                    }
                ]
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button({provider_row_collapse, SpaceId, ProviderId, RowId, SpinnerId})
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch details of provider with ID ~p: ~p", [ProviderId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch details of provider with ID: <b>", ProviderId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_provider_details, [provider, SpaceId, ProviderId]),
            provider_row_collapsed(SpaceId, ProviderId, RowId)
    end.


%% users_table_collapsed/2
%% ====================================================================
%% @doc Renders collapsed users table for given Space.
%% @end
-spec users_table_collapsed(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
users_table_collapsed(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Users">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button(<<"Expand All">>, {users_table_expand, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, UserIds} = cacheable_call(gr_spaces, get_users, [provider, SpaceId]),
        Rows = lists:map(fun({UserId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = user_row_collapsed(SpaceId, UserId, RowId)
            }
        end, lists:zip(UserIds, tl(lists:seq(0, length(UserIds))))),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch users of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch users of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_users, [provider, SpaceId]),
            [Header]
    end.


%% users_table_expanded/1
%% ====================================================================
%% @doc Renders expanded users table for given Space.
%% @end
-spec users_table_expanded(SpaceId :: binary(), TableId :: binary()) -> Result when
    Result :: [#tr{}].
%% ====================================================================
users_table_expanded(SpaceId, TableId) ->
    SpinnerId = <<TableId/binary, "_spinner">>,
    Header = #tr{
        cells = [
            #th{
                style = <<"font-size: large;">>,
                body = <<"Users">>
            },
            #th{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button(<<"Collapse All">>, {users_table_collapse, SpaceId, TableId, SpinnerId})
            }
        ]
    },
    try
        {ok, UserIds} = cacheable_call(gr_spaces, get_users, [provider, SpaceId]),
        Rows = lists:map(fun({UserId, Counter}) ->
            RowId = <<TableId/binary, "_", (integer_to_binary(Counter))/binary>>,
            #tr{
                id = RowId,
                cells = user_row_expanded(SpaceId, UserId, RowId)
            }
        end, lists:zip(UserIds, tl(lists:seq(0, length(UserIds))))),
        [Header | Rows]
    catch
        _:Reason ->
            ?error("Cannot fetch users of Space with ID ~p: ~p", [SpaceId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch users of Space with ID: <b>", SpaceId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_users, [provider, SpaceId]),
            [Header]
    end.


%% user_row_collapsed/3
%% ====================================================================
%% @doc Renders collapsed user row for given Space.
%% @end
-spec user_row_collapsed(SpaceId :: binary(), UserId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
user_row_collapsed(SpaceId, UserId, RowId) ->
    try
        {ok, #user_details{name = Name}} = cacheable_call(gr_spaces, get_user_details, [provider, SpaceId, UserId]),
        SpinnerId = <<RowId/binary, "_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = #table{
                    style = ?TABLE_STYLE,
                    body = [
                        #tr{
                            cells = [
                                #td{
                                    style = ?DESCRIPTION_STYLE,
                                    body = #label{
                                        style = ?LABEL_STYLE,
                                        class = <<"label label-large label-inverse">>,
                                        body = <<"Name">>
                                    }
                                },
                                #td{
                                    style = ?MAIN_STYLE,
                                    body = #p{
                                        style = ?PARAGRAPH_STYLE,
                                        body = Name
                                    }
                                }
                            ]
                        }
                    ]
                }
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = expand_button({user_row_expand, SpaceId, UserId, RowId, SpinnerId})
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch details of user with ID ~p: ~p", [UserId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch details of user with ID: <b>", UserId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_user_details, [provider, SpaceId, UserId]),
            user_row_collapsed(SpaceId, UserId, RowId)
    end.


%% user_row_expanded/3
%% ====================================================================
%% @doc Renders expanded user row for given Space.
%% @end
-spec user_row_expanded(SpaceId :: binary(), UserId :: binary(), RowId :: binary()) -> Result when
    Result :: [#td{}].
%% ====================================================================
user_row_expanded(SpaceId, UserId, RowId) ->
    try
        {ok, #user_details{name = Name}} = cacheable_call(gr_spaces, get_user_details, [provider, SpaceId, UserId]),
        SpinnerId = <<RowId/binary, "_spinner">>,
        [
            #td{
                style = ?CONTENT_COLUMN_STYLE,
                body = [
                    #table{
                        style = ?TABLE_STYLE,
                        body = lists:map(fun({Description, Main}) ->
                            #tr{
                                cells = [
                                    #td{
                                        style = ?DESCRIPTION_STYLE,
                                        body = #label{
                                            style = ?LABEL_STYLE,
                                            class = <<"label label-large label-inverse">>,
                                            body = Description
                                        }
                                    },
                                    #td{
                                        style = ?MAIN_STYLE,
                                        body = #p{
                                            style = ?PARAGRAPH_STYLE,
                                            body = Main
                                        }
                                    }
                                ]
                            }
                        end, [{<<"Name">>, Name}, {<<"User ID">>, UserId}])
                    }
                ]
            },
            #td{
                id = SpinnerId,
                style = ?NAVIGATION_COLUMN_STYLE,
                body = collapse_button({user_row_collapse, SpaceId, UserId, RowId, SpinnerId})
            }
        ]
    catch
        _:Reason ->
            ?error("Cannot fetch details of user with ID ~p: ~p", [UserId, Reason]),
            onepanel_gui_utils:message(<<"error_message">>, <<"Cannot fetch details of user with ID: <b>", UserId/binary, "</b>."
            "<br>Please try again later.">>),
            clear_call(gr_spaces, get_user_details, [provider, SpaceId, UserId]),
            user_row_collapsed(SpaceId, UserId, RowId)
    end.


%% collapse_button/1
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
collapse_button(Postback) ->
    collapse_button(<<"Collapse">>, Postback).


%% collapse_button/2
%% ====================================================================
%% @doc Renders collapse button.
%% @end
-spec collapse_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
collapse_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large; vertical-align: top;">>,
            class = <<"fui-triangle-up">>
        }
    }.


%% expand_button/1
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
expand_button(Postback) ->
    expand_button(<<"Expand">>, Postback).


%% expand_button/2
%% ====================================================================
%% @doc Renders expand button.
%% @end
-spec expand_button(Title :: binary(), Postback :: term()) -> Result when
    Result :: #link{}.
%% ====================================================================
expand_button(Title, Postback) ->
    #link{
        title = Title,
        class = <<"glyph-link">>,
        postback = Postback,
        body = #span{
            style = <<"font-size: large;  vertical-align: top;">>,
            class = <<"fui-triangle-down">>
        }
    }.


%% spinner/0
%% ====================================================================
%% @doc Renders spinner GIF.
%% @end
-spec spinner() -> Result when
    Result :: #image{}.
%% ====================================================================
spinner() ->
    #image{
        image = <<"/images/spinner.gif">>,
        style = <<"width: 1.5em;">>
    }.


%% cacheable_call/3
%% ====================================================================
%% @doc Calls given function with given arguments and returns result.
%% If given function was previously called with same arguments and cache
%% has not been cleared it returns previous function call result.
%% @end
-spec cacheable_call(Module :: module(), Function :: atom(), Args :: [term()]) -> Result when
    Result :: term().
%% ====================================================================
cacheable_call(Module, Function, Args) ->
    case get({Module, Function, Args}) of
        undefined ->
            Result = apply(Module, Function, Args),
            put({Module, Function, Args}, Result),
            Result;
        Result ->
            Result
    end.


%% clear_call/3
%% ====================================================================
%% @doc Clears cache for given function call.
%% @end
-spec clear_call(Module :: module(), Function :: atom(), Args :: [term()]) -> Result when
    Result :: term().
%% ====================================================================
clear_call(Module, Function, Args) ->
    put({Module, Function, Args}, undefined).


%% comet_loop/1
%% ====================================================================
%% @doc Handles spaces management actions.
%% @end
-spec comet_loop(State :: #?STATE{}) -> Result when
    Result :: {error, Reason :: term()}.
%% ====================================================================
comet_loop({error, Reason}) ->
    {error, Reason};

comet_loop(#?STATE{} = State) ->
    NewState = try
        receive
            {create_space, Name, Token, RowId} ->
                gui_jq:css(<<"settings_spinner">>, <<"visibility">>, <<"visible">>),
                gui_comet:flush(),
                case gr_providers:create_space(provider, [{<<"name">>, Name}, {<<"token">>, Token}]) of
                    {ok, SpaceId} ->
                        onepanel_gui_utils:message(<<"ok_message">>, <<"Created Space ID: <b>", SpaceId/binary, "</b>">>),
                        add_space_row(SpaceId, RowId);
                    Other ->
                        ?error("Cannot create Space ~p associated with token ~p: ~p", [Name, Token, Other]),
                        onepanel_gui_utils:message(<<"error_message">>, <<"Operation failed.<br>Please try again later.">>)
                end,
                gui_jq:css(<<"settings_spinner">>, <<"visibility">>, <<"hidden">>),
                gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State;

            {support_space, Token, RowId} ->
                gui_jq:css(<<"settings_spinner">>, <<"visibility">>, <<"visible">>),
                gui_comet:flush(),
                case gr_providers:support_space(provider, [{<<"token">>, Token}]) of
                    {ok, SpaceId} ->
                        onepanel_gui_utils:message(<<"ok_message">>, <<"Supported Space ID: <b>", SpaceId/binary, "</b>">>),
                        add_space_row(SpaceId, RowId);
                    Other ->
                        ?error("Cannot support Space associated with token ~p: ~p", [Token, Other]),
                        onepanel_gui_utils:message(<<"error_message">>, <<"Operation failed.<br>Please try again later.">>)
                end,
                gui_jq:css(<<"settings_spinner">>, <<"visibility">>, <<"hidden">>),
                gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State;

            {revoke_space_support, SpaceId, RowId, ButtonId, SpinnerId} ->
                gui_jq:show(SpinnerId),
                gui_comet:flush(),
                case gr_providers:revoke_space_support(provider, SpaceId) of
                    ok ->
                        onepanel_gui_utils:message(<<"ok_message">>, <<"Space: <b>", SpaceId/binary, "</b> is no longer supported.">>),
                        gui_jq:remove(RowId),
                        clear_call(gr_providers, get_spaces, [provider]);
                    Other ->
                        ?error("Cannot cancel support for Space ~p: ~p", [SpaceId, Other]),
                        onepanel_gui_utils:message(<<"error_message">>, <<"Operation failed.<br>Please try again later.">>)
                end,
                gui_jq:hide(SpinnerId),
                gui_jq:prop(ButtonId, <<"disabled">>, <<"">>),
                gui_comet:flush(),
                State;

            {spaces_table_collapse, TableId} ->
                gui_jq:update(TableId, spaces_table_collapsed(TableId)),
                gui_comet:flush(),
                State;

            {spaces_table_expand, TableId} ->
                gui_jq:update(TableId, spaces_table_expanded(TableId)),
                gui_comet:flush(),
                State;

            {space_row_collapse, SpaceId, RowId} ->
                gui_jq:update(RowId, space_row_collapsed(SpaceId, RowId)),
                gui_comet:flush(),
                State;

            {space_row_expand, SpaceId, RowId} ->
                gui_jq:update(RowId, space_row_expanded(SpaceId, RowId)),
                gui_comet:flush(),
                State;

            {providers_table_collapse, SpaceId, TableId} ->
                gui_jq:update(TableId, providers_table_collapsed(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {providers_table_expand, SpaceId, TableId} ->
                gui_jq:update(TableId, providers_table_expanded(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {provider_row_collapse, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, provider_row_collapsed(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State;

            {provider_row_expand, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, provider_row_expanded(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State;

            {users_table_collapse, SpaceId, TableId} ->
                gui_jq:update(TableId, users_table_collapsed(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {users_table_expand, SpaceId, TableId} ->
                gui_jq:update(TableId, users_table_expanded(SpaceId, TableId)),
                gui_comet:flush(),
                State;

            {user_row_collapse, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, user_row_collapsed(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State;

            {user_row_expand, SpaceId, ProviderId, RowId} ->
                gui_jq:update(RowId, user_row_expanded(SpaceId, ProviderId, RowId)),
                gui_comet:flush(),
                State
        end
               catch Type:Message ->
                   ?error("Comet process exception: ~p:~p", [Type, Message]),
                   onepanel_gui_utils:message(<<"error_message">>, <<"There has been an error in comet process. Please refresh the page.">>),
                   {error, Message}
               end,
    ?MODULE:comet_loop(NewState).


%% ====================================================================
%% Events handling
%% ====================================================================

%% event/1
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec event(Event :: term()) -> no_return().
%% ====================================================================
event(init) ->
    {ok, Pid} = gui_comet:spawn(fun() -> comet_loop(#?STATE{}) end),
    put(?COMET_PID, Pid),
    gui_jq:wire(#api{name = "createSpace", tag = "createSpace"}, false),
    gui_jq:wire(#api{name = "supportSpace", tag = "supportSpace"}, false),
    gui_jq:wire(#api{name = "cancelSpaceSupport", tag = "cancelSpaceSupport"}, false),
    gui_jq:bind_key_to_click_on_class(<<"13">>, <<"confirm">>);

event(to_account_page) ->
    gui_jq:redirect(?PAGE_SPACES_ACCOUNT);

event(create_space) ->
    Title = <<"Create Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<p id=\"create_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"create_space_name\" type=\"text\" style=\"width: 100%;\" placeholder=\"Name\">",
    "<input id=\"create_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    "</div>">>,
    Script = <<"var alert = $(\"#create_space_alert\");",
    "var name = $.trim($(\"#create_space_name\").val());",
    "var token = $.trim($(\"#create_space_token\").val());",
    "if(name.length == 0) { alert.html(\"Please provide Space name.\"); alert.fadeIn(300); return false; }",
    "else if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    "else { createSpace([name, token]); return true; }">>,
    gui_jq:dialog_popup(Title, Message, Script),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#create_space_name\").focus(); });">>);

event(support_space) ->
    Title = <<"Support Space">>,
    Message = <<"<div style=\"margin: 0 auto; width: 80%;\">",
    "<p id=\"support_space_alert\" style=\"width: 100%; color: red; font-size: medium; text-align: center; display: none;\"></p>",
    "<input id=\"support_space_token\" type=\"text\" style=\"width: 100%;\" placeholder=\"Token\">",
    "</div>">>,
    Script = <<"var alert = $(\"#support_space_alert\");",
    "var token = $.trim($(\"#support_space_token\").val());",
    "if(token.length == 0) { alert.html(\"Please provide Space token.\"); alert.fadeIn(300); return false; }",
    "else { supportSpace([token]); return true; }">>,
    gui_jq:dialog_popup(Title, Message, Script),
    gui_jq:wire(<<"box.on('shown',function(){ $(\"#support_space_token\").focus(); });">>);

event({revoke_space_support, SpaceId, RowId, ButtonId, SpinnerId}) ->
    Message = <<"Are you sure you want to stop supporting Space: <b>", SpaceId/binary, "</b>?<br>This operation cannot be undone.">>,
    Script = <<"cancelSpaceSupport(['", SpaceId/binary, "','", RowId/binary, "','", ButtonId/binary, "','", SpinnerId/binary, "']);">>,
    gui_jq:confirm_popup(Message, Script);

event({spaces_table_collapse, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {spaces_table_collapse, TableId},
    gui_jq:update(SpinnerId, spinner());

event({spaces_table_expand, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {spaces_table_expand, TableId},
    gui_jq:update(SpinnerId, spinner());

event({space_row_collapse, SpaceId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {space_row_collapse, SpaceId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({space_row_expand, SpaceId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {space_row_expand, SpaceId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({providers_table_collapse, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {providers_table_collapse, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({providers_table_expand, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {providers_table_expand, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({provider_row_collapse, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {provider_row_collapse, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({provider_row_expand, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {provider_row_expand, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({users_table_collapse, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {users_table_collapse, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({users_table_expand, SpaceId, TableId, SpinnerId}) ->
    get(?COMET_PID) ! {users_table_expand, SpaceId, TableId},
    gui_jq:update(SpinnerId, spinner());

event({user_row_collapse, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {user_row_collapse, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({user_row_expand, SpaceId, ProviderId, RowId, SpinnerId}) ->
    get(?COMET_PID) ! {user_row_expand, SpaceId, ProviderId, RowId},
    gui_jq:update(SpinnerId, spinner());

event({close_message, MessageId}) ->
    gui_jq:hide(MessageId);

event(terminate) ->
    ok.


%% api_event/3
%% ====================================================================
%% @doc Handles page events.
%% @end
-spec api_event(Name :: string(), Args :: string(), Req :: string()) -> no_return().
%% ====================================================================
api_event("createSpace", Args, _) ->
    [Name, Token] = mochijson2:decode(Args),
    Id = gui_ctx:get(?ID),
    gui_ctx:put(?ID, Id + 1),
    get(?COMET_PID) ! {create_space, Name, Token, <<"space_", (integer_to_binary(Id + 1))/binary>>},
    gui_jq:prop(<<"create_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("supportSpace", Args, _) ->
    [Token] = mochijson2:decode(Args),
    Id = gui_ctx:get(?ID),
    gui_ctx:put(?ID, Id + 1),
    get(?COMET_PID) ! {support_space, Token, <<"space_", (integer_to_binary(Id + 1))/binary>>},
    gui_jq:prop(<<"support_space_button">>, <<"disabled">>, <<"disabled">>);

api_event("cancelSpaceSupport", Args, _) ->
    [SpaceId, RowId, ButtonId, SpinnerId] = mochijson2:decode(Args),
    get(?COMET_PID) ! {revoke_space_support, SpaceId, RowId, ButtonId, SpinnerId},
    gui_jq:prop(ButtonId, <<"disabled">>, <<"disabled">>).
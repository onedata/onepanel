%%%-------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2019 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles translation of middleware results concerning
%%% the onp_panel resource type into REST responses.
%%% @end
%%%-------------------------------------------------------------------
-module(onepanel_rest_translator).
-author("Wojciech Geisler").

-include("http/rest.hrl").
-include("middleware/middleware.hrl").
-include("modules/onepanel_dns.hrl").
-include_lib("ctool/include/graph_sync/gri.hrl").

-export([create_response/3, get_response/2]).


%%%===================================================================
%%% API
%%%===================================================================


-spec create_response(gri:gri(), gs_protocol:data_format(), Result) -> #rest_resp{}
    when Result :: term() | {gri:gri(), term()}.
create_response(#gri{aspect = invite_token}, value, InviteToken) ->
    ?OK_REPLY(#{<<"inviteToken">> => InviteToken}).


-spec get_response(gri:gri(), Resource :: term()) -> #rest_resp{}.
get_response(#gri{aspect = cookie}, Cookie) ->
    ?OK_REPLY(atom_to_binary(Cookie, utf8));

get_response(#gri{aspect = test_image}, <<PNG/binary>>) ->
    ?OK_REPLY({binary, PNG});

get_response(#gri{aspect = dns_check}, Result) ->
    IpOrTxtToBinary = fun
        ({_, _, _, _} = Ip) ->
            {ok, Binary} = ip_utils:to_binary(Ip),
            Binary;
        (String) ->
            % some DNS results are not IPs - e.g. NS records
            onepanel_utils:convert(String, binary)
    end,

    JsonMap = maps:fold(fun
        (Key, #dns_check{
            summary = Summary, expected = E, got = G, bind_records = Records
        }, Acc) ->
            Acc#{str_utils:to_binary(Key) => #{
                <<"summary">> => Summary,
                <<"expected">> => lists:map(IpOrTxtToBinary, E),
                <<"got">> => lists:map(IpOrTxtToBinary, G),
                <<"recommended">> => Records
            }};
        (timestamp, Seconds, Acc) ->
            Acc#{<<"timestamp">> => time_utils:seconds_to_iso8601(Seconds)};
        (Key, LiteralValue, Acc) ->
            Acc#{str_utils:to_binary(Key) => LiteralValue}
    end, #{}, Result),
    ?OK_REPLY(JsonMap);


get_response(#gri{aspect = emergency_passphrase}, IsSet) when is_boolean(IsSet)->
    ?OK_REPLY(#{<<"isSet">> => IsSet});

get_response(#gri{}, Data) when is_map(Data) ->
    ?OK_REPLY(Data).

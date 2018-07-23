%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains record definitions for onepanel_dns_check
%%% module and its clients.
%%% @end
%%%--------------------------------------------------------------------
-ifndef(ONEPANEL_DNS_CHECK_HRL).
-define(ONEPANEL_DNS_CHECK_HRL, 1).

% Results description
-record(dns_check, {
    summary :: onepanel_dns:summary(),
    expected = [] :: [onepanel_dns:dns_value()],
    got = [] :: [onepanel_dns:dns_value()],
    excessive = [] :: [onepanel_dns:dns_value()],
    missing = [] :: [onepanel_dns:dns_value()],
    bind_records = [] :: [binary()] % examples of BIND records satisfying the check
}).

-endif.


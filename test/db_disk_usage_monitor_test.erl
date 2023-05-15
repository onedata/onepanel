%%%--------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc
%%% Unit tests for db_disk_usage_monitor module.
%%% @end
%%%--------------------------------------------------------------------
-module(db_disk_usage_monitor_test).
-author("Bartosz Walkowicz").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Test functions
%%%===================================================================


parse_du_cmd_output_test_() ->
    F = fun db_disk_usage_monitor:parse_du_cmd_output/1,

    [
        ?_assertEqual(339989717, F(<<"339989717\t/opt/couchbase">>)),
        ?_assertEqual(717, F(<<"717\t/opt/couchbase">>)),
        ?_assertEqual(339989717, F(<<"339989717\t/opt/couch">>)),
        ?_assertError({badmatch, nomatch}, F(<<"/opt/couchbase">>)),
        ?_assertError({badmatch, nomatch}, F(<<"123456">>)),
        ?_assertError({badmatch, nomatch}, F(<<"339989717 /opt/couchbase">>)),
        ?_assertError({badmatch, nomatch}, F(<<"error">>))
    ].


parse_df_cmd_output_test_() ->
    F = fun db_disk_usage_monitor:parse_df_cmd_output/1,

    [
        ?_assertEqual(13460676608, F(<<"      Avail\n13460676608">>)),
        ?_assertEqual(608, F(<<"      Avail\n608">>)),
        ?_assertError({badmatch, nomatch}, F(<<"13460676608">>)),
        ?_assertError({badmatch, nomatch}, F(<<"Avail 13460676608">>)),
        ?_assertError({badmatch, nomatch}, F(<<"error">>))
    ].


-endif.

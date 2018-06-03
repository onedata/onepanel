%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This header contains constants used in input validation.
%%% @end
%%%--------------------------------------------------------------------
-author("Wojciech Geisler").
-ifndef(VALIDATION_HRL).
-define(VALIDATION_HRL, 1).

-define(USERNAME_VALIDATION_REGEXP, <<"^[a-z0-9A-Z][a-z0-9A-Z_-]{0,13}[a-z0-9A-Z]$">>).

-define(PASSWORD_MIN_LENGTH, 8).
-define(PASSWORD_FORBIDDEN_REGEXP, <<":">>).


-endif.

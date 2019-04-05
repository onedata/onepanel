%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc This behaviour describes functions which must be provided
%%% by a service module to cooperate with service_letsencrypt.
%%% @end
%%%--------------------------------------------------------------------
-module(letsencrypt_plugin_behaviour).
-author("Wojciech Geisler").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Sets txt record in the dns.
%%--------------------------------------------------------------------
-callback set_txt_record(#{txt_name  := binary(), txt_ttl := non_neg_integer(),
                           txt_value := binary(), _=>_}) -> ok.

%%--------------------------------------------------------------------
%% Removes txt record from dns.
%%--------------------------------------------------------------------
-callback remove_txt_record(#{txt_name := binary(), _ => _}) -> ok.

%%--------------------------------------------------------------------
%% Sets static http content.
%%--------------------------------------------------------------------
-callback set_http_record(Name :: binary(), Value :: binary()) -> ok.

%%--------------------------------------------------------------------
%% Returns hostname of the server at which txt record is set.
%%--------------------------------------------------------------------
-callback get_dns_server() -> string().

%%--------------------------------------------------------------------
%% Returns current domain.
%%--------------------------------------------------------------------
-callback get_domain() -> binary().

%%--------------------------------------------------------------------
%% Returns admin email to be used in Let's Encrypt registration.
%%--------------------------------------------------------------------
-callback get_admin_email(service:ctx()) -> binary() | undefined.

%%--------------------------------------------------------------------
%% Checks if Let's Encrypt challenge of given type can be currently
%% fulfilled by the plugin service.
%%--------------------------------------------------------------------
-callback supports_letsencrypt_challenge(letsencrypt_api:challenge_type()) ->
    boolean().

%%--------------------------------------------------------------------
%% Clears worker ssl cache to ensure certificates changed on disk
%% are reloaded.
%%--------------------------------------------------------------------
-callback reload_webcert(service:ctx()) -> ok.

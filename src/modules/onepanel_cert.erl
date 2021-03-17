%%%--------------------------------------------------------------------
%%% @author Wojciech Geisler
%%% @copyright (C) 2018 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%--------------------------------------------------------------------
%%% @doc Module providing utilities for managing and reading
%%% ssl certificates.
%%% @end
%%%--------------------------------------------------------------------
-module(onepanel_cert).
-author("Wojciech Geisler").

-include("modules/errors.hrl").
-include_lib("ctool/include/logging.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-type pem() :: binary().
-type cert() :: #'Certificate'{}.

-export_type([pem/0, cert/0]).

%% API
-export([generate_csr_and_key/1, backup_exisiting_certs/0,
    list_certificate_files/0]).
-export([read/1, verify_hostname/2, get_subject_cn/1, get_issuer_cn/1,
    get_seconds_till_expiration/1]).
-export([get_times/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Generates Certificate Signing Request and matching private key.
%% @end
%%--------------------------------------------------------------------
-spec generate_csr_and_key(Domain :: string()) ->
    {ok, CSR :: pem(), Key :: pem()} | error.
generate_csr_and_key(Domain) ->
    KeyFile = onepanel_shell:mktemp(),
    CSRFile = onepanel_shell:mktemp(),

    try
        os:cmd([
            "openssl req -new -batch"
            " -subj '/CN=", Domain, "/'",
            " -addext '/subjectAltName=DNS:", Domain, "/'",
            " -nodes", % no password on keyfile
            " -keyout ", KeyFile,
            " -out ", CSRFile]),

        {ok, KeyPem} = file:read_file(KeyFile),
        {ok, CSRPem} = file:read_file(CSRFile),

        % Check expected result
        case {public_key:pem_decode(CSRPem), public_key:pem_decode(KeyPem)} of
            {[{'CertificationRequest', _CSRDer, not_encrypted}],
                [{'PrivateKeyInfo', _KeyDer, not_encrypted}]} ->
                {ok, CSRPem, KeyPem};
            _ ->
                ?error("Error generating CSR for Let's Encrypt."),
                error
        end
    after
        file:delete(KeyFile),
        file:delete(CSRFile)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Reads first certificate from a pem file.
%% @end
%%--------------------------------------------------------------------
-spec read(Path :: file:name_all()) ->
    {ok, cert()} | {error, Reason :: term()}.
read(Path) ->
    case file:read_file(Path) of
        {ok, Pem} ->
            case public_key:pem_decode(Pem) of
                [Entries | _] ->
                    #'Certificate'{} = Cert = public_key:pem_entry_decode(Entries),
                    {ok, Cert};
                [] -> {error, bad_cert}
            end;
        Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Extracts subject Common Name from a certificate.
%% If the certificate containts no Common Name 'undefined' is returned.
%% @end
%%--------------------------------------------------------------------
-spec get_subject_cn(cert() | file:name_all()) ->
    binary() | undefined.
get_subject_cn(#'Certificate'{} = Cert) ->
    #'Certificate'{tbsCertificate = #'TBSCertificate'{
        subject = {rdnSequence, SubjectParts}
    }} = Cert,
    get_common_name(SubjectParts).


%%--------------------------------------------------------------------
%% @doc
%% Extracts issuer Common Name from a certificate.
%% If the certificate containts no issuer Common Name 'undefined'
%% is returned.
%% @end
%%--------------------------------------------------------------------
-spec get_issuer_cn(cert() | file:name_all()) ->
    binary() | undefined.
get_issuer_cn(#'Certificate'{} = Cert) ->
    #'Certificate'{tbsCertificate = #'TBSCertificate'{
        issuer = {rdnSequence, IssuerParts}
    }} = Cert,
    get_common_name(IssuerParts).


-spec verify_hostname(cert(), Hostname :: binary() | string()) ->
    valid | invalid | error.
verify_hostname(Cert, Hostname) ->
    HostnameStr = onepanel_utils:convert(Hostname, list),
    OtpCert = to_otp(Cert),

    case ssl_verify_hostname:verify_cert_hostname(OtpCert, HostnameStr) of
        {fail, unable_to_decode_common_name} -> error;
        {fail, _} -> invalid;
        {valid, _} -> valid
    end.


%%--------------------------------------------------------------------
%% @doc
%% Returns seconds until expiration time of a given certificate.
%% @end
%%--------------------------------------------------------------------
-spec get_seconds_till_expiration(cert()) -> integer().
get_seconds_till_expiration(#'Certificate'{} = Cert) ->
    {_, ExpirationTime} = get_times(Cert),
    ExpirationTime - global_clock:timestamp_seconds().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns times when certificate validity begins and ends.
%% @end
%%--------------------------------------------------------------------
-spec get_times(cert()) ->
    {Since :: integer(), Until :: integer()}.
get_times(#'Certificate'{} = Cert) ->
    #'TBSCertificate'{
        validity = Validity
    } = Cert#'Certificate'.tbsCertificate,
    {'Validity', NotBeforeStr, NotAfterStr} = Validity,
    {time_str_to_seconds(NotBeforeStr), time_str_to_seconds(NotAfterStr)}.


%%--------------------------------------------------------------------
%% @doc
%% Copies existing certificates to files with timestamped suffix '.bak'.
%% @end
%%--------------------------------------------------------------------
-spec backup_exisiting_certs() -> ok.
backup_exisiting_certs() ->
    WebKeyPath = onepanel_env:get(web_key_file),
    WebCertPath = onepanel_env:get(web_cert_file),
    WebChainPath = onepanel_env:get(web_cert_chain_file),

    lists:foreach(fun(Path) ->
        case filelib:is_regular(Path) of
            false -> ok;
            true ->
                BackupPath = str_utils:format("~s.~B.bak", [
                    Path, global_clock:timestamp_seconds()
                ]),
                file:copy(Path, BackupPath)
        end
    end, [WebKeyPath, WebCertPath, WebChainPath]).


%%--------------------------------------------------------------------
%% @doc
%% Lists certificate-related files present on the current node.
%% That is: web cert, key, chain file; any content of Let's Encrypt
%% credentials directory.
%% @end
%%--------------------------------------------------------------------
-spec list_certificate_files() -> [file:filename()].
list_certificate_files() ->
    lists:filter(fun filelib:is_regular/1, [
        onepanel_env:get(web_key_file),
        onepanel_env:get(web_cert_file),
        onepanel_env:get(web_cert_chain_file)
        | filelib:wildcard([onepanel_env:get(letsencrypt_keys_dir), "/**"])
    ]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts plain 'Certificate' record to 'OTPCertificate'.
%% @end
%%--------------------------------------------------------------------
-spec to_otp(cert()) -> #'OTPCertificate'{}.
to_otp(Cert) ->
    Der = public_key:pkix_encode('Certificate', Cert, plain),
    public_key:pkix_decode_cert(Der, otp).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns Common Name from issuer or subject attribues.
%% @end
%%--------------------------------------------------------------------
-spec get_common_name([#'AttributeTypeAndValue'{}]) -> binary() | undefined.
get_common_name(RdnSequence) ->
    case lists:filtermap(fun
        ([#'AttributeTypeAndValue'{
            type = ?'id-at-commonName',
            value = <<_:16, CN/binary>>}]) -> {true, CN};
        (_) -> false
    end, RdnSequence) of
        [CN|_] -> CN;
        [] -> undefined
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts time string YYmmDDHHMMSS to epoch seconds.
%% @end
%%--------------------------------------------------------------------
-spec time_str_to_seconds({utcTime | generalTime, string()}) -> integer().
time_str_to_seconds({utcTime, [Y1,Y2,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,Z]}) ->
    case list_to_integer([Y1,Y2]) of
        N when N >= 50 ->
            time_str_to_seconds({generalTime,
                [$1,$9,Y1,Y2,M1,M2,D1,D2,
                    H1,H2,M3,M4,S1,S2,Z]});
        _ ->
            time_str_to_seconds({generalTime,
                [$2,$0,Y1,Y2,M1,M2,D1,D2,
                    H1,H2,M3,M4,S1,S2,Z]})
    end;

time_str_to_seconds({_,[Y1,Y2,Y3,Y4,M1,M2,D1,D2,H1,H2,M3,M4,S1,S2,$Z]}) ->
    Year  = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day   = list_to_integer([D1, D2]),
    Hour  = list_to_integer([H1, H2]),
    Min   = list_to_integer([M3, M4]),
    Sec   = list_to_integer([S1, S2]),
    time:datetime_to_seconds({{Year, Month, Day}, {Hour, Min, Sec}}).

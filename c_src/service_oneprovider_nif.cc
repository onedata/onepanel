/*********************************************************************
* @author Krzysztof Trzepla
* @copyright (C): 2016 ACK CYFRONET AGH
* This software is released under the MIT license
* cited in 'LICENSE.txt'.
* @end
**********************************************************************
* @doc This is an interface for Erlang NIF library. It contains a method
* that allows to create private key and Certificate Signing Request using
* Botan library.
* @end
*********************************************************************/

#include "nifpp.h"

#include <botan/auto_rng.h>
#include <botan/dsa.h>
#include <botan/init.h>
#include <botan/rsa.h>
#include <botan/x509self.h>

#include <sstream>
#include <string>

#define KEY_SIZE 4096

namespace {

std::tuple<nifpp::str_atom, std::string, std::string> create_csr(
    const std::string &password)
{
    using namespace Botan;

    LibraryInitializer init;
    std::stringstream keyString, csrString;

    try {
        AutoSeeded_RNG rng;
        RSA_PrivateKey key{rng, KEY_SIZE};
        keyString << PKCS8::PEM_encode(key, rng, password);

        // default values which will be overwritten by onezone
        X509_Cert_Options options;
        options.common_name = "CN";
        options.country = "AU";

        PKCS10_Request csr =
            X509::create_cert_req(options, key, "SHA-256", rng);
        csrString << csr.PEM_encode();
    }
    catch (std::exception &e) {
        return std::make_tuple("error", e.what(), "");
    }

    return std::make_tuple("ok", keyString.str(), csrString.str());
}
}

extern "C" {

static ERL_NIF_TERM create_csr_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        std::string password;

        nifpp::get_throws(env, argv[0], password);

        return nifpp::make(env, create_csr(password));
    }
    catch (nifpp::badarg) {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {{"create_csr", 1, create_csr_nif}};

ERL_NIF_INIT(service_oneprovider_nif, nif_funcs, NULL, NULL, NULL, NULL)

} // extern C

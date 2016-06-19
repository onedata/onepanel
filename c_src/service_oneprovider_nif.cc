/*********************************************************************
*  @author Krzysztof Trzepla
*  @copyright (C): 2016 ACK CYFRONET AGH
*  This software is released under the MIT license
*  cited in 'LICENSE.txt'.
*  @end
**********************************************************************
*  @doc This is an interface for Erlang NIF library. It contains one
*  method that allows to create private key and Certificate Signing
*  Request using Botan library.
*  @end
*********************************************************************/

#include "nifpp.h"

#include <botan/auto_rng.h>
#include <botan/dsa.h>
#include <botan/init.h>
#include <botan/rsa.h>
#include <botan/x509self.h>

#include <fstream>
#include <iostream>
#include <string>

#define KEY_SIZE 4096

namespace {

std::tuple<nifpp::str_atom, std::string> create_csr(const std::string &password,
    const std::string &keyPath, const std::string &csrPath)
{
    using namespace Botan;

    LibraryInitializer init;
    try {
        AutoSeeded_RNG rng;
        RSA_PrivateKey privateKey(rng, KEY_SIZE);
        std::ofstream keyFile(keyPath);
        keyFile << PKCS8::PEM_encode(privateKey, rng, password);

        X509_Cert_Options options;

        // default values which will be overwritten by onezone
        options.common_name = "CN";
        options.country = "AU";

        PKCS10_Request request =
            X509::create_cert_req(options, privateKey, "SHA-256", rng);

        std::ofstream requestFile(csrPath);
        requestFile << request.PEM_encode();
    }
    catch (std::exception &e) {
        return std::make_tuple("error", e.what());
    }

    return std::make_tuple("ok", "");
}
}

extern "C" {

static ERL_NIF_TERM create_csr_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        std::string password;
        std::string keyPath;
        std::string csrPath;

        nifpp::get_throws(env, argv[0], password);
        nifpp::get_throws(env, argv[1], keyPath);
        nifpp::get_throws(env, argv[2], csrPath);

        return nifpp::make(env, create_csr(password, keyPath, csrPath));
    }
    catch (nifpp::badarg) {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {{"create_csr", 3, create_csr_nif}};

ERL_NIF_INIT(service_oneprovider, nif_funcs, NULL, NULL, NULL, NULL)

} // extern C

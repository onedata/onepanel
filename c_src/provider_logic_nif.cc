/*********************************************************************
*  @author Krzysztof Trzepla
*  @copyright (C): 2014 ACK CYFRONET AGH
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

#include <botan/init.h>
#include <botan/auto_rng.h>
#include <botan/x509self.h>
#include <botan/rsa.h>
#include <botan/dsa.h>

#include <iostream>
#include <fstream>
#include <string>

#define KEY_SIZE 4096

namespace {

std::tuple<nifpp::str_atom, std::string> create_csr(const std::string &password,
    const std::string &key_path, const std::string &csr_path)
{
    using namespace Botan;

    LibraryInitializer init;
    try {
        AutoSeeded_RNG rng;
        RSA_PrivateKey priv_key(rng, KEY_SIZE);
        std::ofstream key_file(key_path);
        key_file << PKCS8::PEM_encode(priv_key, rng, password);

        X509_Cert_Options opts;

        // default values for certificate fields, which will be later
        // overwritten by Global Registry
        opts.common_name = "common name";
        opts.country = "PL";
        opts.organization = "organization";
        opts.email = "email";

        PKCS10_Request req =
            X509::create_cert_req(opts, priv_key, "SHA-256", rng);

        std::ofstream req_file(csr_path);
        req_file << req.PEM_encode();
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
        std::string key_path;
        std::string csr_path;

        nifpp::get_throws(env, argv[0], password);
        nifpp::get_throws(env, argv[1], key_path);
        nifpp::get_throws(env, argv[2], csr_path);

        return nifpp::make(env, create_csr(password, key_path, csr_path));
    }
    catch (nifpp::badarg) {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {{"create_csr", 3, create_csr_nif}};

ERL_NIF_INIT(provider_logic, nif_funcs, NULL, NULL, NULL, NULL)

} // extern C

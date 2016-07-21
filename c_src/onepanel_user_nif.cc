/*********************************************************************
*  @author Krzysztof Trzepla
*  @copyright (C): 2014 ACK CYFRONET AGH
*  This software is released under the MIT license
*  cited in 'LICENSE.txt'.
*  @end
**********************************************************************
*  @doc This is an interface for Erlang NIF library. It contains two
*  methods that allows to hash user's password and verify user's identity
*  using Botan library.
*  @end
*********************************************************************/

#include "nifpp.h"

#include <botan/auto_rng.h>
#include <botan/bcrypt.h>

#include <string>

namespace {

std::string hash_password(const std::string &password, int workFactor)
{
    Botan::AutoSeeded_RNG rng;
    return Botan::generate_bcrypt(password, rng, workFactor);
}

bool check_password(const std::string &password, const std::string &hash)
{
    if (hash.length() != 60)
        return false;

    return Botan::check_bcrypt(password, hash);
}
}

extern "C" {

static ERL_NIF_TERM hash_password_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        std::string password;
        int workFactor;

        nifpp::get_throws(env, argv[0], password);
        nifpp::get_throws(env, argv[1], workFactor);

        auto hash = hash_password(password, workFactor);

        return nifpp::make(env, hash);
    }
    catch (nifpp::badarg) {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM check_password_nif(
    ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    try {
        std::string password, hash;

        nifpp::get_throws(env, argv[0], password);
        nifpp::get_throws(env, argv[1], hash);

        auto valid = check_password(password, hash);

        return nifpp::make(env, valid);
    }
    catch (nifpp::badarg) {
        return enif_make_badarg(env);
    }
}

static ErlNifFunc nif_funcs[] = {{"hash_password", 2, hash_password_nif},
    {"check_password", 2, check_password_nif}};

ERL_NIF_INIT(onepanel_user, nif_funcs, NULL, NULL, NULL, NULL)

} // extern C

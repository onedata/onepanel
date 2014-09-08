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

#include "erl_nif.h"

#define MAX_STRING_SIZE 2048

extern char* hash_password(char* password, int work_factor);
extern int   check_password(char* password, char* hash);

ERL_NIF_TERM hash_password_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    int  work_factor;
    char password[MAX_STRING_SIZE];

    if (!enif_get_string(env, argv[0], password, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
	    return enif_make_badarg(env);
    }
    if (!enif_get_int(env, argv[1], &work_factor))
    {
        return enif_make_badarg(env);
    }

    char* hash = hash_password(password, work_factor);

    return enif_make_string(env, hash, ERL_NIF_LATIN1);
}

ERL_NIF_TERM check_password_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    char password[MAX_STRING_SIZE];
    char hash[MAX_STRING_SIZE];

    if (!enif_get_string(env, argv[0], password, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
	    return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[1], hash, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    int res = check_password(password, hash);

    return enif_make_int(env, res);
}

ErlNifFunc nif_funcs[] = {
    {"hash_password",  2, hash_password_nif},
    {"check_password", 2, check_password_nif}
};

ERL_NIF_INIT(user_logic, nif_funcs, NULL, NULL, NULL, NULL)

#include "erl_nif.h"

#define MAX_STRING_SIZE 2048

extern int create_csr(char* password, char* common_name, char* country, char* organization, char* email);

static ERL_NIF_TERM create_csr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 5)
    {
        return enif_make_badarg(env);
    }

    int ret;
    char password[MAX_STRING_SIZE];
    char common_name[MAX_STRING_SIZE];
    char country[MAX_STRING_SIZE];
    char organization[MAX_STRING_SIZE];
    char email[MAX_STRING_SIZE];

    if (!enif_get_string(env, argv[0], password, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
	    return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[1], common_name, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[2], country, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[3], organization, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[4], email, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    ret = create_csr(password, common_name, country, organization, email);

    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"create_csr", 5, create_csr_nif}
};

ERL_NIF_INIT(pkcs10, nif_funcs, NULL, NULL, NULL, NULL)

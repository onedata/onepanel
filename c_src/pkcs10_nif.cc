#include "erl_nif.h"

#define MAX_STRING_SIZE 2048

extern int create_csr(char* password, char* key_path, char* csr_path);

static ERL_NIF_TERM create_csr_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    int ret;
    char password[MAX_STRING_SIZE];
    char key_path[MAX_STRING_SIZE];
    char csr_path[MAX_STRING_SIZE];

    if (!enif_get_string(env, argv[0], password, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
	    return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[1], key_path, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }
    if (!enif_get_string(env, argv[2], csr_path, MAX_STRING_SIZE, ERL_NIF_LATIN1))
    {
        return enif_make_badarg(env);
    }

    ret = create_csr(password, key_path, csr_path);

    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"create_csr", 3, create_csr_nif}
};

ERL_NIF_INIT(pkcs10, nif_funcs, NULL, NULL, NULL, NULL)

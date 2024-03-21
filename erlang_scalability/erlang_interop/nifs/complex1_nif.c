/*
  Compile with:
  gcc -o complex1_nif.so -shared -I/Users/meraj/.asdf/installs/erlang/26.2.2/usr/include complex.c complex1_nif.c -undefined dynamic_lookup -dynamiclib
*/

#include "erl_nif.h"

extern int foo(int x);
extern int bar(int x);

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int x, ret;

    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }

    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int x, ret;

    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }

    ret = bar(x);
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"bar", 1, foo_nif}
};

ERL_NIF_INIT(complex1, nif_funcs, NULL, NULL, NULL, NULL);

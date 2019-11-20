#ifndef C_SRC_NIF_UTILS_H_
#define C_SRC_NIF_UTILS_H_

#include <stdint.h>

#include "erl_nif.h"

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* error);
ERL_NIF_TERM make_binary(ErlNifEnv* env, const uint8_t* buff, size_t length);
ERL_NIF_TERM make_badarg(ErlNifEnv* env);

#endif  // C_SRC_NIF_UTILS_H_

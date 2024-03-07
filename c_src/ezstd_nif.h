#ifndef C_SRC_EZSTD_NIF_H_
#define C_SRC_EZSTD_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomBadArg;
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomContinue;
    ERL_NIF_TERM atomInvalidData;
};

extern atoms ATOMS;

#endif  // C_SRC_EZSTD_NIF_H_


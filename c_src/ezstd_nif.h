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
    ERL_NIF_TERM atomFlush;
    ERL_NIF_TERM atomEnd;
    ERL_NIF_TERM atomSessionOnly;
    ERL_NIF_TERM atomParameters;
    ERL_NIF_TERM atomSessionAndParameters;
};

extern atoms ATOMS;

#endif  // C_SRC_EZSTD_NIF_H_


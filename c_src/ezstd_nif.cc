#include "ezstd_nif.h"
#include "nif_utils.h"
#include "macros.h"

#include <stdlib.h>
#include <memory>
#include <zstd.h>

const char kAtomError[] = "error";
const char kAtomBadArg[] = "badarg";

atoms ATOMS;

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);
    *priv_data = nullptr;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
}

static ERL_NIF_TERM zstd_nif_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ErlNifBinary bin;
    uint32_t compression_level;

    if(!enif_inspect_binary(env, argv[0], &bin) || 
       !enif_get_uint(env, argv[1], &compression_level) || 
       compression_level > static_cast<uint32_t>(ZSTD_maxCLevel()))
            return make_badarg(env);

    size_t out_buffer_size = ZSTD_compressBound(bin.size);
    std::unique_ptr<uint8_t[]> out_buffer(new uint8_t[out_buffer_size]);
  
    size_t compressed_size = ZSTD_compress(out_buffer.get(), out_buffer_size, bin.data, bin.size, compression_level);

    if(ZSTD_isError(compressed_size))
        return make_error(env, "failed to compress");

    return make_binary(env, out_buffer.get(), compressed_size);
}

static ERL_NIF_TERM zstd_nif_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    UNUSED(argc);

    ErlNifBinary bin;
    
    if(!enif_inspect_binary(env, argv[0], &bin))
        return make_badarg(env);

    uint64_t uncompressed_size = ZSTD_getDecompressedSize(bin.data, bin.size);

    ERL_NIF_TERM out_term;
    uint8_t *destination_buffer = enif_make_new_binary(env, uncompressed_size, &out_term);

    if(ZSTD_decompress(destination_buffer, uncompressed_size, bin.data, bin.size) != uncompressed_size)
        return make_error(env, "failed to decompress");

    return out_term;
}

static ErlNifFunc nif_funcs[] = {
    {"compress", 2, zstd_nif_compress},
    {"decompress", 1, zstd_nif_decompress}
};

ERL_NIF_INIT(ezstd_nif, nif_funcs, on_nif_load, NULL, NULL, on_nif_unload);

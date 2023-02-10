#include "ezstd_nif.h"
#include "nif_utils.h"
#include "macros.h"

#include <stdlib.h>
#include <memory>
#include <zstd.h>

const char kAtomError[] = "error";
const char kAtomBadArg[] = "badarg";

atoms ATOMS;

ErlNifResourceType *COMPRESS_DICTIONARY_RES_TYPE;
ErlNifResourceType *DECOMPRESS_DICTIONARY_RES_TYPE;

struct ZSTDCCtxDeleter {
  void operator()(ZSTD_CCtx* ctx) {
    ZSTD_freeCCtx(ctx);
  }
};

struct ZSTDDCtxDeleter {
  void operator()(ZSTD_DCtx* ctx) {
    ZSTD_freeDCtx(ctx);
  }
};

void zstd_nif_compress_dictionary_destructor(ErlNifEnv *env, void *res) {
  UNUSED(env);
  ZSTD_CDict** dict_resource = static_cast<ZSTD_CDict**>(res);
  ZSTD_freeCDict(*dict_resource);  
}

void zstd_nif_decompress_dictionary_destructor(ErlNifEnv *env, void *res) {
  UNUSED(env);
  ZSTD_DDict** dict_resource = static_cast<ZSTD_DDict**>(res);
  ZSTD_freeDDict(*dict_resource);
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);
    *priv_data = nullptr;

    ErlNifResourceFlags flags = ErlNifResourceFlags(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    COMPRESS_DICTIONARY_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.CompressDictionary", zstd_nif_compress_dictionary_destructor, flags, nullptr);

    DECOMPRESS_DICTIONARY_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.DecompressDictionary", zstd_nif_decompress_dictionary_destructor, flags, nullptr);

    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
}

static ERL_NIF_TERM zstd_nif_get_dict_id_from_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZSTD_CDict** dict_resource;

    if(!enif_get_resource(env, argv[0], COMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }

    unsigned result = ZSTD_getDictID_fromCDict(*dict_resource);
    return enif_make_uint(env, result);
}

static ERL_NIF_TERM zstd_nif_get_dict_id_from_ddict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZSTD_DDict** dict_resource;

    if(!enif_get_resource(env, argv[0], DECOMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }

    unsigned result = ZSTD_getDictID_fromDDict(*dict_resource);
    return enif_make_uint(env, result);
} 

static ERL_NIF_TERM zstd_nif_compress_using_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ErlNifBinary bin;
    ZSTD_CDict** dict_resource; 

    if(!enif_inspect_binary(env, argv[0], &bin) ||
       !enif_get_resource(env, argv[1], COMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }

    size_t out_buffer_size = ZSTD_compressBound(bin.size);
    std::unique_ptr<uint8_t[]> out_buffer(new uint8_t[out_buffer_size]);

    std::unique_ptr<ZSTD_CCtx, ZSTDCCtxDeleter> ctx {ZSTD_createCCtx()};

    if (!ctx) {
      return make_error(env, "failed to alloc");
    }

    size_t compressed_size = ZSTD_compress_usingCDict(ctx.get(), out_buffer.get(), out_buffer_size, bin.data, bin.size, *dict_resource);

    if(ZSTD_isError(compressed_size)) {
        return make_error(env, "failed to compress");
    }

    return make_binary(env, out_buffer.get(), compressed_size);
}

static ERL_NIF_TERM zstd_nif_decompress_using_ddict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ErlNifBinary bin;
    ZSTD_DDict** dict_resource;

    if(!enif_inspect_binary(env, argv[0], &bin) ||
       !enif_get_resource(env, argv[1], DECOMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }


    std::unique_ptr<ZSTD_DCtx, ZSTDDCtxDeleter> ctx {ZSTD_createDCtx()};

    if (!ctx) {
      return make_error(env, "failed to alloc");
    }

    uint64_t uncompressed_size = ZSTD_getFrameContentSize(bin.data, bin.size);

    ERL_NIF_TERM out_term;
    uint8_t *destination_buffer = enif_make_new_binary(env, uncompressed_size, &out_term);

    size_t actual_decompressed_size = ZSTD_decompress_usingDDict(ctx.get(), destination_buffer, uncompressed_size, bin.data, bin.size, *dict_resource);

    if (actual_decompressed_size != uncompressed_size) {
        return make_error(env, "failed to decompress");
    }

    return out_term;
}

static ERL_NIF_TERM zstd_nif_create_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    UNUSED(argc);

    ErlNifBinary bin;
    uint32_t compression_level;
   

    if(!enif_inspect_binary(env, argv[0], &bin) ||
       !enif_get_uint(env, argv[1], &compression_level) ||
       compression_level > static_cast<uint32_t>(ZSTD_maxCLevel())) {
            return make_badarg(env);
    }


    ZSTD_CDict* dict = ZSTD_createCDict(bin.data, bin.size, compression_level);

    if (dict == nullptr) {
      return make_error(env, "failed to create cdict");
    }

    /* enif_alloc_resource cannot fail: https://github.com/erlang/otp/blob/df484d244705180def80fae22cba747d3e5bfdb1/erts/emulator/beam/erl_nif.c#L3029 */
    ZSTD_CDict** resource = static_cast<ZSTD_CDict**>(enif_alloc_resource(COMPRESS_DICTIONARY_RES_TYPE, sizeof(ZSTD_CDict*)));

    *resource = dict;

    ERL_NIF_TERM result = enif_make_resource(env, resource);

    enif_release_resource(resource);
    return result;
}

static ERL_NIF_TERM zstd_nif_create_ddict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    UNUSED(argc);

    ErlNifBinary bin;


    if(!enif_inspect_binary(env, argv[0], &bin)) {
            return make_badarg(env);
    }


    ZSTD_DDict* dict = ZSTD_createDDict(bin.data, bin.size);

    if (dict == nullptr) {
      return make_error(env, "failed to create cdict");
    }

    /* enif_alloc_resource cannot fail */
    ZSTD_DDict** resource = static_cast<ZSTD_DDict**>(enif_alloc_resource(DECOMPRESS_DICTIONARY_RES_TYPE, sizeof(ZSTD_DDict*)));

    *resource = dict;

    ERL_NIF_TERM result = enif_make_resource(env, resource);

    enif_release_resource(resource);
    return result;
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

static ERL_NIF_TERM zstd_nif_get_dict_id_from_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

  
    ErlNifBinary bin;

    if(!enif_inspect_binary(env, argv[0], &bin)) {
            return make_badarg(env);
    }

    unsigned result = ZSTD_getDictID_fromFrame(bin.data, bin.size);
    return enif_make_uint(env, result);
}

static ERL_NIF_TERM zstd_nif_decompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) 
{
    UNUSED(argc);

    ErlNifBinary bin;
    
    if(!enif_inspect_binary(env, argv[0], &bin))
        return make_badarg(env);

    uint64_t uncompressed_size = ZSTD_getFrameContentSize(bin.data, bin.size);

    ERL_NIF_TERM out_term;
    uint8_t *destination_buffer = enif_make_new_binary(env, uncompressed_size, &out_term);

    if(ZSTD_decompress(destination_buffer, uncompressed_size, bin.data, bin.size) != uncompressed_size)
        return make_error(env, "failed to decompress");

    return out_term;
}

static ErlNifFunc nif_funcs[] = {
    {"compress", 2, zstd_nif_compress},
    {"decompress", 1, zstd_nif_decompress},
    {"create_cdict", 2, zstd_nif_create_cdict},
    {"create_ddict", 1, zstd_nif_create_ddict},
    {"get_dict_id_from_ddict", 1, zstd_nif_get_dict_id_from_ddict},
    {"get_dict_id_from_cdict", 1, zstd_nif_get_dict_id_from_cdict},
    {"get_dict_id_from_frame", 1, zstd_nif_get_dict_id_from_frame},
    {"compress_using_cdict", 2, zstd_nif_compress_using_cdict},
    {"decompress_using_ddict", 2, zstd_nif_decompress_using_ddict},
};

ERL_NIF_INIT(ezstd_nif, nif_funcs, on_nif_load, NULL, NULL, on_nif_unload);

#include "ezstd_nif.h"
#include "nif_utils.h"
#include "macros.h"

#include <stdlib.h>
#include <memory>
#include <string.h>

#define ZSTD_STATIC_LINKING_ONLY
#include <zstd.h>

#define MAX_BUFFER_SIZE (1<<30)

const char kAtomError[] = "error";
const char kAtomBadArg[] = "badarg";
const char kAtomOk[] = "ok";
const char kAtomContinue[] = "continue";
const char kAtomFlush[] = "flush";
const char kAtomEnd[] = "zstdend";
const char kAtomSessionOnly[] = "session_only";
const char kAtomParameters[] = "parameters";
const char kAtomSessionAndParameters[] = "session_and_parameters";

atoms ATOMS;

ErlNifResourceType *COMPRESS_DICTIONARY_RES_TYPE;
ErlNifResourceType *DECOMPRESS_DICTIONARY_RES_TYPE;
ErlNifResourceType *COMPRESS_CONTEXT_RES_TYPE;
ErlNifResourceType *DECOMPRESS_CONTEXT_RES_TYPE;

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

struct ZstdCCtxWithBuffer {
    ZSTD_CCtx* cctx;
    ZSTD_outBuffer out;
    ZSTD_CDict** dict;
};

struct ZstdDCtxWithBuffer {
    ZSTD_DCtx* dctx;
    ZSTD_outBuffer out;
    ZSTD_DDict** dict;
};

void zstd_nif_compress_dictionary_destructor(ErlNifEnv *env, void *res)
{
    UNUSED(env);
    ZSTD_CDict** dict_resource = static_cast<ZSTD_CDict**>(res);
    ZSTD_freeCDict(*dict_resource);
}

void zstd_nif_decompress_dictionary_destructor(ErlNifEnv *env, void *res)
{
    UNUSED(env);
    ZSTD_DDict** dict_resource = static_cast<ZSTD_DDict**>(res);
    ZSTD_freeDDict(*dict_resource);
}

void zstd_nif_compression_context_destructor(ErlNifEnv *env, void *res)
{
    UNUSED(env);
    ZstdCCtxWithBuffer* ctx_resource = static_cast<ZstdCCtxWithBuffer*>(res);
    ZSTD_freeCCtx(ctx_resource->cctx);

    if (ctx_resource->dict != nullptr)
        enif_release_resource(ctx_resource->dict);

    enif_free(ctx_resource->out.dst);
}

void zstd_nif_decompression_context_destructor(ErlNifEnv *env, void *res)
{
    UNUSED(env);
    ZstdDCtxWithBuffer* ctx_resource = static_cast<ZstdDCtxWithBuffer*>(res);
    ZSTD_freeDCtx(ctx_resource->dctx);

    if (ctx_resource->dict != nullptr)
        enif_release_resource(ctx_resource->dict);

    enif_free(ctx_resource->out.dst);
}

void* zstd_nif_malloc(void *unused, size_t size)
{
    UNUSED(unused);
    return enif_alloc(size);
}

void zstd_nif_free(void *unused, void *address)
{
    UNUSED(unused);
    enif_free(address);
}

static ZSTD_customMem get_enif_zstd_allocator()
{
    return ZSTD_customMem({zstd_nif_malloc, zstd_nif_free, nullptr});
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomBadArg = make_atom(env, kAtomBadArg);
    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomContinue = make_atom(env, kAtomContinue);
    ATOMS.atomFlush = make_atom(env, kAtomFlush);
    ATOMS.atomEnd = make_atom(env, kAtomEnd);
    ATOMS.atomSessionOnly = make_atom(env, kAtomSessionOnly);
    ATOMS.atomParameters = make_atom(env, kAtomParameters);
    ATOMS.atomSessionAndParameters = make_atom(env, kAtomSessionAndParameters);
    *priv_data = nullptr;

    ErlNifResourceFlags flags = ErlNifResourceFlags(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    COMPRESS_DICTIONARY_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.CompressDictionary", zstd_nif_compress_dictionary_destructor, flags, nullptr);

    DECOMPRESS_DICTIONARY_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.DecompressDictionary", zstd_nif_decompress_dictionary_destructor, flags, nullptr);

    COMPRESS_CONTEXT_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.CompressionContext", zstd_nif_compression_context_destructor, flags, nullptr);
    DECOMPRESS_CONTEXT_RES_TYPE = enif_open_resource_type(env, nullptr, "ZStandard.DecompressionContext", zstd_nif_decompression_context_destructor, flags, nullptr);

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

    if(!enif_get_resource(env, argv[0], COMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource)))
        return make_badarg(env);

    unsigned result = ZSTD_getDictID_fromCDict(*dict_resource);
    return enif_make_uint(env, result);
}

static ERL_NIF_TERM zstd_nif_get_dict_id_from_ddict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZSTD_DDict** dict_resource;

    if(!enif_get_resource(env, argv[0], DECOMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource)))
        return make_badarg(env);

    unsigned result = ZSTD_getDictID_fromDDict(*dict_resource);
    return enif_make_uint(env, result);
}

static ERL_NIF_TERM zstd_nif_compress_using_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ErlNifBinary bin;
    ZSTD_CDict** dict_resource;

    if(!enif_inspect_binary(env, argv[0], &bin) || !enif_get_resource(env, argv[1], COMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource)))
        return make_badarg(env);

    size_t out_buffer_size = ZSTD_compressBound(bin.size);
    std::unique_ptr<uint8_t[]> out_buffer(new uint8_t[out_buffer_size]);

    std::unique_ptr<ZSTD_CCtx, ZSTDCCtxDeleter> ctx {ZSTD_createCCtx_advanced(get_enif_zstd_allocator())};

    if (!ctx)
      return make_error(env, "failed to alloc");

    size_t compressed_size = ZSTD_compress_usingCDict(ctx.get(), out_buffer.get(), out_buffer_size, bin.data, bin.size, *dict_resource);

    if(ZSTD_isError(compressed_size))
        return make_error(env, "failed to compress");

    return make_binary(env, out_buffer.get(), compressed_size);
}

static ERL_NIF_TERM zstd_nif_create_compression_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    unsigned int out_buffer_size;

    if (!enif_get_uint(env, argv[0], &out_buffer_size))
        return make_badarg(env);

    if (out_buffer_size > MAX_BUFFER_SIZE)
        return make_badarg(env);

    ZSTD_CCtx* context = ZSTD_createCCtx_advanced(get_enif_zstd_allocator());

    if (!context)
        return make_error(env, "unable to create context");

    void* buffer = enif_alloc(out_buffer_size);
    if (!buffer)
    {
        ZSTD_freeCCtx(context);
        return make_error(env, "unable to create buffer");
    }

    ZstdCCtxWithBuffer* resource = static_cast<ZstdCCtxWithBuffer*>(enif_alloc_resource(COMPRESS_CONTEXT_RES_TYPE, sizeof(ZstdCCtxWithBuffer)));
    resource->cctx = context;
    resource->out.dst = buffer;
    resource->out.pos = 0;
    resource->out.size = out_buffer_size;
    resource->dict = nullptr;

    ERL_NIF_TERM result = enif_make_resource(env, resource);

    enif_release_resource(resource);
    return result;
}

static ERL_NIF_TERM zstd_nif_create_decompression_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    unsigned int out_buffer_size;

    if (!enif_get_uint(env, argv[0], &out_buffer_size))
        return make_badarg(env);

    if (out_buffer_size > MAX_BUFFER_SIZE)
        return make_badarg(env);

    ZSTD_DCtx* context = ZSTD_createDCtx_advanced(get_enif_zstd_allocator());
    if (!context)
        return make_error(env, "unable to create context");

    void* buffer = enif_alloc(out_buffer_size);
    if (!buffer)
    {
        ZSTD_freeDCtx(context);
        return make_error(env, "unable to create buffer");
    }

    ZstdDCtxWithBuffer* resource = static_cast<ZstdDCtxWithBuffer*>(enif_alloc_resource(DECOMPRESS_CONTEXT_RES_TYPE, sizeof(ZstdDCtxWithBuffer)));
    resource->dctx = context;
    resource->out.dst = buffer;
    resource->out.pos = 0;
    resource->out.size = out_buffer_size;
    resource->dict = nullptr;

    ERL_NIF_TERM result = enif_make_resource(env, resource);

    enif_release_resource(resource);
    return result;
}

static ERL_NIF_TERM zstd_nif_reset_compression_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZstdCCtxWithBuffer* ctx_resource;

    if(!enif_get_resource(env, argv[0], COMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)))
        return make_badarg(env);

    size_t result = 0;

    if(enif_is_identical(argv[1], ATOMS.atomSessionOnly))
        result = ZSTD_CCtx_reset(ctx_resource->cctx, ZSTD_reset_session_only);
    else if(enif_is_identical(argv[1], ATOMS.atomParameters))
        result = ZSTD_CCtx_reset(ctx_resource->cctx, ZSTD_reset_parameters);
    else if(enif_is_identical(argv[1], ATOMS.atomSessionAndParameters))
        result = ZSTD_CCtx_reset(ctx_resource->cctx, ZSTD_reset_session_and_parameters);
    else
        return make_badarg(env);

    if (ZSTD_isError(result))
        return make_error(env, "failed to reset context");

    return ATOMS.atomOk;
}

static ERL_NIF_TERM zstd_nif_reset_decompression_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZstdDCtxWithBuffer* ctx_resource;

    if(!enif_get_resource(env, argv[0], DECOMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)))
        return make_badarg(env);

    size_t result = 0;

    if(enif_is_identical(argv[1], ATOMS.atomSessionOnly))
        result = ZSTD_DCtx_reset(ctx_resource->dctx, ZSTD_reset_session_only);
    else if(enif_is_identical(argv[1], ATOMS.atomParameters))
        result = ZSTD_DCtx_reset(ctx_resource->dctx, ZSTD_reset_parameters);
    else if(enif_is_identical(argv[1], ATOMS.atomSessionAndParameters))
        result = ZSTD_DCtx_reset(ctx_resource->dctx, ZSTD_reset_session_and_parameters);
    else
        return make_badarg(env);

    if (ZSTD_isError(result))
        return make_error(env, "failed to reset context");

    return ATOMS.atomOk;
}

static ERL_NIF_TERM zstd_nif_select_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZstdCCtxWithBuffer* ctx_resource;
    ZSTD_CDict** dict_resource;

    if(!enif_get_resource(env, argv[0], COMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
        !enif_get_resource(env, argv[1], COMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }

    size_t result = ZSTD_CCtx_refCDict(ctx_resource->cctx, *dict_resource);
    if (ctx_resource->dict != nullptr)
    {
        // refCDict replaces the dictionary for this context, so we can't use the old one.
        enif_release_resource(ctx_resource->dict);
    }

    ctx_resource->dict = dict_resource;
    enif_keep_resource(dict_resource);

    if (ZSTD_isError(result))
        return make_error(env, "failed to set dictionary");

    return ATOMS.atomOk;
}

static ERL_NIF_TERM zstd_nif_select_ddict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    ZstdDCtxWithBuffer* ctx_resource;
    ZSTD_DDict** dict_resource;

    if(!enif_get_resource(env, argv[0], DECOMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
        !enif_get_resource(env, argv[1], DECOMPRESS_DICTIONARY_RES_TYPE, reinterpret_cast<void**>(&dict_resource))) {
            return make_badarg(env);
    }

    size_t result = ZSTD_DCtx_refDDict(ctx_resource->dctx, *dict_resource);
    if (ctx_resource->dict != nullptr)
    {
        // We do not support ZSTD_d_refMultipleDDicts so setting a new one means we no longer could want
        // to use the old dictionary.
        enif_release_resource(ctx_resource->dict);
    }

    ctx_resource->dict = dict_resource;
    enif_keep_resource(dict_resource);

    if (ZSTD_isError(result))
        return make_error(env, "failed to set dictionary");

    return ATOMS.atomOk;
}

static ERL_NIF_TERM zstd_nif_set_compression_parameter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ZstdCCtxWithBuffer* ctx_resource;
    ZSTD_cParameter param_id;
    int value;

    if(!enif_get_resource(env, argv[0], COMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
       !enif_get_int(env, argv[1], reinterpret_cast<int*>(&param_id)) ||
       !enif_get_int(env, argv[2], &value)) {
            return make_badarg(env);
    }

    size_t result = ZSTD_CCtx_setParameter(ctx_resource->cctx, param_id, value);

    if (ZSTD_isError(result))
        return make_badarg(env);

    return ATOMS.atomOk;
}

static ERL_NIF_TERM zstd_nif_set_decompression_parameter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ZstdDCtxWithBuffer* ctx_resource;
    ZSTD_dParameter param_id;
    int value;

    if(!enif_get_resource(env, argv[0], DECOMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
       !enif_get_int(env, argv[1], reinterpret_cast<int*>(&param_id)) ||
       !enif_get_int(env, argv[2], &value)) {
            return make_badarg(env);
    }

    size_t result = ZSTD_DCtx_setParameter(ctx_resource->dctx, param_id, value);
    if (ZSTD_isError(result))
        return make_badarg(env);

    return ATOMS.atomOk;
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

    std::unique_ptr<ZSTD_DCtx, ZSTDDCtxDeleter> ctx {ZSTD_createDCtx_advanced(get_enif_zstd_allocator())};

    if (!ctx)
        return make_error(env, "failed to alloc");

    uint64_t uncompressed_size = ZSTD_getFrameContentSize(bin.data, bin.size);

    if (uncompressed_size == ZSTD_CONTENTSIZE_UNKNOWN)
        return make_error(env, "failed to decompress: ZSTD_CONTENTSIZE_UNKNOWN");

    if (uncompressed_size == ZSTD_CONTENTSIZE_ERROR)
        return make_error(env, "failed to decompress: ZSTD_CONTENTSIZE_ERROR");

    ERL_NIF_TERM out_term;
    uint8_t *destination_buffer = enif_make_new_binary(env, uncompressed_size, &out_term);

    size_t actual_decompressed_size = ZSTD_decompress_usingDDict(ctx.get(), destination_buffer, uncompressed_size, bin.data, bin.size, *dict_resource);

    if (actual_decompressed_size != uncompressed_size)
        return make_error(env, "failed to decompress");

    return out_term;
}

static ERL_NIF_TERM zstd_nif_create_cdict(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    UNUSED(argc);

    ErlNifBinary bin;
    uint32_t compression_level;

    if(!enif_inspect_binary(env, argv[0], &bin) || !enif_get_uint(env, argv[1], &compression_level) || compression_level > static_cast<uint32_t>(ZSTD_maxCLevel()))
        return make_badarg(env);

    ZSTD_CDict* dict = ZSTD_createCDict(bin.data, bin.size, compression_level);

    if (dict == nullptr)
        return make_error(env, "failed to create cdict");

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

    if(!enif_inspect_binary(env, argv[0], &bin))
        return make_badarg(env);

    ZSTD_DDict* dict = ZSTD_createDDict(bin.data, bin.size);

    if (dict == nullptr)
        return make_error(env, "failed to create cdict");

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

    if(!enif_inspect_binary(env, argv[0], &bin) || !enif_get_uint(env, argv[1], &compression_level) || compression_level > static_cast<uint32_t>(ZSTD_maxCLevel()))
        return make_badarg(env);

    size_t out_buffer_size = ZSTD_compressBound(bin.size);
    std::unique_ptr<uint8_t[]> out_buffer(new uint8_t[out_buffer_size]);

    size_t compressed_size = ZSTD_compress(out_buffer.get(), out_buffer_size, bin.data, bin.size, compression_level);

    if(ZSTD_isError(compressed_size))
        return make_error(env, "failed to compress");

    return make_binary(env, out_buffer.get(), compressed_size);
}

static ERL_NIF_TERM zstd_nif_compress_streaming_chunk(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ZstdCCtxWithBuffer* ctx_resource;
    ErlNifBinary bin;
    unsigned long offset;

    if (!enif_get_resource(env, argv[0], COMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
        !enif_inspect_binary(env, argv[1], &bin) ||
        !enif_get_ulong(env, argv[3], &offset)) {
            return make_badarg(env);
        }

    if (offset > SIZE_MAX)
        return make_badarg(env);

    ZSTD_EndDirective flush_type;

    if (enif_is_identical(argv[2], ATOMS.atomFlush))
        flush_type = ZSTD_e_flush;
    else if (enif_is_identical(argv[2], ATOMS.atomEnd))
        flush_type = ZSTD_e_end;
    else
        return make_badarg(env);

    ZSTD_inBuffer in_buffer;
    in_buffer.src = bin.data;
    in_buffer.size = bin.size;
    in_buffer.pos = offset;
    ctx_resource->out.pos = 0;

    size_t result = ZSTD_compressStream2(ctx_resource->cctx, &ctx_resource->out, &in_buffer, flush_type);

    ERL_NIF_TERM result_chunk;
    unsigned char* result_buffer = enif_make_new_binary(env, ctx_resource->out.pos, &result_chunk);
    memcpy(result_buffer, ctx_resource->out.dst, ctx_resource->out.pos);

    bool made_forward_progress = in_buffer.pos > offset || ctx_resource->out.pos > 0;
    bool fully_processed_input = in_buffer.pos == in_buffer.size;

    if (result == 0 || (!made_forward_progress && fully_processed_input))
    {
        return enif_make_tuple2(env, ATOMS.atomOk, result_chunk);
    }
    else if (result > 0)
    {
        if (!fully_processed_input && !made_forward_progress)
            return make_error(env, "compressor stuck");

        ERL_NIF_TERM new_offset = enif_make_uint(env, in_buffer.pos);
        return enif_make_tuple3(env, ATOMS.atomContinue, result_chunk, new_offset);
    }
    else
    {
        return make_badarg(env);
    }
}

static ERL_NIF_TERM zstd_nif_decompress_streaming_chunk(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ZstdDCtxWithBuffer* ctx_resource;
    ErlNifBinary bin;
    unsigned long offset;

    if (!enif_get_resource(env, argv[0], DECOMPRESS_CONTEXT_RES_TYPE, reinterpret_cast<void**>(&ctx_resource)) ||
        !enif_inspect_binary(env, argv[1], &bin) ||
        !enif_get_ulong(env, argv[2], &offset)) {
            return make_badarg(env);
        }

    if (offset > SIZE_MAX)
        return make_badarg(env);

    ZSTD_inBuffer in_buffer;
    in_buffer.src = bin.data;
    in_buffer.size = bin.size;
    in_buffer.pos = offset;
    ctx_resource->out.pos = 0;

    size_t result = ZSTD_decompressStream(ctx_resource->dctx, &ctx_resource->out, &in_buffer);

    ERL_NIF_TERM result_chunk;
    unsigned char* result_buffer = enif_make_new_binary(env, ctx_resource->out.pos, &result_chunk);
    memcpy(result_buffer, ctx_resource->out.dst, ctx_resource->out.pos);

    bool made_forward_progress = in_buffer.pos > offset || ctx_resource->out.pos > 0;
    bool fully_processed_input = in_buffer.pos == in_buffer.size;

    if (result == 0 || (!made_forward_progress && fully_processed_input))
    {
        return enif_make_tuple2(env, ATOMS.atomOk, result_chunk);
    }
    else if (result > 0)
    {
        if (!fully_processed_input && !made_forward_progress)
            return make_error(env, "corrupted data");

        ERL_NIF_TERM new_offset = enif_make_uint(env, in_buffer.pos);
        return enif_make_tuple3(env, ATOMS.atomContinue, result_chunk, new_offset);
    }
    else
    {
        return make_badarg(env);
    }
}

static ERL_NIF_TERM zstd_nif_get_dict_id_from_frame(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    ErlNifBinary bin;

    if(!enif_inspect_binary(env, argv[0], &bin))
        return make_badarg(env);

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

    if (uncompressed_size == ZSTD_CONTENTSIZE_UNKNOWN)
        return make_error(env, "failed to decompress: ZSTD_CONTENTSIZE_UNKNOWN");

    if (uncompressed_size == ZSTD_CONTENTSIZE_ERROR)
        return make_error(env, "failed to decompress: ZSTD_CONTENTSIZE_ERROR");

    ERL_NIF_TERM out_term;
    uint8_t *destination_buffer = enif_make_new_binary(env, uncompressed_size, &out_term);

    if (ZSTD_decompress(destination_buffer, uncompressed_size, bin.data, bin.size) != uncompressed_size)
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
    {"create_compression_context", 1, zstd_nif_create_compression_context},
    {"create_decompression_context", 1, zstd_nif_create_decompression_context},
    {"select_cdict", 2, zstd_nif_select_cdict},
    {"select_ddict", 2, zstd_nif_select_ddict},
    {"set_compression_parameter", 3, zstd_nif_set_compression_parameter},
    {"set_decompression_parameter", 3, zstd_nif_set_decompression_parameter},
    {"compress_streaming_chunk", 4, zstd_nif_compress_streaming_chunk},
    {"decompress_streaming_chunk", 3, zstd_nif_decompress_streaming_chunk},
    {"reset_compression_context", 2, zstd_nif_reset_compression_context},
    {"reset_decompression_context", 2, zstd_nif_reset_decompression_context}
};

ERL_NIF_INIT(ezstd_nif, nif_funcs, on_nif_load, NULL, NULL, on_nif_unload);

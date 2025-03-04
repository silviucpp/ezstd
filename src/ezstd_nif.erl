%% @private
-module(ezstd_nif).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    compress/2,
    decompress/1,
    create_cdict/2,
    create_ddict/1,
    compress_using_cdict/2,
    decompress_using_ddict/2,
    get_dict_id_from_cdict/1,
    get_dict_id_from_ddict/1,
    get_dict_id_from_frame/1,
    create_compression_context/1,
    select_cdict/2,
    set_compression_parameter/3,
    compress_streaming_chunk/4,
    create_decompression_context/1,
    select_ddict/2,
    set_decompression_parameter/3,
    decompress_streaming_chunk/3,
    reset_compression_context/2,
    reset_decompression_context/2
]).

%% nif functions

load_nif() ->
    ok = erlang:load_nif(get_priv_path(?MODULE), 0).

get_priv_path(File) ->
    case code:priv_dir(ezstd) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join([filename:dirname(Ebin), "priv", File]);
        Dir ->
            filename:join(Dir, File)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

compress(_Binary, _CompressionLevel) ->
    ?NOT_LOADED.

decompress(_Binary) ->
    ?NOT_LOADED.

create_cdict(_Binary, _CompressionLevel) ->
    ?NOT_LOADED.

create_ddict(_Binary) ->
    ?NOT_LOADED.

compress_using_cdict(_Binary, _CCDict) ->
    ?NOT_LOADED.

decompress_using_ddict(_Binary, _DDict) ->
    ?NOT_LOADED.

get_dict_id_from_cdict(_CDict) ->
    ?NOT_LOADED.

get_dict_id_from_ddict(_DDict) ->
    ?NOT_LOADED.

get_dict_id_from_frame(_Binary) ->
    ?NOT_LOADED.

create_compression_context(_BufferSize) ->
    ?NOT_LOADED.

select_cdict(_CCtx, _CDict) ->
    ?NOT_LOADED.

set_compression_parameter(_CCtx, _Param, _Value) ->
    ?NOT_LOADED.

compress_streaming_chunk(_CCtx, _Binary, _Offset, _FlushType) ->
    ?NOT_LOADED.

create_decompression_context(_BufferSize) ->
    ?NOT_LOADED.

select_ddict(_DCtx, _Ddict) ->
    ?NOT_LOADED.

set_decompression_parameter(_CCtx, _Param, _Value) ->
    ?NOT_LOADED.

decompress_streaming_chunk(_DCtx, _Binary, _Offset) ->
    ?NOT_LOADED.

reset_compression_context(_CCtx, _ResetDirective) ->
    ?NOT_LOADED.

reset_decompression_context(_DCtx, _ResetDirective) ->
    ?NOT_LOADED.

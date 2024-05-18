%% @doc
%% Zstd [1] binding for Erlang.
%%
%% [1]: [http://facebook.github.io/zstd]
-module(ezstd).

-export([
    compress/1,
    compress/2,
    decompress/1,
    create_cdict/2,
    create_ddict/1,
    compress_using_cdict/2,
    decompress_using_ddict/2,
    get_dict_id_from_frame/1,
    get_dict_id_from_ddict/1,
    get_dict_id_from_cdict/1,
    create_compression_context/1,
    select_cdict/2,
    set_compression_parameter/3,
    compress_streaming/2,
    compress_streaming_end/2,
    create_decompression_context/1,
    set_decompression_parameter/3,
    select_ddict/2,
    decompress_streaming/2,
    reset_compression_context/2,
    reset_decompression_context/2
]).

-type zstd_compression_flag() :: 'zstd_c_compression_level'
      | 'zstd_c_window_log'
      | 'zstd_c_hash_log'
      | 'zstd_c_chain_log'
      | 'zstd_c_search_log'
      | 'zstd_c_min_match'
      | 'zstd_c_target_length'
      | 'zstd_c_strategy'.

-type zstd_decompression_flag() :: 'zstd_d_window_log_max'.

-type zstd_reset_directive() :: 'session_only' | 'parameters' | 'session_and_parameters'.

-spec create_cdict(binary(), integer()) -> reference() | {error, any()}.
create_cdict(Binary, CompressionLevel) ->
    ezstd_nif:create_cdict(Binary, CompressionLevel).

-spec create_ddict(binary()) -> reference() | {error, any()}.
create_ddict(Binary) ->
    ezstd_nif:create_ddict(Binary).

-spec compress_using_cdict(binary(), reference()) -> binary() | {error, any()}.
compress_using_cdict(Binary, CCDict) ->
    ezstd_nif:compress_using_cdict(Binary, CCDict).

-spec decompress_using_ddict(binary(), reference()) -> binary() | {error, any()}.
decompress_using_ddict(Binary, DDict) ->
    ezstd_nif:decompress_using_ddict(Binary, DDict).

-spec get_dict_id_from_frame(binary()) -> integer().
get_dict_id_from_frame(Binary) ->
    returns_integers(ezstd_nif:get_dict_id_from_frame(Binary)).

-spec get_dict_id_from_ddict(reference()) -> integer().
get_dict_id_from_ddict(DDict) ->
    returns_integers(ezstd_nif:get_dict_id_from_ddict(DDict)).

-spec get_dict_id_from_cdict(reference()) -> integer().
get_dict_id_from_cdict(CDict) ->
    returns_integers(ezstd_nif:get_dict_id_from_cdict(CDict)).

%% @doc Compresses the given binary.
-spec compress(binary()) -> binary() | {error, any()}.
compress(Binary) ->
    ezstd_nif:compress(Binary, 1).

%% @doc Compresses the given binary with compression level.
-spec compress(binary(), integer()) -> binary() | {error, any()}.
compress(Binary, CompressionLevel) ->
    ezstd_nif:compress(Binary, CompressionLevel).

%% @doc Decompresses the given binary.
-spec decompress(binary()) -> binary() | {error, any()}.
decompress(Binary) ->
    ezstd_nif:decompress(Binary).

%% @doc Create a streaming compression context, with a buffer of the given size.
-spec create_compression_context(pos_integer()) -> reference() | {error, any()}.
create_compression_context(BufferSize) ->
    ezstd_nif:create_compression_context(BufferSize).

%% @doc Set a dictionary for the given streaming compression context. Must be called
%% before beginning compression.
-spec select_cdict(reference(), reference()) -> ok | {error, any()}.
select_cdict(Context, CDict) ->
    ezstd_nif:select_cdict(Context, CDict).

%% @doc Set a compression parameter on the given compression context. Valid values for
%% flag are zstd_c_compression_level and zstd_c_window_log.
- spec set_compression_parameter(reference(), zstd_compression_flag(), integer()) -> ok | {error, any()}.
set_compression_parameter(Context, Flag, Value) ->
    case flag_to_compression_param_number(Flag) of
        {ok, Param} ->
            ezstd_nif:set_compression_parameter(Context, Param, Value);
        error ->
            error
    end.

%% @doc Compress some data without closing out the compression frame. This
%% is intended to be used by a streaming decompressor which receives the same
%% data in the same order.
-spec compress_streaming(reference(), binary()) -> iolist() | {error, any()}.
compress_streaming(Context, Binary) ->
    compress_streaming_chunk(Context, Binary, flush, 0, [], 1000).

-spec compress_streaming_end(reference(), binary()) -> iolist() | {error, any()}.
compress_streaming_end(Context, Binary) ->
    compress_streaming_chunk(Context, Binary, zstdend, 0, [], 1000).

compress_streaming_chunk(_Context, _Binary, _FlushType, _Offset, _Sofar, 0) ->
    {error, compressor_stuck};

compress_streaming_chunk(Context, Binary, FlushType, Offset, Sofar, Attempts) ->
    case ezstd_nif:compress_streaming_chunk(Context, Binary, FlushType, Offset) of
        {ok, Chunk} ->
            [Sofar | Chunk];
        {continue, Chunk, NextOffset} ->
            compress_streaming_chunk(Context, Binary, FlushType, NextOffset, [Sofar | Chunk], Attempts - 1);
        Error ->
            Error
    end.

%% @doc Create a streaming decompression context, with a buffer of the given size.
-spec create_decompression_context(pos_integer()) -> reference() | {error, any()}.
create_decompression_context(BufferSize) ->
    ezstd_nif:create_decompression_context(BufferSize).

%% @doc Set a dictionary for the given streaming decompression context. Must be called
%% before beginning decompression.
-spec select_ddict(reference(), reference()) -> ok | {error, any()}.
select_ddict(Context, DDict) ->
    ezstd_nif:select_ddict(Context, DDict).

%% @doc Set a decompression parameter on the given compression context. The only valid
%% value for Flag is zstd_d_window_log_max.
- spec set_decompression_parameter(reference(), zstd_decompression_flag(), integer()) -> ok | {error, any()}.
set_decompression_parameter(Context, Flag, Value) ->
    case flag_to_decompression_param_number(Flag) of
        {ok, Param} ->
            ezstd_nif:set_decompression_parameter(Context, Param, Value);
        error ->
            error
    end.

%% @doc Compress some data without closing out the compression frame. This
%% is intended to be used by a streaming decompressor which receives the same
%% data in the same order.
-spec decompress_streaming(reference(), binary()) -> iolist() | {error, any()}.
decompress_streaming(Context, Binary) ->
    decompress_streaming_chunk(Context, Binary, 0, [], 1000).

decompress_streaming_chunk(_Context, _Binary, _Offset, _Sofar, 0) ->
    {error, decompressor_stuck};

decompress_streaming_chunk(Context, Binary, Offset, Sofar, Attempts) ->
    case ezstd_nif:decompress_streaming_chunk(Context, Binary, Offset) of
        {ok, Chunk} ->
            [Sofar | Chunk];
        {continue, Chunk, NextOffset} ->
            decompress_streaming_chunk(Context, Binary, NextOffset, [Sofar | Chunk], Attempts - 1);
        Error ->
            Error
    end.

%% @doc Resets the compression context to its initial state.
%%
%% The `ResetDirective' can be one of:
%%
%% - `session_only' - only resets the session state, keeping the parameters and dictionary.
%%
%% - `parameters' - only resets the parameters and dictionary, keeping the session state.
%%
%% - `session_and_parameters' - resets both the session state and the parameters (and dictionary).
-spec reset_compression_context(reference(), zstd_reset_directive()) -> ok | {error, any()}.
reset_compression_context(Context, ResetDirective) ->
    ezstd_nif:reset_compression_context(Context, ResetDirective).

%% @doc Resets the decompression context to its initial state.
%%
%% The `ResetDirective' can be one of:
%%
%% - `session_only' - only resets the session state, keeping the parameters and dictionary.
%%
%% - `parameters' - only resets the parameters and dictionary, keeping the session state.
%%
%% - `session_and_parameters' - resets both the session state and the parameters (and dictionary).
-spec reset_decompression_context(reference(), zstd_reset_directive()) -> ok | {error, any()}.
reset_decompression_context(Context, ResetDirective) ->
    ezstd_nif:reset_decompression_context(Context, ResetDirective).

% internals

returns_integers(Value) ->
    % the _dict_id functions only return non-integers when
    % the preconditions are broken. ie: non-binary or non-reference
    % supplied as an argument
    % in the case of frame it returns 0 if the frame has no
    % dict_id
    case Value of
        V when is_integer(V) ->
            V;
        Other ->
            error(Other)
    end.

flag_to_compression_param_number(zstd_c_compression_level) ->
    {ok, 100};
flag_to_compression_param_number(zstd_c_window_log) ->
    {ok, 101};
flag_to_compression_param_number(zstd_c_hash_log) ->
    {ok, 102};
flag_to_compression_param_number(zstd_c_chain_log) ->
    {ok, 103};
flag_to_compression_param_number(zstd_c_search_log) ->
    {ok, 104};
flag_to_compression_param_number(zstd_c_min_match) ->
    {ok, 105};
flag_to_compression_param_number(zstd_c_target_length) ->
    {ok, 106};
flag_to_compression_param_number(zstd_c_strategy) ->
    {ok, 107};
flag_to_compression_param_number(_Other) ->
    {error, badarg}.

flag_to_decompression_param_number(zstd_d_window_log_max) ->
    {ok, 100};
flag_to_decompression_param_number(_Other) ->
    {error, badarg}.

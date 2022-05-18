%% @doc
%% Zstd [1] binding for Erlang.
%%
%% [1]: [http://facebook.github.io/zstd]
-module(ezstd).

-export([
    compress/1, 
    compress/2,
    decompress/1
]).

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

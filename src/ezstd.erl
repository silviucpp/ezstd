-module(ezstd).

-export([
    compress/1, 
    compress/2,
    decompress/1
]).

-spec compress(binary()) -> binary() | {error, any()}.

compress(Binary) ->
    ezstd_nif:compress(Binary, 1).

-spec compress(binary(), integer()) -> binary() | {error, any()}.

compress(Binary, CompressionLevel) ->
    ezstd_nif:compress(Binary, CompressionLevel).

-spec decompress(binary()) -> binary() | {error, any()}.

decompress(Binary) ->
    ezstd_nif:decompress(Binary).

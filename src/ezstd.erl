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
    get_dict_id_from_cdict/1
]).

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

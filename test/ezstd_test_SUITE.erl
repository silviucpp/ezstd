-module(ezstd_test_SUITE).

-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

all() -> [
    roundtrip_content_dictionary_test,
    roundtrip_content_using_real_dictionary_test,
    roundtrip_normal_compression_test,
    roundtrip_with_streaming_compress_test,
    roundtrip_streaming_full_frame,
    reset_context_test_a,
    reset_context_test_b,
    reset_context_real_dictionary_test
].

roundtrip_normal_compression_test(_) ->
  Plaintext = <<"contentcontentcontentcontent">>,
  Compressed = ezstd:compress(Plaintext, 1),
  ?assertEqual(Plaintext, ezstd:decompress(Compressed)).

roundtrip_content_dictionary_test(_) ->
  Dict = <<"content-dict">>,
  CDict = ezstd:create_cdict(Dict, 1),
  DDict = ezstd:create_ddict(Dict),
  Plaintext = <<"contentcontentcontentcontent">>,

  ContentCompressed = ezstd:compress_using_cdict(Plaintext, CDict),
  ?assertEqual(Plaintext, ezstd:decompress_using_ddict(ContentCompressed, DDict)).

roundtrip_content_using_real_dictionary_test(_) ->
  Dict = real_dictionary(),
  CDict = ezstd:create_cdict(Dict, 1),

  ?assertEqual(967448963, ezstd:get_dict_id_from_cdict(CDict)),

  DDict = ezstd:create_ddict(Dict),

  ?assertEqual(967448963, ezstd:get_dict_id_from_ddict(DDict)),
  Plaintext = <<"contentcontentcontentcontent">>,

  DictCompressed = ezstd:compress_using_cdict(Plaintext, CDict),
  ?assertEqual(967448963, ezstd:get_dict_id_from_frame(DictCompressed)),
  ?assertEqual(Plaintext, ezstd:decompress_using_ddict(DictCompressed, DDict)).

roundtrip_with_streaming_compress_test(_) ->
  Dict = real_dictionary(),
  CDict = ezstd:create_cdict(Dict, 10),
  DDict = ezstd:create_ddict(Dict),

  CContext = ezstd:create_compression_context(10),
  ?assertEqual(ok, ezstd:select_cdict(CContext, CDict)),
  ?assertEqual(ok, ezstd:set_compression_parameter(CContext, zstd_c_compression_level, 5)),
  ?assertEqual(ok, ezstd:set_compression_parameter(CContext, zstd_c_window_log, 12)),
  Plaintext = <<"contentcontentcontentcontentcontentcontentcontentcontentcontentcontentcontentcontent">>,
 
  CompressedIOList = ezstd:compress_streaming(CContext, Plaintext),
  CompressedBinary = erlang:iolist_to_binary(CompressedIOList),
  
  DContext = ezstd:create_decompression_context(10),
  ?assertEqual(ok, ezstd:select_ddict(DContext, DDict)),
  ?assertEqual(ok, ezstd:set_decompression_parameter(DContext, zstd_d_window_log_max, 15)),

  RehydratedIOList = ezstd:decompress_streaming(DContext, CompressedBinary),
  RehydratedBinary = erlang:iolist_to_binary(RehydratedIOList),
  ?assertEqual(Plaintext, RehydratedBinary),

  SecondPlaintext = <<"morecontentmorecontentmorecontentmorecontent">>,
  SecondBinary = erlang:iolist_to_binary(ezstd:compress_streaming(CContext, SecondPlaintext)),
  ?assertEqual(SecondPlaintext, erlang:iolist_to_binary(ezstd:decompress_streaming(DContext, SecondBinary))),

  ?assertEqual({error, <<"corrupted data">>}, ezstd:decompress_streaming(DContext, <<"this is not a compressed text">>)).

roundtrip_streaming_full_frame(_) ->
  Dict = real_dictionary(),
  CDict = ezstd:create_cdict(Dict, 10),
  DDict = ezstd:create_ddict(Dict),

  CContext = ezstd:create_compression_context(10),
  ?assertEqual(ok, ezstd:select_cdict(CContext, CDict)),
  Plaintext = <<"contentcontentcontentcontentcontentcontentcontentcontentcontentcontentcontentcontent">>,

  CompressedIOList = ezstd:compress_streaming_end(CContext, Plaintext),
  CompressedBinary = erlang:iolist_to_binary(CompressedIOList),

  RehydratedIOList = ezstd:decompress_using_ddict(CompressedBinary, DDict),
  RehydratedBinary = erlang:iolist_to_binary(RehydratedIOList),
  ?assertEqual(Plaintext, RehydratedBinary).

reset_context_test_a(_) ->
  % Create a compression context and compress a string
  CompressCtx = ezstd:create_compression_context(1024),
  CompressedA = ezstd:compress_streaming(CompressCtx, <<"content">>),

  % Reset the compression context (and the dictionary)
  ?assertEqual(ok, ezstd:reset_compression_context(CompressCtx, session_and_parameters)),

  % Compress the same string with the reset context
  CompressedB = ezstd:compress_streaming(CompressCtx, <<"content">>),

  % The compressed strings should be the same
  ?assertEqual(CompressedA, CompressedB).
  
reset_context_test_b(_) ->
  % Create a compression and decompression context
  CompressCtx = ezstd:create_compression_context(1024),
  DecompressCtx = ezstd:create_decompression_context(1024),

  % Compress a string and decompress it
  CompressedA = ezstd:compress_streaming(CompressCtx, <<"content">>),
  DecompressedA = ezstd:decompress_streaming(DecompressCtx, erlang:iolist_to_binary(CompressedA)),

  % The decompressed string should be the same as the original
  ?assertEqual(<<"content">>, erlang:iolist_to_binary(DecompressedA)),

  % Reset the decompression context
  ?assertEqual(ok, ezstd:reset_decompression_context(DecompressCtx, session_and_parameters)),

  % Compress the same string using the non-reset compression context
  CompressedB = ezstd:compress_streaming(CompressCtx, <<"content">>),

  % Attempt to decompress
  AttemptDecompress = ezstd:decompress_streaming(DecompressCtx, erlang:iolist_to_binary(CompressedB)),

  % Expect failure because the decompression context was reset
  ?assertEqual({error, <<"corrupted data">>}, AttemptDecompress).

reset_context_real_dictionary_test(_) ->
    % Create a new compression and decompression dictionary
    Dict = real_dictionary(),
    CDict = ezstd:create_cdict(Dict, 10),
    DDict = ezstd:create_ddict(Dict),

    % Create a compression context and use the created dictionary
    CompressCtx = ezstd:create_compression_context(10),
    ?assertEqual(ok, ezstd:select_cdict(CompressCtx, CDict)),

    % Create a decompression context and use the created dictionary
    DecompressCtx = ezstd:create_decompression_context(10),
    ?assertEqual(ok, ezstd:select_ddict(DecompressCtx, DDict)),

    % Compress and decompress some data
    Compressed = ezstd:compress_streaming(CompressCtx, <<"contentcontentcontent">>),
    Decompressed = ezstd:decompress_streaming(DecompressCtx, erlang:iolist_to_binary(Compressed)),

    % Validate the success of the compress/decompress operation
    ?assertEqual(<<"contentcontentcontent">>, erlang:iolist_to_binary(Decompressed)),

    % Reset the decompressor, wiping the session, parameters and dictionary
    ?assertEqual(ok, ezstd:reset_decompression_context(DecompressCtx, session_and_parameters)),

    % Try to decompress the same compressed value from earlier, this should
    % fail as we don't have the same dictionary
    ShouldFailDecompress = ezstd:decompress_streaming(DecompressCtx, erlang:iolist_to_binary(Compressed)),

    ?assertEqual({error, <<"corrupted data">>}, ShouldFailDecompress),

    % Reset the decompressor again but this time select a newly created dictionary
    % with the same value as earlier
    ?assertEqual(ok, ezstd:reset_decompression_context(DecompressCtx, session_and_parameters)),

    NewDDict = ezstd:create_ddict(Dict),
    ezstd:select_ddict(DecompressCtx, NewDDict),

    % Attempt the decompression again with the initial compressed blob, expecting success
    AttemptDecompress = ezstd:decompress_streaming(DecompressCtx, erlang:iolist_to_binary(Compressed)),

    ?assertEqual(<<"contentcontentcontent">>, erlang:iolist_to_binary(AttemptDecompress)).

% internals

real_dictionary() ->
  <<16#37,16#a4,16#30,16#ec,16#83,16#19,16#aa,16#39,16#9,16#10,16#10,16#df,16#30,16#33,16#33,16#b3,16#77,16#a,16#33,16#f1,16#78,16#3c,16#1e,16#8f,16#c7,16#e3,16#f1,16#78,16#3c,16#cf,16#f3,16#bc,16#f7,16#d4,16#42,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#a1,16#50,16#28,16#14,16#a,16#85,16#42,16#a1,16#50,16#28,16#14,16#a,16#85,16#a2,16#28,16#8a,16#a2,16#28,16#4a,16#29,16#7d,16#74,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#f1,16#78,16#3c,16#1e,16#8f,16#c7,16#e3,16#f1,16#78,16#9e,16#e7,16#79,16#ef,16#1,16#1,16#0,16#0,16#0,16#4,16#0,16#0,16#0,16#8,16#0,16#0,16#0,16#63,16#6f,16#6e,16#74,16#65,16#6e,16#74,16#a>>.


-module(ezstd_test_SUITE).

-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

all() -> [
    roundtrip_content_dictionary_test,
    roundtrip_content_using_real_dictionary_test,
    roundtrip_normal_compression_test,
    roundtrip_with_streaming_compress_test
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

  CContext = ezstd:create_compression_context(512),
  ?assertEqual(ok, ezstd:select_cdict(CContext, CDict)),
  Plaintext = <<"contentcontentcontentcontent">>,
 
  CompressedIOList = ezstd:compress_streaming(CContext, Plaintext),
  CompressedBinary = erlang:iolist_to_binary(CompressedIOList),
  
  DContext = ezstd:create_decompression_context(1024),
  ?assertEqual(ok, ezstd:select_ddict(DContext, DDict)),

  RehydratedIOList = ezstd:decompress_streaming(DContext, CompressedBinary),
  RehydratedBinary = erlang:iolist_to_binary(RehydratedIOList),
  ?assertEqual(Plaintext, RehydratedBinary),

  SecondPlaintext = <<"morecontentmorecontentmorecontentmorecontent">>,
  SecondBinary = erlang:iolist_to_binary(ezstd:compress_streaming(CContext, SecondPlaintext)),
  ?assertEqual(SecondPlaintext, erlang:iolist_to_binary(ezstd:decompress_streaming(DContext, SecondBinary))),

  ?assertEqual({error, <<"corrupted data">>}, ezstd:decompress_streaming(DContext, <<"this is not a compressed text">>)).




% internals

real_dictionary() ->
  <<16#37,16#a4,16#30,16#ec,16#83,16#19,16#aa,16#39,16#9,16#10,16#10,16#df,16#30,16#33,16#33,16#b3,16#77,16#a,16#33,16#f1,16#78,16#3c,16#1e,16#8f,16#c7,16#e3,16#f1,16#78,16#3c,16#cf,16#f3,16#bc,16#f7,16#d4,16#42,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#41,16#a1,16#50,16#28,16#14,16#a,16#85,16#42,16#a1,16#50,16#28,16#14,16#a,16#85,16#a2,16#28,16#8a,16#a2,16#28,16#4a,16#29,16#7d,16#74,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#e1,16#f1,16#78,16#3c,16#1e,16#8f,16#c7,16#e3,16#f1,16#78,16#9e,16#e7,16#79,16#ef,16#1,16#1,16#0,16#0,16#0,16#4,16#0,16#0,16#0,16#8,16#0,16#0,16#0,16#63,16#6f,16#6e,16#74,16#65,16#6e,16#74,16#a>>.


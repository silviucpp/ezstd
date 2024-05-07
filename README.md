# ezstd

[![Build Status](https://app.travis-ci.com/silviucpp/ezstd.svg?branch=master)](https://travis-ci.com/github/silviucpp/ezstd)
[![GitHub](https://img.shields.io/github/license/silviucpp/ezstd)](https://github.com/silviucpp/ezstd/blob/master/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/ezstd)](https://hex.pm/packages/ezstd)

## [Zstd][1] binding for Erlang

This binding is based on zstd v1.5.5. In case you want to modify the `zstd` version you can change `ZSTD_TAG` from `build_deps.sh`

## API

### Compress and decompress

```erl
Plaintext = <<"contentcontentcontentcontent">>,
Compressed = ezstd:compress(Plaintext, 1),
Plaintext = ezstd:decompress(Compressed).
```

### Compress and decompress using dictionary

```erl
Dict = <<"content-dict">>,
CDict = ezstd:create_cdict(Dict, 1),
DDict = ezstd:create_ddict(Dict),
Plaintext = <<"contentcontentcontentcontent">>,
ContentCompressed = ezstd:compress_using_cdict(Plaintext, CDict),
Plaintext = ezstd:decompress_using_ddict(ContentCompressed, DDict).
```

## Running tests

```sh
rebar3 ct
```

[1]:http://facebook.github.io/zstd/

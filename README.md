ezstd
=====

[![Build Status](https://travis-ci.com/silviucpp/ezstd.svg?branch=master)](https://travis-ci.com/github/silviucpp/ezstd)
![GitHub](https://img.shields.io/github/license/silviucpp/ezstd)
![Hex.pm](https://img.shields.io/hexpm/v/ezstd)

[Zstd][1] binding for Erlang

This binding is based on zstd v1.5.1. In case you want to modify the zstd version you can change `ZSTD_REV` from `build_deps.sh`

Usage
-----

```
ezstd:compress(<<"hello world">>).
ezstd:compress(<<"hello world">>, 3).
ezstd:decompress(ezstd:compress(<<"hello world">>, 3)).
```

[1]:http://facebook.github.io/zstd/

ezstd
=====

[![Build Status](https://travis-ci.org/silviucpp/ezstd.svg?branch=master)](https://travis-ci.org/silviucpp/ezstd)
![GitHub](https://img.shields.io/github/license/silviucpp/ezstd)
![Hex.pm](https://img.shields.io/hexpm/v/ezstd)
![Maintenance](https://img.shields.io/maintenance/yes/2019)

[Zstd][1] binding for Erlang

Usage
-----

```
ezstd:compress(<<"hello world">>).
ezstd:compress(<<"hello world">>, 3).
ezstd:decompress(ezstd:compress(<<"hello world">>, 3)).
```

[1]:http://facebook.github.io/zstd/

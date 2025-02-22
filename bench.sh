#!/bin/bash
set -e

RunBench()
{
    if [ -z "$1" ]; then
        echo "Compiling with CMAKE"
        export CFLAGS=
        export NO_CMAKE=
    else
        echo "Compiling with CFLAGS=$1 and NO_CMAKE=1"
        export CFLAGS=$1
        export NO_CMAKE=1
    fi
    make clean_zstd >> /dev/null
    rebar3 compile >> /dev/null
    rebar3 shell --script scripts/bench_compress.erl | grep -oE "AVG Time: .*"
}

RunBench "-g"
RunBench "-O1"
RunBench "-O2"
RunBench "-O2 -march=native -mtune=native"
RunBench

# Benchmark results:
#   Operating System: Linux
#   CPU Information: AMD Ryzen 9 6900HS with Radeon Graphics
#   Erlang 26.2.5.8
#
# Based on these decided to default to CFLAGS=-O2 if no cmake is available. As "-O2" is much faster than the default
# and still keeps the binary compatible with other CPU architectures (e.g. for shipping releases).
#
# ./bench.sh
# Compiling with CFLAGS=-g and NO_CMAKE=1
# AVG Time: 2725070.5
# Compiling with CFLAGS=-O1 and NO_CMAKE=1
# AVG Time: 804889.6
# Compiling with CFLAGS=-O2 and NO_CMAKE=1
# AVG Time: 688417.6
# Compiling with CFLAGS=-O2 -march=native -mtune=native and NO_CMAKE=1
# AVG Time: 652656.5
# Compiling with CMAKE
# AVG Time: 631423.5
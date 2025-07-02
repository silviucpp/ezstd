#!/usr/bin/env bash

ROOT=$(pwd)
DEPS_LOCATION=_build/deps
OS=$(uname -s)
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')
CPUS=`getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu`

# https://github.com/facebook/zstd.git

ZSTD_DESTINATION=zstd
ZSTD_REPO=https://github.com/facebook/zstd.git
ZSTD_BRANCH=release
ZSTD_TAG=v1.5.7
ZSTD_SUCCESS=lib/libzstd.a

fail_check()
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

CheckoutLib()
{
    if [ -f "$DEPS_LOCATION/$4/$5" ]; then
        echo "$4 fork already exist. delete $DEPS_LOCATION/$4 for a fresh checkout ..."
    else
        #repo rev branch destination

        echo "repo=$1 tag=$2 branch=$3"

        mkdir -p $DEPS_LOCATION
        pushd $DEPS_LOCATION

        if [ ! -d "$4" ]; then
            fail_check git clone -b $3 $1 $4
        fi

        pushd $4
        fail_check git checkout $2
        BuildLibrary $4
        popd
        popd


    fi
}

BuildLibrary()
{
    if [ -z "$NO_CMAKE" ] && command -v cmake >/dev/null 2>&1; then
        echo "Using cmake build system..."
        cmake -S build/cmake
    else
        echo "cmake not found, using make directly..."
        # Only define -O2 if CFLAGS is not already defined
        export CFLAGS=${CFLAGS:-"-O2"}

        case $OS in
            Linux)
                export CFLAGS="$CFLAGS -fPIC"
                export CXXFLAGS="$CXXFLAGS -fPIC"
                ;;
            *)
                ;;
        esac
    fi

    fail_check make -j $CPUS
    rm -rf lib/*.so
    rm -rf lib/*.so.*
    rm -rf lib/*.dylib
}

CheckoutLib $ZSTD_REPO $ZSTD_TAG $ZSTD_BRANCH $ZSTD_DESTINATION $ZSTD_SUCCESS

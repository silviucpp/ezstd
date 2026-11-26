#!/usr/bin/env bash
set -euo pipefail

if [ -n "${EZSTD_SYSTEM_LDFLAGS-}" ]; then
    echo "🏡 Using system $EZSTD_SYSTEM_LDFLAGS ${EZSTD_SYSTEM_CFLAGS-}"
    exit 0
fi

ROOT="$(pwd)"
DEPS_DIR="_build/deps"
OS="$(uname -s)"
KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')
CPUS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu)"

# Zstandard configuration
# https://github.com/facebook/zstd.git

ZSTD_REPO="https://github.com/facebook/zstd.git"
# Pin to an immutable commit SHA instead of a mutable tag: tags can be
# re-pointed at a different commit upstream, so trusting the tag alone is a
# supply-chain risk. Resolve a tag to its commit with:
#   git ls-remote https://github.com/facebook/zstd.git 'v1.5.7^{}'
ZSTD_SHA="f8745da6ff1ad1e7bab384bd1f9d742439278e99" # v1.5.7
ZSTD_DIR="zstd"
ZSTD_SUCCESS_FILE="lib/libzstd.a"

fail_check() {
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "❌ Error running command: $*" >&2
        exit $status
    fi
}

checkout_lib() {
    local repo_url="$1"
    local sha="$2"
    local dir_name="$3"
    local success_file="$4"

    local full_path="$DEPS_DIR/$dir_name/$success_file"
    if [ -f "$full_path" ]; then
        echo "✅ $dir_name already exists at $full_path"
        echo "   To rebuild, delete: $DEPS_DIR/$dir_name"
        return
    fi

    echo "📦 Cloning $repo_url (pinned commit: $sha)"

    mkdir -p "$DEPS_DIR"
    pushd "$DEPS_DIR" > /dev/null

    if [ ! -d "$dir_name" ]; then
        # --no-tags keeps mutable tag refs off disk; we check out an immutable
        # commit by SHA, so tags are never consulted or trusted.
        fail_check git clone --no-tags "$repo_url" "$dir_name"
    fi

    pushd "$dir_name" > /dev/null
    fail_check git checkout "$sha"
    build_library "$dir_name"
    popd > /dev/null
    popd > /dev/null
}

build_library() {
    local dir="$1"
    echo "🔧 Building $dir"

    if [[ -z "${NO_CMAKE:-}" ]] && command -v cmake >/dev/null 2>&1; then
        echo "   ➤ Using CMake..."
        cmake -S build/cmake -DZSTD_BUILD_PROGRAMS=OFF -DZSTD_LEGACY_SUPPORT=OFF
        fail_check make libzstd_static -j "$CPUS"
    else
        echo "   ➤ Using Make directly..."
        export CFLAGS="${CFLAGS:--O2}"

        if [[ "$OS" == "Linux" ]]; then
            export CFLAGS="$CFLAGS -fPIC"
            export CXXFLAGS="${CXXFLAGS:-} -fPIC"
        fi

        fail_check make lib-release -j "$CPUS"

        # Remove shared libs to ensure a static-only build
        rm -f lib/*.so lib/*.so.* lib/*.dylib
    fi
}

echo "🖥️  Detected system configuration:"
echo "   ➤ OS Type   : $OS"
echo "   ➤ OS Name   : $KERNEL"
echo "   ➤ CPU Cores : $CPUS"

checkout_lib "$ZSTD_REPO" "$ZSTD_SHA" "$ZSTD_DIR" "$ZSTD_SUCCESS_FILE"

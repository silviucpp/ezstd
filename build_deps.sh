#!/usr/bin/env bash
set -euo pipefail

ROOT="$(pwd)"
DEPS_DIR="_build/deps"
OS="$(uname -s)"
KERNEL="$(lsb_release -is 2>/dev/null || grep -m1 '^NAME=' /etc/*release | cut -d= -f2 | awk '{print $1}' | tr -d '"')"
CPUS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || sysctl -n hw.ncpu)"

# Zstandard configuration
# https://github.com/facebook/zstd.git

ZSTD_REPO="https://github.com/facebook/zstd.git"
ZSTD_BRANCH="release"
ZSTD_TAG="v1.5.7"
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
    local tag="$2"
    local branch="$3"
    local dir_name="$4"
    local success_file="$5"

    local full_path="$DEPS_DIR/$dir_name/$success_file"
    if [ -f "$full_path" ]; then
        echo "✅ $dir_name already exists at $full_path"
        echo "   To rebuild, delete: $DEPS_DIR/$dir_name"
        return
    fi

    echo "📦 Cloning $repo_url (branch: $branch, tag: $tag)"

    mkdir -p "$DEPS_DIR"
    pushd "$DEPS_DIR" > /dev/null

    if [ ! -d "$dir_name" ]; then
        fail_check git clone --branch "$branch" "$repo_url" "$dir_name"
    fi

    pushd "$dir_name" > /dev/null
    fail_check git checkout "$tag"
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

checkout_lib "$ZSTD_REPO" "$ZSTD_TAG" "$ZSTD_BRANCH" "$ZSTD_DIR" "$ZSTD_SUCCESS_FILE"

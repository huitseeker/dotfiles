#!/bin/bash
set -euxo pipefail

outfile="mirai-${PWD##*/}"

rustup override set $(cat ~/tmp/MIRAI/rust-toolchain)
RUSTFLAGS="-Z always_encode_mir" cargo build --tests
find ./ -iname 'lib.rs' -exec touch '{}' \;

export DYLD_LIBRARY_PATH=~/.rustup/toolchains/$(cat ~/tmp/MIRAI/rust-toolchain)-x86_64-unknown-linux-gnu/lib
RUSTC_WRAPPER=mirai RUSTFLAGS="-Z always_encode_mir" cargo "$@" 2>&1 | tee $HOME/tmp/$outfile

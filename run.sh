#!/bin/bash
PATH_TO_PROJECT=$(pwd)
export PKG_CONFIG_PATH=$PATH_TO_PROJECT/rust-wrapper/target/x86_64-unknown-linux-gnu/debug
export LD_LIBRARY_PATH=$PATH_TO_PROJECT/rust-wrapper/target/x86_64-unknown-linux-gnu/debug
cd $PATH_TO_PROJECT/rust-wrapper
cbindgen --lang=c -o haskell-wrapper/cbits/rust-wrapper.h
cargo cbuild
cd $PATH_TO_PROJECT
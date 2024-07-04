#!/bin/bash
cbindgen --lang=c -o haskell-wrapper/cbits/rust-wrapper.h
export PKG_CONFIG_PATH=/Users/iokasimov/Documents/code/zkfold/zkfold-prover/rust-wrapper/target/aarch64-apple-darwin/debug
export LD_LIBRARY_PATH=/Users/iokasimov/Documents/code/zkfold/zkfold-prover/rust-wrapper/target/aarch64-apple-darwin/debug
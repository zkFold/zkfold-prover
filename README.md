# zkFold Prover
Optimized ZK provers

### Setting up the environment

<!-- In the comments you can find instructions for installing the required packages. The instructions were tested on an Ubuntu 24.04 virtual machine -->

<!-- Curl*
```bash
sudo apt install curl
``` -->
Install rustup and cargo (require `curl`):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

<!-- gcc*
```bash
sudo apt install gcc
``` -->

Install cbindgen (require `gcc`):
```bash
cargo install --force cbindgen
```
<!-- openssl*
```bash
sudo apt-get install libssl-dev
```
pkg-config*
```bash
sudo apt install pkg-config
``` -->

Install cargo-c (require `openssl` and `pkg-config`):
```bash
cargo install cargo-c
```

### Build Rust wrap

You need to run the file `run.sh` from the project directory (or specify the path in the file).

```bash
source ./run.sh
```

### Build, Tests and Examples

The package compiles with GHC 9.6.3 and Cabal 3.10.2.1.

Running tests:
```bash
cabal run wrapper-test
```

Run benchmark:
```bash
cabal run msm
```
### Haskell profiling and benchmarking

Before you need install `ghc-prof-flamegraph`. Remember path to executable file
```bash
cabal install ghc-prof-flamegraph
```

Run benchmark with .prof generating
```bash
cabal run msm --enable-profiling -- +RTS -p
```

Run generating flamegraph. Make sure the path to `ghc-prof-flamegraph` is correct
```bash
~/.local/bin/ghc-prof-flamegraph msm.prof
```
Flamegraph will be in `msm.svg` file


### Rust profiling and benchmarking

To run benchmark install [cargo-criterion](https://github.com/bheisler/cargo-criterion)
```bash
cargo install cargo-criterion
```
And run
```bash
cargo criterion --bench msm_bench
```

For generate flamegraph install [flamegraph](https://github.com/flamegraph-rs/flamegraph) (require `perf`)
```bash
cargo install flamegraph
```

And generate flamegraph from bench:
```bash
cargo flamegraph --bench msm_bench
```


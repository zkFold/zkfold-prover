# zkFold Prover
Optimized ZK provers

### Setting up the environment

For Rust wrapper part you need to setup rustup
Also need curl, 
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```
Install cargo:
```bash
rustup install cargo
```
Install cbindgen:
```bash
cargo install --force cbindgen
```
Install cargo-c:
```bash
cargo install cargo-c
```

### Run

You need to run the file `run.sh` from the project directory (or specify the path in the file).

```bash
source ./run.sh
```

### Run

Running tests:
```bash
cabal run -- wrapper-test
```

Running Main:
```
cabal run haskell-wrapper/
```
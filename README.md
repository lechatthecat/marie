# Oran
Programming language written by rust  
**Still in development**

# rust version
```
$ rustc --version
rustc 1.41.0 (5e1a79984 2020-01-27)
$ cargo --version
cargo 1.41.0 (626f0f40e 2019-12-03)
```

# compatibility
tested in Ubuntu 18.04.3 LTS.

# run the project
```
$ git clone https://github.com/lechatthecat/oran.git
$ cd oran
$ cargo build
$ ./target/debug/oran -f ./examples/exmple.orn
```
# To try with nightly rust
Use the nightly version by `$ rustup default nightly`.  
You can change it back by `$ rustup default stable`.
```
$ rustup toolchain install nightly
$ rustup default nightly
```

# Oran
Programming language written by rust  
**Still in development**

# Grammar
This is a scripting language, but the grammar is very similar to Rust:
```rust
fn hello {
    println("Hello World");
}
```

# Rust version
```
$ rustc --version
rustc 1.41.0 (5e1a79984 2020-01-27)
$ cargo --version
cargo 1.41.0 (626f0f40e 2019-12-03)
```

# Compatibility
Tested in Ubuntu 18.04.3 LTS.
But should work in other platforms too.

# Run the project
```
$ git clone https://github.com/lechatthecat/oran.git
$ cd oran
$ cargo build --release
$ ./target/release/oran -f ./examples/hello.orn
```

# To try with nightly rust
Use the nightly version by `$ rustup default nightly`.  
You can change it back by `$ rustup default stable`.
```
$ rustup toolchain install nightly
$ rustup default nightly
```

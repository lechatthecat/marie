# Oran
Programming language written by rust  
**Still in development**

# Syntax
This is a scripting language, but the syntax is very similar to Rust:
```rust
fn hello () {
    println("Hello World");
}
```

# Rust version
```
$ rustc --version
rustc 1.47.0 (18bf6b4f0 2020-10-07)
$ cargo --version
cargo 1.47.0 (f3c7e066a 2020-08-28)
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
$ ./target/release/oran -f ./examples/example.orn
```

# To try with nightly rust
Use the nightly version by `$ rustup default nightly`.  
You can change it back by `$ rustup default stable`.
```
$ rustup toolchain install nightly
$ rustup default nightly
```

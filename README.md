# Oran
Programming language (written by rust)  
You can also see how I tried JIT compiled scripting language and failed: https://github.com/lechatthecat/marie
**Still in development**

# Syntax
This is a scripting language. Syntax is as follows:
```rust
fn hello () {
    println("Hello World");
}
```

```rust
fn test () {
    let test = 'hey';
    println(test);
}
test();
````

```rust
fn test () {
    let test = ' test';
    for i in 0..5 {
        println(i << test);
    }
}
test();
```
Please note that you need "mut" for mutable variables.
```rust
fn test () {
    let mut test = 'hey'; // here
    test = "hello"; 
    println(test);
}
test();
```

If you want, you can omit "return" like in rust.  
If you write something without ";" at the end of function, it's considered as returened sentence.
```rust
fn test (test1, test2) {
    test1 + test2
}
fn test2 (test1, test2) {
    return test1 * test2;
}

println(test(5,5));
println(test2(5,5));
```

You can see many other examples in examples/example.orn

# Example
Calculate n-th of Fibonacci sequence:
```rust
fn fib (n) {
  let mut f0 = 0;
  let mut f1 = 1;
  let mut f2 = 0;

  for i in 1..n {
    f2 = f1 + f0;
    f0 = f1;
    f1 = f2;
  }
  println("Answer:"<<f1);
}
fib(50);
```

Result:
```
$ ./target/release/oran -f  ./examples/hello.orn -t
Answer:12586269025
443.486µs
```

Calculate n-th of Fibonacci sequence by recursive call (this is slow though):
```rust
fn fib(n) {
    if n < 2 {
        return n;
    } else {
        return fib(n-1) + fib(n-2);
    }
}
println(fib(15));
```

Result:
```
$ ./target/release/oran -f  ./examples/hello.orn -t
610
722.379µs
```

# Rust version
```
$ rustc --version
rustc 1.48.0 (7eac88abb 2020-11-16)
$ cargo --version
cargo 1.48.0 (65cbdd2dc 2020-10-14)
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

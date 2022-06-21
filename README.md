### About
This is a Scripting programming language

### Example
```rust
fn hello () {
    print("Hello World");
}
```
```rust
fn test () {
    let test = "hey";
    print(test);
}
test();
````
```rust
fn fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

let before = clock();
print fib(15);
let after = clock();
print after - before;
```
Please note that you need "mut" for mutable variables.
```rust
fn test () {
    let mut test = "hey"; // here
    test = "hello"; 
    print(test);
}
test();
```

How instance is created:
```rust
class A {
    f() {
        return "cat";
    }
}
let b = new A();
print(b.f());
```

Please run `cargo test` and see test cases for more examples.

### How to use
```
$ git clone https://github.com/lechatthecat/marie.git
$ cd marie
$ cargo build --release
$ cargo run --quiet --release example/test.mr
```
For debug run, use 
```
$ cargo run --quiet -- example/test.mr
```
or
```
$ cargo run --quiet -- --debug example/test.mr
```

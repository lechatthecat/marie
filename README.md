### About
This is a Scripting programming language

### Example
```rust
fn hello () {
    print("Hello World");
}
hello(); 
```
```rust
fn test (mut abc) {
    let test = "hey";
    print(test);
    print(abc);
}
test(1);
````
```rust
fn fib(n) {
  if (n < 2) return n;
  return fib(n - 1) + fib(n - 2);
}

let before = clock();
print(fib(15));
let after = clock();
print(after - before << "ms");
```

You can return values like in rust:
```rust
fn test() {
  "hey guys"
}

print(test());
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
    pub name = "john";
    pub weight = 10;
    pub fn f() {
        return "cat";
    }
}
let b = new A();
print(b.name);
print(b.f());
```
Please note that, if you don't use "pub", the properties automatically become private.

"+" cannot be used for String concatenation.  
String concatenation is done like this:
```rust
fn test () {
    let myfrined = "john";
    let test = "hey, " << myfrined; // here
    print(test);
}
test();
```
Why? Well, isn't it annoying when string-type numbers are unintentionally concatenated when you want to calculate it?  
  
You can also see example file here: [example/test.mr](https://github.com/lechatthecat/marie/blob/master/example/test.mr)  
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

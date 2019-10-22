# Oran
Programming language written by rust  
** Still in development **

# dependencies
You need to install:  
cmake make g++ 

And you need to install llvm8.

# compatibility
tested in Ubuntu 18.

# run the project
You must specify where the llvm8 is by `LLVM_SYS_80_PREFIX=`.
```
$ git clone https://github.com/lechatthecat/oran.git
$ cd oran
$ LLVM_SYS_80_PREFIX=$HOME/llvm-8.0.0 cargo build
$ ./target/debug/oran
```

Bigloo-WASM, a WASM backend for the Bigloo compiler
===================================================

This is a fork of the "official" Bigloo distribution that adds a
WASM backend to the compiler and an implementation of the runtime system.

> [!CAUTION]
> This fork is experimental and not all Bigloo features are supported yet.
> The main missing features are:

  - no thread support;
  - no `call/cc`.

> [!WARNING]
> Some features are known to be slow:

  - regexp;
  - bignum.
  
Installation
------------

To install the Bigloo-WASM branch:

```
./configure --wasm=yes && make && make install
```

# How to use Bigloo with WASM backend

```
bigloo -wasm source.scm 
```

This generates a shell script that executes the generated wasm file
with node. 

> [!WARNING]
> The generated WASM code can only be executed by a Nodejs
> compatible JavaScript engine because some of the library functions,
> for instance those dealing with IO, are backed by Nodejs JavaScript
> functions. Currently there is no option for selecting another
> JavaScript engine when invoking Bigloo.

## TODO

+ Integrate `bigloo-wasm-merge` directly to the WASM backend
+ Implement JS regexes
+ Implement string input ports
+ Implement remaining date functions
+ Implement HTTP and sockets
+ Implement OS functions
+ Implement mmap
+ Implement what is missing in the runtime library (see [runtime/objs/wasm_s/dummy_runtime.wat] after compiling the runtime library)
+ Fix compilation bugs when compiling in safe mode some libraries
+ Fix runtime bugs in intext
+ See todos in the WASM backend code (in comptime and runtime folders)
+ Fix compilation of classes with mutual dependencies
+ Transform irreducible CFG to reducible ones for Relooper
+ Cleanup the WASM backend
+ Directly assemble WAT to binary WASM in the backend (to avoid dependency on external `wasm-as`)
+ Profile and optimize the generated WASM code

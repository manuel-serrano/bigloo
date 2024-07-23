# How to use Bigloo with WASM backend

Let's explain how to compile a foo.scm scheme file to WebAssembly and run it.

1. First, compile Bigloo in this repository (as usually).
2. Make the WASM heap: `cd runtime && make heap-wasm`.
3. Compile the WASM lib: `cd runtime && make lib-wasm -j`.
This will create the files `bigloo_s.wasm` and `bigloo_s.wasm.map` (source map) in the Bigloo lib directory.

Now, it is time to compile foo.scm.

1. Generate the .wat file: `bigloo -wasm foo.scm -o foo.wat`.
2. Link the file with the Bigloo standard library: `wasm-merge -all $LIBDIR/bigloo_s.wasm bigloo $LIBDIR/bigloo_s.wasm __runtime foo.wat foo --rename-export-conflicts -o foo.wasm`.
If you want, you can add the options `-g -ism $LIBDIR/bigloo_s.wasm.map` to add debug info (recommended).
3. This will create a `foo.wasm` file that can be executed in any supported WASM virtual machine.

Finally, lets run the code:
1. Using Node: `node bigloo.mjs foo.wasm`.
2. Or using Deno: `deno run --allow-read bigloo.mjs foo.wasm`.

Sum up to compile `test.scm` and run it:
```sh
LIBWASM=$LIBDIR/bigloo_s.wasm

make -C comptime new -j
make -C runtime heap-wasm BIGLOO=./bin/bigloo.new
make -C runtime lib-wasm -j BIGLOO=./bin/bigloo.new

./bin/bigloo.new -wasm test.scm -o test.wat
wasm-merge -all -g $LIBWASM bigloo $LIBWASM __runtime -ism $LIBWASM.map foo.wat foo --rename-export-conflicts -o foo.wasm
node bigloo.mjs test.wasm
```

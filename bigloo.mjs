import { readFile } from 'node:fs/promises';
import { argv, exit } from 'node:process';
import { existsSync } from 'node:fs';
import { extname } from 'node:path';
import process from 'node:process';

if (argv.length < 3) {
    console.error("ERROR: missing input WASM module file.");
    exit(1);
} else if (!existsSync(argv[2])) {
    console.error(`ERROR: file '${argv[2]}' doesn't exist.`);
    exit(1);
} else if (extname(argv[2]) != ".wasm") {
    console.error(`ERROR: input file '${argv[2]}' is not a WASM module.`);
    exit(1);
}

const wasm = await WebAssembly.compile(await readFile(argv[2]));
const instance = await WebAssembly.instantiate(wasm, {
    __js: {
        trace: function (x) {
            console.log("TRACE: " + x);
        },

        read_string: function (fd, offset, length) {
            const memory = new Uint32Array(instance.exports.memory.buffer);
            let buffer = undefined;
            switch (fd) {
                case 0:
                    buffer = process.stdin.read(length);
                case 1:
                case 2:
                    break;
            }

            if (buffer) {
                memory.set(buffer, offset);
                return buffer.length;
            } else {
                return 0;
            }
        },

        write_string: function (fd, offset, length) {
            const memory = new Uint32Array(instance.exports.memory.buffer);
            let buffer = new Uint8Array(memory.buffer, offset, length);
            switch (fd) {
                case 0: 
                    break;
                case 1:
                    process.stdout.write(buffer);
                    break;
                case 2:
                    process.stderr.write(buffer);
                    break;
            }
        }
    }
});

if (!instance.exports.bigloo_main) {
    console.error("ERROR: missing 'bigloo_main' symbol in WASM module file.");
    exit(1);
}

instance.exports.bigloo_main(null);

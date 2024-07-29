import { readFile } from 'node:fs/promises';
import process, { argv, exit } from 'node:process';
import { closeSync, existsSync, openSync, readSync, writeSync } from 'node:fs';
import { extname } from 'node:path';

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

        is_tty: function (fd) {
            switch (fd) {
                case 0: // stdin
                    return process.stdin.isTTY;
                case 1: // stdout
                    return process.stdout.isTTY;
                case 2: // stderr
                    return process.stderr.isTTY;
                default:
                    return false;
            }
        },

        open_file: function (path_addr, path_length, flags) {
            const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
            const path = new TextDecoder().decode(buffer);

            let fs_flags;
            switch (flags) {
                // In the following flags, we add the 's' which stands for synchronous mode.
                case 0: // read-only
                    fs_flags = 'rs';
                    break;
                case 1: // write-only
                    fs_flags = 'w';
                    break;
                case 2: // write-only in append mode
                    fs_flags = 'as';
                    break;
                default:
                    throw WebAssembly.RuntimeError("invalid open flags");
            }

            return openSync(path, fs_flags);
        },

        close_file: function (fd) {
            if (fd < 0)
                throw WebAssembly.RuntimeError("invalid file descriptor");

            closeSync(fd);
        },

        read_file: function (fd, offset, length) {
            if (fd < 0)
                throw WebAssembly.RuntimeError("invalid file descriptor");

            const memory = new Uint8Array(instance.exports.memory.buffer, offset, length);
            const readBytes = readSync(fd, memory);
            return readBytes;
        },

        write_file: function (fd, offset, length) {
            if (fd < 0)
                throw WebAssembly.RuntimeError("invalid file descriptor");

            const buffer = new Uint8Array(instance.exports.memory.buffer, offset, length);
            writeSync(fd, buffer);
        }
    }
});

if (!instance.exports.bigloo_main) {
    console.error("ERROR: missing 'bigloo_main' symbol in WASM module file.");
    exit(1);
}

instance.exports.bigloo_main(null);

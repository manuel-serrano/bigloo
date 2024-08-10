import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync } from 'node:fs';
import { readFile } from 'node:fs/promises';
import { extname } from 'node:path';

// This code is a bit strange but is required to support Deno and NodeJS.
// If we import 'process' in NodeJS, readSync() will throw the error EAGAIN
// when reading, so we can't import it. However, in Deno, process is not
// a global variable and therefore we need to explicitly import 'process'.
let argv;
if (globalThis.window && "Deno" in window) {
    argv = (await import ('node:process')).argv;
} else {
    argv = process.argv;
}

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

const schemeStringDecoder = new TextDecoder();
const schemeStringEncoder = new TextEncoder();
function loadSchemeString(buffer) { return schemeStringDecoder.decode(buffer); }
function storeJSStringToScheme(string, addr) {
    const memory = new Uint8Array(instance.exports.memory.buffer, addr);
    const bytes = schemeStringEncoder.encode(string);
    memory.set(bytes);
    return bytes.length;
}

const wasm = await WebAssembly.compile(await readFile(argv[2]));
const instance = await WebAssembly.instantiate(wasm, {
    __js: {
        trace: function (x) {
            console.log("TRACE: " + x);
        },

        is_tty: function (fd) {
            // FIXME: will not work in Deno as process is not imported globally.
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
            const path = loadSchemeString(buffer);

            let fs_flags;
            switch (flags) {
                // In the following flags, we add the 's' which stands for synchronous mode.
                case 0: // read-only
                    fs_flags = 'r';
                    break;
                case 1: // write-only
                    fs_flags = 'w';
                    break;
                case 2: // write-only in append mode
                    fs_flags = 'a';
                    break;
                default:
                    throw WebAssembly.RuntimeError("invalid open flags");
            }

            const fd = openSync(path, fs_flags);
            return fd;
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
        },

        file_exists: function (path_addr, path_length) {
            const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
            const path = loadSchemeString(buffer);
            try {
                accessSync(path, constants.F_OK);
                return true;
            } catch (err) {
                return false;
            }
        },

        file_delete: function (path_addr, path_length) {
            const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
            const path = loadSchemeString(buffer);
            try {
                unlinkSync(path);
                return true;
            } catch (err) {
                return false;
            }
        },

        dir_remove: function (path_addr, path_length) {
            const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
            const path = loadSchemeString(buffer);
            try {
                rmdirSync(path);
                return true;
            } catch (err) {
                return false;
            }
        },

        number_to_string: function (x, addr) {
            return storeJSStringToScheme(x.toString(), addr);
        }
    },

    __math: {
        fmod: (x, y) => x % y,
        exp: Math.exp,
        log: Math.log,
        log2: Math.log2,
        log10: Math.log10,
        sin: Math.sin,
        cos: Math.cos,
        tan: Math.tan,
        asin: Math.asin,
        acos: Math.acos,
        atan: Math.atan,
        atan2: Math.atan2,
        pow: Math.pow,
        randomf: Math.random,
    }
});

if (!instance.exports.bigloo_main) {
    console.error("ERROR: missing 'bigloo_main' symbol in WASM module file.");
    exit(1);
}

instance.exports.bigloo_main(null);

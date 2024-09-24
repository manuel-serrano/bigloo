/*=====================================================================*/
/*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/runtime.mjs        */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Sep 24 09:08:45 2024 (serrano)                */
/*    Copyright   :  2024 manuel serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding.                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync, readFileSync, fstatSync } from 'node:fs';
//import { readFile } from 'node:fs/promises';
import { extname } from 'node:path';

/*---------------------------------------------------------------------*/
/*    Minimalist command line parsing                                  */
/*---------------------------------------------------------------------*/
// This code is a bit strange but is required to support Deno and NodeJS.
// If we import 'process' in NodeJS, readSync() will throw the error EAGAIN
// when reading, so we can't import it. However, in Deno, process is not
// a global variable and therefore we need to explicitly import 'process'.
const argv = (globalThis.window && "Deno" in window)
   ? (await import('node:process')).argv
   : process.argv;

if (argv.length < 3) {
    console.error("ERROR: missing input WASM module file.");
    process.exit(1);
} else if (!existsSync(argv[2])) {
    console.error(`ERROR: file '${argv[2]}' doesn't exist.`);
    process.exit(1);
} else if (extname(argv[2]) != ".wasm") {
    console.error(`ERROR: input file '${argv[2]}' is not a WASM module.`);
    process.exit(1);
}

/*---------------------------------------------------------------------*/
/*    currentLocale ...                                                */
/*---------------------------------------------------------------------*/
const currentLocale = globalThis?.navigator.languages[0] || "en-US";

const schemeStringDecoder = new TextDecoder();
const schemeStringEncoder = new TextEncoder();
function loadSchemeString(buffer) { return schemeStringDecoder.decode(buffer); }
function storeJSStringToScheme(string, addr) {
    const memory = new Uint8Array(instance.exports.memory.buffer, addr);
    const bytes = schemeStringEncoder.encode(string);
    memory.set(bytes);
    return bytes.length;
}

/*---------------------------------------------------------------------*/
/*    IO                                                               */
/*---------------------------------------------------------------------*/
const charBuffer = new Uint8Array(1);

/*---------------------------------------------------------------------*/
/*    internalErrors                                                   */
/*---------------------------------------------------------------------*/
const internalErrors = [
   "apply: unsupported arity %d"
];

/*---------------------------------------------------------------------*/
/*    wasm ...                                                         */
/*---------------------------------------------------------------------*/
//const wasm = await WebAssembly.compile(await readFile(argv[2]));
const wasm = await WebAssembly.compile(readFileSync(argv[2]));

const instance = await WebAssembly.instantiate(wasm, {
   __js: {
      not_implemented: x => {
	 console.error("*** WASM WARNING: function not implemented", x);
      },
      
      trace: function (x) {
         console.log("TRACE: " + x);
      },

      internalError: function (errno, val) {
         console.error("*** INTERNAL-ERROR:",
		       format(internalErrors[errno], val));
      },

      argc: argv.length - 2 /* ignore the path of NodeJS and of runtime.mjs. */,

      get_arg: function (idx, addr) {
         let real_idx = idx + 2 /* ignore the path of NodeJS and of runtime.mjs. */;
         let arg = argv[real_idx];
         return storeJSStringToScheme(arg, addr);
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
         if (fd < 0) {
            throw WebAssembly.RuntimeError("invalid file descriptor");
	 }

         const buffer = new Uint8Array(instance.exports.memory.buffer, offset, length);
         writeSync(fd, buffer);
      },
      
      write_char: (fd, c) => {
	 charBuffer[0] = c;
         writeSync(fd, charBuffer);
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

      is_dir: function (path_addr, path_length) {
         const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
         const path = loadSchemeString(buffer);
         try {
            return fstatSync(path).isDirectory();
         } catch (err) {
            return false;
         }
      },

      number_to_string: function (x, addr) {
         return storeJSStringToScheme(x.toString(), addr);
      },

      exit: function (val) {
 	 process.exit(val);
      },

      signal: function (sig, hdl) {
	 // console.log("NOT IMPLEMENTED SIGNAL sig=", sig, "hdl=", hdl);
      }
   },

   __js_date: {
      current_seconds: () => BigInt(Math.trunc(Date.now() / 1000)),
      current_milliseconds: () => BigInt(Math.trunc(Date.now())),
      current_microseconds: () => BigInt(Math.trunc(Date.now() * 1000)),
      current_nanoseconds: () => BigInt(Math.trunc(Date.now() * 1000000)),

      mktime: (year, month, day, hour, minute, second, millisecond) => (new Date(year, month, day, hour, minute, second, millisecond)).getTime(),
      mktimegm: (year, month, day, hour, minute, second, millisecond) => (Date.UTC(year, month, day, hour, minute, second, millisecond)),

      day_name: (day, longFormat, addr) =>
         storeJSStringToScheme((new Date(Date.UTC(2021, 1, day + 1)))
				  .toLocaleDateString(currentLocale, {
				     weekday: (longFormat ? "long" : "short")
				  }), addr),

      month_name: (month, longFormat, addr) =>
         storeJSStringToScheme((new Date(Date.UTC(2021, month)))
				  .toLocaleDateString(currentLocale, {
				     month: (longFormat ? "long" : "short")
				  }), addr)
   },

   __js_math: {
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

/*    __js_bignum: {                                                   */
/*       long_to_bignum: (value) => {                                  */
/* 	 return BigInt(value);                                         */
/*       }                                                             */
/*    }                                                                */
});

if (!instance.exports.bigloo_main) {
   console.error("ERROR: missing 'bigloo_main' symbol in WASM module file.");
    process.exit(1);
}

if (!instance.exports.__js_bigloo_main) {
   console.error("ERROR: missing '__js_bigloo_main' symbol in WASM module file.");
   process.exit(1);
}

// Call the Bigloo Scheme program!
instance.exports.__js_bigloo_main();

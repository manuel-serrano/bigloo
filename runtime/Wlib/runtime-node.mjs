/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/runtime-node.mjs       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Jun  3 08:36:28 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding, node specific                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync, readFileSync, fstatSync, lstatSync, mkdirSync, readdirSync } from "node:fs";
import { isatty } from "tty";
import { extname, sep as file_sep } from "node:path";
import { format } from "node:util";
import { __js_unicode, __js_bignum, __js_math, __js_date} from "./runtime-common.mjs";

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

const ucs2StringDecoder = new TextDecoder("ucs-2");
const ucs2StringEncoder = new TextEncoder("ucs-2");

function loadSchemeString(buffer) {
   return schemeStringDecoder.decode(buffer);
}

function loadUCS2String(buffer) {
   return ucs2StringDecoder.decode(buffer);
}

function storeJSStringToScheme(string, addr) {
    const memory = new Uint8Array(instance.exports.memory.buffer, addr);
    const bytes = schemeStringEncoder.encode(string);
    memory.set(bytes);
    return bytes.length;
}

/*---------------------------------------------------------------------*/
/*    string_to_bignum_radix ...                                       */
/*---------------------------------------------------------------------*/
function string_to_bignum_radix(str, radix) {
   let n = 0n;
   const kr = BigInt(radix);
   const l = str.length;
   
   for (let i = 0; i < l; i++) {
      const m = str.charCodeAt(i) - '0'.charCodeAt(0);
      n = (n * kr) + BigInt(m);
   }

   return n;
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
/*    Random                                                           */
/*---------------------------------------------------------------------*/
let S = false;
const M = 2147483648n;
const A = 1103515245n;
const C = 12345n;
   
function seedRandom(seed) {
   S = BigInt(seed) % M;
   return S;
}

function randBignum(bx) {
   if (!S) {
      return bx ^ BigInt(Math.random() * 5379239846);
   } else {
      S = (S * A + C);
      return S % bx;
   }
}

function randFixnum() {
   if (!S) {
      return Math.round(Math.random() * ((1 << 31) - 1));
   } else {
      S = (S * A + C) % M;
      return Number(S);
   }
}


/*---------------------------------------------------------------------*/
/*    __js_system                                                      */
/*---------------------------------------------------------------------*/
const __js_system = {
   command_line_size: () => process.argv.length,
   command_line_entry: (num, addr) => storeJSStringToScheme(process.argv[num], addr),
   executable_name: (addr) => storeJSStringToScheme(process.argv[0], addr)
}

/*---------------------------------------------------------------------*/
/*    __js_io ...                                                      */
/*---------------------------------------------------------------------*/
const __js_io = {
   open_file: (path_addr, path_length, flags) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);

      let fs_flags;
      switch (flags) {
         case 0: // read-only
            fs_flags = 'r';
            break;
         case 1: // write-only
            fs_flags = 'w';
            break;
         case 2: // write-only in append mode
            fs_flags = 'r+';
            break;
         default:
            throw WebAssembly.RuntimeError("invalid open flags");
      }
      try {
         return openSync(path, fs_flags);
      } catch(e) {
	 return -1;
      }
   },
   
   close_file: (fd) => {
      closeSync(fd);
   },

   read_file: (fd, offset, length, position) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      const memory = new Uint8Array(instance.exports.memory.buffer, offset, length, position);
      const nbread = readSync(fd, memory, 0, length, position);

      return nbread;
   },

   path_size: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 return lstatSync(path).size;
      } catch (err) {
         return -1;
      }
   },

   last_modification_time: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 return lstatSync(path).mtime;
      } catch (err) {
         return -1;
      }
   },

   file_size: (fd) => {
      try {
	 return fstatSync(fd).size;
      } catch (err) {
         return -1;
      }
   },

   isatty: (fd) => {
      return isatty(fd);
   },
   
   file_exists: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
         accessSync(path, constants.F_OK);
         return true;
      } catch (err) {
         return false;
      }
   },

   file_delete: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 if (existsSync(path)) {
            unlinkSync(path);
            return false;
	 } else {
	    return true;
	 }
      } catch (err) {
         return true;
      }
   },

   dir_remove: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
         rmdirSync(path);
         return false;
      } catch (err) {
         return true;
      }
   },

   is_dir: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);

      try {
         return lstatSync(path).isDirectory();
      } catch (err) {
         return false;
      }
   },

   make_dir: (path_addr, path_length, mod) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 mkdirSync(path, {mod: mod});
	 return true;
      } catch(e) {
	 return false;
      }
   },

   append_file: (fd, offset, length) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }

      const buffer = new Uint8Array(instance.exports.memory.buffer, offset, length);
      return writeSync(fd, buffer, 0, length);
   },
   
   write_file: (fd, offset, length, position) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }

      const buffer = new Uint8Array(instance.exports.memory.buffer, offset, length);
      return writeSync(fd, buffer, 0, length, position);
   },
      
   append_char: (fd, c) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      charBuffer[0] = c;
      return writeSync(fd, charBuffer, 0, 1);
   },

   write_char: (fd, c, position) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      charBuffer[0] = c;
      return writeSync(fd, charBuffer, 0, 1, position);
   },

   write_bignum: (fd, n) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      return writeSync(fd, n.toString());
   },

   read_dir_init: (path_addr, path_length) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 return readdirSync(path);
      } catch(e) {
	 return null;
      }
   },
   read_dir_size: (dir) => dir.length,
   read_dir_entry: (dir, num, addr) => {
      return storeJSStringToScheme(dir[num], addr);
   },

   mmap_init: (path_addr, path_length, read, write) => {
      const buffer = new Uint8Array(instance.exports.memory.buffer, path_addr, path_length);
      const path = loadSchemeString(buffer);
      try {
	 return readFile(path);
      } catch(e) {
	 return null;
      }
   },
}

/*---------------------------------------------------------------------*/
/*    wasm ...                                                         */
/*---------------------------------------------------------------------*/
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
         console.error("*** INTERNAL-ERROR(" + errno +"):",
		       format(internalErrors[errno], val));
      },

      getcwd: (addr) => {
	 const s = process.cwd();
	 storeJSStringToScheme(s, addr);
	 return s.length;
      },

      getenv: (addr, len) => {
         const buffer = new Uint8Array(instance.exports.memory.buffer, addr, len);
         const v = loadSchemeString(buffer);

	 if (v in process.env) {
	    storeJSStringToScheme(process.env[v], addr);
	    return process.env[v].length;
	 } else {
	    return -1;
	 } 
      },

      file_separator: file_sep.charCodeAt(0),
      
      argc: argv.length - 2 /* ignore the path of NodeJS and of runtime.mjs. */,

      get_arg: function (idx, addr) {
         let real_idx = idx + 2 /* ignore the path of NodeJS and of runtime.mjs. */;
         let arg = argv[real_idx];
         return storeJSStringToScheme(arg, addr);
      },

      number_to_string: (x, addr) => {
         return storeJSStringToScheme(x.toString(), addr);
      },

      exit: function (val) {
 	 process.exit(val);
      },

      signal: function (sig, hdl) {
	 // console.log("NOT IMPLEMENTED SIGNAL sig=", sig, "hdl=", hdl);
      }
   },

   __js_io,
   __js_system,
   __js_unicode,
   __js_bignum,
   __js_math,
   __js_date
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

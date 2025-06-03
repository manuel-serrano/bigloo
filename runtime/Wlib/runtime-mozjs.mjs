/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/runtime-mozjs.mjs      */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Jun  3 08:36:53 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding.                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { __js_unicode, __js_bignum, __js_math, __js_date} from "./runtime-common.mjs";

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
const process = {
   argv: ["js", "rts.mjs", "a.out.wasm"].concat(scriptArgs),
   env: { HOME: "/home/serrano" },
   exit: n => quit(n > 127 ? 1 : n)
}

function readFileSync(o) {
   return os.file.readFile(o, "binary");
}

function writeSync(fd, buffer, offset, length) {
   if (fd === 1) {
      const buf = buffer.subarray(offset, length);
      print(Array.from(buf, byte => String.fromCharCode(byte)).join(''));
   }
}

let instance;

/*---------------------------------------------------------------------*/
/*    Minimalist command line parsing                                  */
/*---------------------------------------------------------------------*/
// This code is a bit strange but is required to support Deno and NodeJS.
// If we import 'process' in NodeJS, readSync() will throw the error EAGAIN
// when reading, so we can't import it. However, in Deno, process is not
// a global variable and therefore we need to explicitly import 'process'.

/*---------------------------------------------------------------------*/
/*    currentLocale ...                                                */
/*---------------------------------------------------------------------*/
const currentLocale = "en-US";

/* const schemeStringDecoder = new TextDecoder();                      */
/* const schemeStringEncoder = new TextEncoder();                      */
/*                                                                     */
/* const ucs2StringDecoder = new TextDecoder("ucs-2");                 */
/* const ucs2StringEncoder = new TextEncoder("ucs-2");                 */
/*                                                                     */
/* function loadSchemeString(buffer) {                                 */
/*    return schemeStringDecoder.decode(buffer);                       */
/* }                                                                   */
/*                                                                     */
/* function loadUCS2String(buffer) {                                   */
/*    return ucs2StringDecoder.decode(buffer);                         */
/* }                                                                   */
/*                                                                     */
/* function storeJSStringToScheme(string, addr) {                      */
/*     const memory = new Uint8Array(instance.exports.memory.buffer, addr); */
/*     const bytes = schemeStringEncoder.encode(string);               */
/*     memory.set(bytes);                                              */
/*     return bytes.length;                                            */
/* }                                                                   */

function loadSchemeString(buffer) {
   Array.from(buffer, byte => String.fromCharCode(byte)).join('');
}

function loadUCS2String(buffer) {
   Array.from(buffer, byte => String.fromCharCode(byte)).join('');
}

function storeJSStringToScheme(string, addr) {
   const memory = new Uint8Array(instance.exports.memory.buffer, addr);
   const bytes = string;
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
      return false;
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
      print("ICI");
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
//const wasm = await WebAssembly.compile(await readFile(argv[2]));
async function run(argv) {
   const bytes = readFileSync(argv[2]);
   const wasm = new WebAssembly.Module(bytes);

   instance = new  WebAssembly.Instance(wasm, {
      __js: {
	 glup: () => { console.log("in glup"); return Date.now()},
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

	 file_separator: 47,
	 
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
}

try {
   run(process.argv);
} catch(e) {
   print("*** ERROR", e);
}

// time /usr/lib/x86_64-linux-gnu/mozjs-128/js128 -P wasm_gc -P wasm_exnref rts.mjs

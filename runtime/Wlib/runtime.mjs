/*=====================================================================*/
/*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/runtime.mjs        */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Wed Jan  8 06:55:37 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding.                                  */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync, readFileSync, fstatSync, lstatSync, mkdirSync, readdirSync } from "node:fs";
import { isatty } from "tty";
//import { readFile } from 'node:fs/promises';
import { extname, sep as file_sep } from "node:path";
import { format } from "node:util";

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
            fs_flags = 'a';
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

   write_file: (fd, offset, length) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }

      const buffer = new Uint8Array(instance.exports.memory.buffer, offset, length);
      return writeSync(fd, buffer);
   },
      
   write_char: (fd, c) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      charBuffer[0] = c;
      writeSync(fd, charBuffer);
   },

   write_bignum: (fd, n) => {
      if (fd < 0) {
         throw WebAssembly.RuntimeError("invalid file descriptor");
      }
      writeSync(fd, n.toString());
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
   }
}

/*---------------------------------------------------------------------*/
/*    __js_math ...                                                    */
/*---------------------------------------------------------------------*/
const __js_math = {
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
      strtod: (addr, len) => {
         const buffer = new Uint8Array(instance.exports.memory.buffer, addr, len);
	 return Number.parseFloat(loadSchemeString(buffer));
      }
};

/*---------------------------------------------------------------------*/
/*    __js_date ...                                                    */
/*---------------------------------------------------------------------*/
const __js_date = {
   epoch: new Date(1970),
   current_milliseconds: () => Date.now(),
   mkDate: (ms) => new Date(ms),
   mktime: (year, month, day, hour, minute, second, millisecond, gmt) => gmt
      ? (new Date(Date.UTC(year, month, day, hour, minute, second, millisecond)))
      : (new Date(year, month, day, hour, minute, second, millisecond)),

   getMilliseconds: (dt) => dt.getMilliseconds(),
   setMilliseconds: (dt, ms) => dt.setMilliseconds(ms),
   getSeconds: (dt) => dt.getSeconds(),
   setSeconds: (dt, sec) => dt.setSeconds(sec),
   getMinutes: (dt) => dt.getMinutes(),
   setMinutes: (dt, min) => dt.setMinutes(min),
   getHours: (dt) => dt.getHours(),
   setHours: (dt, h) => dt.setHours(h),
   getDay: (dt) => dt.getDate(),
   setDay: (dt,) => dt.setDate(d),
   getWday: (dt) => dt.getDay(),
   getYday: (dt) => {
      const y = dt.getFullYear();
      const m = dt.getMonth();
      const d = dt.getDate();
      const d1 = new Date(y, m, d);
      const d0 = new Date(y, 0, 1);
      return Math.trunc((d1.valueOf() - d0.valueOf()) / (24 * 60 * 60 * 60 * 1000));
   },
   getMonth: (dt) => dt.getMonth(),
   setMonth: (dt, m) => dt.setMonth(m),
   getYear: (dt) => dt.getFullYear(),
   setYear: (dt, y) => dt.setFullYear(y),
   getTimezone: (dt) => dt.getTimezoneOffset(),

   isDst: (dt) => new Date(dt.valueOf()) !== dt.valueOf(), // MS 18dec2024, not sure!
   getTime: (dt) => Math.trunc(dt.valueOf() / 1000),
   secondsToString: (sec, addr) => {
      const buf = new Date(sec * 1000).toString();

      storeJSStringToScheme(buf, addr);
      return buf.length;
   },
   secondsToUTCString: (sec, addr) => {
      const buf = new Date(sec * 1000).toUTCString();

      storeJSStringToScheme(buf, addr);
      return buf.length;
   },
   

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
};

/*---------------------------------------------------------------------*/
/*    __js_bignum                                                      */
/*---------------------------------------------------------------------*/
const __js_bignum = {
   zerobx: BigInt(0),
   zerobxp: (bx) => bx === 0n,
   bxpositivep: (bx) => bx > 0n,
   bxnegativep: (bx) => bx < 0n,
   bgl_bignum_odd: (bx) => bx % 2n !== 0n,
   bgl_bignum_even: (bx) => bx % 2n === 0n,
   long_to_bignum: (value) => BigInt(value),
   safe_bignum_to_fixnum: (bx, bsz) => {
      if (bsz > 53) bsz = 52; // max support JS fixnums
      const u = BigInt.asIntN(bsz, bx);
      if (u === bx) {
	 const m = new Number(u);

	 if (m >= Number.MIN_SAFE_INTEGER && m <= Number.MAX_SAFE_INTEGER) {
	    return m;
	 } else {
	    return 0;
	 }
      } else {
	 return 0;
      }
   },
   bignum_to_long: bx => BigInt.asIntN(64, bx),
   bignum_remainder: (bx, by) => bx % by,
   bignum_quotient: (bx, by) => bx / by,
   seed_rand: () => Math.random(),
   rand_bignum: bx => bx ^ BigInt(Math.random() * 5379239846),
   bignum_to_string: (value, addr) => {
      return storeJSStringToScheme(value.toString(), addr);
   },
   string_to_bignum: (offset, len, radix) => {
      const buf = new Uint8Array(instance.exports.memory.buffer, offset, len);
      const str = loadSchemeString(buf);
      switch(radix) {
	 case 2: return string_to_bignum_radix(str, 2);
	 case 8: return string_to_bignum_radix(str, 8);
	 case 10: return BigInt(str);
	 case 16: {
	    if (str[0] === '-') {
	       return 0n - BigInt("0x" + str.substring(1));
	    } else {
	       return BigInt("0x" + str);
	    }
	 }
	 default: 
	    console.log("Wong bignum radix", radix);
	    return BigInt(0);
      }
   },
   bignum_neg: (x) => -x,
   bignum_add: (x, y) => x + y,
   bignum_sub: (x, y) => x - y,
   bignum_mul: (x, y) => x * y,
   bignum_quotient: (x, y) => x / y,
   bignum_remainder: (x, y) => x % y,
   bignum_cmp: (x, y) => x < y ? -1 : (x > y ? 1 : 0),
   bignum_to_flonum: x => Number(x)
};

/*---------------------------------------------------------------------*/
/*    wasm ...                                                         */
/*---------------------------------------------------------------------*/
//const wasm = await WebAssembly.compile(await readFile(argv[2]));
const wasm = await WebAssembly.compile(readFileSync(argv[2]));

const instance = await WebAssembly.instantiate(wasm, {
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
   __js_date,
   __js_math,
   __js_bignum,
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

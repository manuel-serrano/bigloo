/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/runtime-node.mjs       */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Jul 15 10:11:55 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding, node specific                    */
/*=====================================================================*/

/*---------------------------------------------------------------------*/
/*    Imports                                                          */
/*---------------------------------------------------------------------*/
import { accessSync, closeSync, constants, existsSync, fstat, openSync, readSync, rmdirSync, unlinkSync, writeSync, readFileSync, fstatSync, lstatSync, mkdirSync, readdirSync } from "node:fs";
import { isatty } from "node:tty";
import { extname, sep as file_sep } from "node:path";
import { format } from "node:util";

/*---------------------------------------------------------------------*/
/*    Wasm instances                                                   */
/*---------------------------------------------------------------------*/
let client, rts;

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

if (argv[2] === "-s") {
   rts = argv[3];
   argv.splice(1, 2);
}

if (argv.length < 3) {
   console.error("ERROR: missing input WASM module file.");
   process.exit(1);
} else if (!existsSync(argv[2])) {
    console.error(`ERROR: file '${argv[2]}' doesn't exist.`);
    process.exit(1);
} else if (extname(argv[2]) != ".wasm") {
    console.error(`ERROR: input file '${argv[2]}' is not a WASM module.`);
    process.exit(1);
} else {
   client = argv[2];
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

function storeJSStringToScheme(instance, string, addr) {
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
/*    __js_unicode ...                                                 */
/*---------------------------------------------------------------------*/
function __js_unicode() {
   return {
      ucs2_toupper: (n) => String.fromCharCode(n).toUpperCase().charCodeAt(0),
      ucs2_tolower: (n) => String.fromCharCode(n).toLowerCase().charCodeAt(0),
      ucs2_upperp: (n) => String.fromCharCode(n).toUpperCase().charCodeAt(0) === n,
      ucs2_lowerp: (n) => String.fromCharCode(n).toLowerCase().charCodeAt(0) === n,
      ucs2_letterp: (n) => {
	 const s = String.fromCharCode(n);
	 return /^\S$/.test(s) && /^\D$/.test(s);
      },
      ucs2_digitp: (n) => /^\d$/.test(String.fromCharCode(n)),
      ucs2_whitespacep: (n) => /^\s$/.test(String.fromCharCode(n)),
      ucs2_definedp: (n) => {
	 try {
	    String.fromCharCode(n).toLowerCase().charCodeAt(0);
	    return 1;
	 } catch(e) {
	    return 0;
	 }
      }
   };
}

/*---------------------------------------------------------------------*/
/*    __js_math ...                                                    */
/*---------------------------------------------------------------------*/
function __js_math() {
   const self = {
      instance: undefined,
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
         const buffer = new Uint8Array(self.instance.exports.memory.buffer, addr, len);
	 return Number.parseFloat(loadSchemeString(buffer));
      }
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js_date ...                                                    */
/*---------------------------------------------------------------------*/
function __js_date() {
   const self = {
      instance: undefined,
      epoch: new Date(1970),
      current_milliseconds: () => Date.now(),
      mkDate: (ms) => new Date(ms),
      mktime: (year, month, day, hour, minute, second, millisecond, gmt) => {
	 if (gmt) {
	    return new Date(Date.UTC(year, month - 1, day, hour, minute, second, millisecond));
	 } else {
	    return new Date(year, month - 1, day, hour, minute, second, millisecond);
	 }
      },

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
      getWday: (dt) => dt.getDay() + 1,
      getYday: (dt) => {
	 const y = dt.getFullYear();
	 const m = dt.getMonth();
	 const d = dt.getDate();
	 const d1 = new Date(y, m, d);
	 const d0 = new Date(y, 0, 1);
	 return Math.trunc((d1.valueOf() - d0.valueOf()) / (24 * 60 * 60 * 60 * 1000));
      },
      getMonth: (dt) => dt.getMonth() + 1,
      setMonth: (dt, m) => dt.setMonth(m),
      getYear: (dt) => dt.getFullYear(),
      setYear: (dt, y) => dt.setFullYear(y),
      getTimezone: (dt) => dt.getTimezoneOffset() * 60,

      isDst: (dt) => new Date(dt.valueOf()) !== dt.valueOf(), // MS 18dec2024, not sure!
      getTime: (dt) => dt.valueOf(),
      secondsToString: (sec, addr) => {
	 const buf = new Date(sec * 1000).toString();

	 storeJSStringToScheme(self.instance, buf, addr);
	 return buf.length;
      },
      secondsToUTCString: (sec, addr) => {
	 const buf = new Date(sec * 1000).toUTCString();

	 storeJSStringToScheme(self.instance, buf, addr);
	 return buf.length;
      },
      

      day_name: (day, longFormat, addr) =>
	 storeJSStringToScheme(self.instance, (new Date(Date.UTC(2021, 1, day + 1)))
					    .toLocaleDateString(currentLocale, {
					       weekday: (longFormat ? "long" : "short")
					    }), addr),

      month_name: (month, longFormat, addr) =>
	 storeJSStringToScheme(self.instance, (new Date(Date.UTC(2021, month)))
					    .toLocaleDateString(currentLocale, {
					       month: (longFormat ? "long" : "short")
					    }), addr)
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js_bignum                                                      */
/*---------------------------------------------------------------------*/
function __js_bignum() {
   const self = {
      instance: undefined,
      zerobx: BigInt(0),
      zerobxp: (bx) => bx === 0n,
      bxpositivep: (bx) => bx > 0n,
      bxnegativep: (bx) => bx < 0n,
      bignum_odd: (bx) => bx % 2n !== 0n,
      bignum_even: (bx) => bx % 2n === 0n,
      double_to_bignum: (value) => BigInt(value),
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
      seed_rand: seedRandom,
      rand_bignum: randBignum,
      rand_fixnum: randFixnum,
      bignum_to_string: (value, addr) => {
	 return storeJSStringToScheme(self.instance, value.toString(), addr);
      },
      string_to_bignum: (offset, len, radix) => {
	 const buf = new Uint8Array(self.instance.exports.memory.buffer, offset, len);
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
      bignum_abs: (x) => x < 0 ? -x : x,
      bignum_gcd: (x, y) => !y ? x : self.gcd(x % y),
      bignum_lcm: (x, y) => (x, y) => x * y / self.gcd(x, y),
      bignum_neg: (x) => -x,
      bignum_add: (x, y) => x + y,
      bignum_sub: (x, y) => x - y,
      bignum_mul: (x, y) => x * y,
      bignum_quotient: (x, y) => x / y,
      bignum_remainder: (x, y) => x % y,
      bignum_xor: (x, y) => x ^ y,
      bignum_and: (x, y) => x & y,
      bignum_or: (x, y) => x | y,
      bignum_not: (x, y) => ~x,
      bignum_lsh: (x, y) => x << y,
      bignum_rsh: (x, y) => x >> y,
      bignum_mask: (x, y) => x & BigInt((1 << y) - 1),
      bignum_cmp: (x, y) => x < y ? -1 : (x > y ? 1 : 0),
      bignum_to_flonum: x => Number(x)
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js_system                                                      */
/*---------------------------------------------------------------------*/
function __js_system() {
   const self = {
      self: undefined,
      
      argc: argv.length - 2,
      
      command_line_size: () => process.argv.length,
      
      command_line_entry: (num, addr) => storeJSStringToScheme(self.instance, process.argv[num], addr),
      
      executable_name: (addr) => storeJSStringToScheme(self.instance, process.argv[0], addr),
      
      get_arg: (idx, addr) => {
	 let real_idx = idx + 2;
	 let arg = argv[real_idx];
	 return storeJSStringToScheme(self.instance, arg, addr);
      },
      
      getenv: (addr, len) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, addr, len);
	 const v = loadSchemeString(buffer);

	 if (v in process.env) {
	    storeJSStringToScheme(self.instance, process.env[v], addr);
	    return process.env[v].length;
	 } else {
	    return -1;
	 } 
      },
      
      getcwd: (addr) => {
	 const s = process.cwd();
	 storeJSStringToScheme(self.instance, s, addr);
	 return s.length;
      },

      exit: function (val) {
	 process.exit(val);
      },

      signal: function (sig, hdl) {
	 // console.log("NOT IMPLEMENTED SIGNAL sig=", sig, "hdl=", hdl);
      }
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js_io ...                                                      */
/*---------------------------------------------------------------------*/
function __js_io() {
   const self = {
      self: undefined,
      file_separator: file_sep.charCodeAt(0),
      
      open_file: (path_addr, path_length, flags) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
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

      open_fd: (fd, flags, addr) => {
	 const buf = fd.toString();
	 storeJSStringToScheme(self.instance, buf, addr);
	 return buf.length;
      },
      
      close_file: (fd) => {
	 closeSync(fd);
      },

      read_file: (fd, offset, length, position) => {
	 if (fd < 0) {
            throw WebAssembly.RuntimeError("invalid file descriptor");
	 }
	 const memory = new Uint8Array(self.instance.exports.memory.buffer, offset, length, position);
	 const nbread = readSync(fd, memory, 0, length, position);

	 return nbread;
      },

      password: (prompt_addr, prompt_length, res_addr) => {
	 const memory = new Uint8Array(self.instance.exports.memory.buffer, offset, length, position);
	 const buf = "toto";

	 storeJSStringToScheme(self.instance, buf, addr);
	 return buf.length;
      },
      
      path_size: (path_addr, path_length) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);
	 try {
	    return lstatSync(path).size;
	 } catch (err) {
            return -1;
	 }
      },

      last_modification_time: (path_addr, path_length) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
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
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);
	 try {
            accessSync(path, constants.F_OK);
            return true;
	 } catch (err) {
            return false;
	 }
      },

      file_delete: (path_addr, path_length) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
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
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);
	 try {
            rmdirSync(path);
            return false;
	 } catch (err) {
            return true;
	 }
      },

      is_dir: (path_addr, path_length) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);

	 try {
            return lstatSync(path).isDirectory();
	 } catch (err) {
            return false;
	 }
      },

      make_dir: (path_addr, path_length, mod) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
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

	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, offset, length);
	 return writeSync(fd, buffer, 0, length);
      },
      
      write_file: (fd, offset, length, position) => {
	 if (fd < 0) {
            throw WebAssembly.RuntimeError("invalid file descriptor");
	 }

	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, offset, length);
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
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);
	 try {
	    return readdirSync(path);
	 } catch(e) {
	    return null;
	 }
      },
      read_dir_size: (dir) => dir.length,
      read_dir_entry: (dir, num, addr) => {
	 return storeJSStringToScheme(self.instance, dir[num], addr);
      },

      mmap_init: (path_addr, path_length, read, write) => {
	 const buffer = new Uint8Array(self.instance.exports.memory.buffer, path_addr, path_length);
	 const path = loadSchemeString(buffer);
	 try {
	    return readFile(path);
	 } catch(e) {
	    return null;
	 }
      },
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js ...                                                         */
/*---------------------------------------------------------------------*/
function __js() {
   const self = {
      self: undefined,

      $bigloo_main: undefined,
      
      $__main: (argv) => self.$bigloo_main(argv),
      
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

      number_to_string: (x, addr) => {
	 return storeJSStringToScheme(self.instance, x.toString(), addr);
      }
   };
   return self;
}

/*---------------------------------------------------------------------*/
/*    __js_all ...                                                     */
/*---------------------------------------------------------------------*/
function __js_all() {
   return {
      __js: __js(),
      __js_io: __js_io(),
      __js_system: __js_system(),
      __js_unicode: __js_unicode(),
      __js_bignum: __js_bignum(),
      __js_math: __js_math(),
      __js_date: __js_date()
   };
}

/*---------------------------------------------------------------------*/
/*    __js_link_instance ...                                           */
/*---------------------------------------------------------------------*/
function __js_link_instance(__js, instance, client) {
   // give access to "instance" to all JS functions
   for (let k in __js) {
      if (Object.isExtensible(__js[k])) {
	 __js[k].instance = instance;
      }
   }
   // link with the program entry point
   __js.__js.$bigloo_main = client.exports.bigloo_main;
}

/*---------------------------------------------------------------------*/
/*    runSingle ...                                                    */
/*    -------------------------------------------------------------    */
/*    Run a whole wasm program in a single self.instance.              */
/*---------------------------------------------------------------------*/
async function runSingle(client) {
   const __js = __js_all();
   const wasmClient = await WebAssembly.compile(readFileSync(client));
   const instanceClient = await WebAssembly.instantiate(wasmClient, __js);
   
   __js_link_instance(__js, instanceClient, instanceClient);

   if (!instanceClient.exports.bigloo_main) {
      console.error(`*** ERROR: missing 'bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   if (!instanceClient.exports.__bigloo_main) {
      console.error(`*** ERROR: missing '__bigloo_main' export in "${client}".`);
      process.exit(1);
   }

   instanceClient.exports.__bigloo_main();
}

/*---------------------------------------------------------------------*/
/*    runDouble ...                                                    */
/*    -------------------------------------------------------------    */
/*    Run a wasm in two instances, one for client, one the runtime.    */
/*---------------------------------------------------------------------*/
async function runDouble(client, rts) {
   const __jsClient = __js_all();
   const __jsRts = __js_all();
   const wasmRts = await WebAssembly.compile(readFileSync(rts));
   const wasmClient = await WebAssembly.compile(readFileSync(client));

   const instanceRts = await WebAssembly.instantiate(wasmRts, __jsRts);
   __jsClient.__bigloo = instanceRts.exports;

   const instanceClient = await WebAssembly.instantiate(wasmClient, __jsClient);

   __js_link_instance(__jsClient, instanceClient, instanceClient);
   __js_link_instance(__jsRts, instanceRts, instanceClient);
   
   if (!instanceClient.exports.bigloo_main) {
      console.error(`*** ERROR: missing 'bigloo_main' export in "${client}".`);
      process.exit(1);
}

   if (!instanceRts.exports.__bigloo_main) {
      console.error(`*** ERROR: missing '__bigloo_main' export in "${rts}".`);
      process.exit(1);
   }

   instanceRts.exports.__bigloo_main();
}

/*---------------------------------------------------------------------*/
/*    top-level                                                        */
/*---------------------------------------------------------------------*/
try {
   if (rts) {
      runDouble(client, rts);
   } else {
      runSingle(client);
   }
} catch(e) {
   console.error("*** ERROR", e);
}


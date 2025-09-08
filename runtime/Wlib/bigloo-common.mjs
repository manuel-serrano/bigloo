/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/bigloo-common.mjs      */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Fri Sep  5 09:06:38 2025                          */
/*    Last change :  Mon Sep  8 15:31:02 2025 (serrano)                */
/*    Copyright   :  2025 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Bigloo WASM/JS runtime system, common to all JS engines.         */
/*=====================================================================*/

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
/*    IO                                                               */
/*---------------------------------------------------------------------*/
const charBuffer = new Uint8Array(1);

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
/*    exports                                                          */
/*---------------------------------------------------------------------*/
export class BglRuntime {
   self;
   instance;
   memory;
   bigloo_main;
   locale;

   __js;
   __js_io;
   __js_socket;
   __js_process;
   __js_system;
   __js_unicode;
   __js_bignum;
   __js_math;
   __js_date;

   constructor() {
      this.__js = this.js(this);
      this.__js_io = this.js_io(this);
      this.__js_socket = this.js_socket(this);
      this.__js_process = this.js_process(this);
      this.__js_system = this.js_system(this);
      this.__js_unicode = this.js_unicode(this);
      this.__js_bignum = this.js_bignum(this);
      this.__js_math = this.js_math(this);
      this.__js_date = this.js_date(this);
      this.locale = this.getCurrentLocale();
   }

   link(instance, client = null) {
      this.instance = instance;
      this.memory = instance.exports.memory;
      if (client) this.bigloo_main = client.exports.bigloo_main;
   }

   notImplemented(fun) {
      console.error("*** BIGLOO WASM WARNING: function not implemented", fun);
      throw new Error(`***BIGLOO ERROR: ${fun}: not implemented`);
   }

   unsupported(x) {
      console.error("*** BIGLOO WASM WARNING: function unsupported", x);
   }

   getCurrentLocale() {
      return "en-US";
   }
   
   stringEncode(string) {
      return this.notImplemented("stringEncode");
   }

   stringDecode(buffer) {
      return this.notImplemented("stringDecode");
   }

   loadString(addr, len) {
      const buffer = new Uint8Array(this.memory.buffer, addr, len);
      return this.stringDecode(buffer);
   }

   loadStringLen2(addr) {
      const lenbuf = new Uint8Array(this.memory.buffer, addr, 2);
      const len = lenbuf[0] * 256 + lenbuf[1];
      const buffer = new Uint8Array(this.memory.buffer, addr + 2, len);

      return { addr: addr + 2 + len, str: this.stringDecode(buffer) };
   }
   
   storeString(string, addr) {
      const memory = new Uint8Array(this.memory.buffer, addr);
      const bytes = this.stringEncode(string);
      memory.set(bytes);
      return bytes.length;
   }
   
   js(self) {
      return {
	 $__main: (argv) => self.bigloo_main(argv),
	 
	 not_implemented: x => {
	    console.error("*** WASM WARNING: function not implemented", x);
	 },
      
	 unsupported: x => {
	    console.error("*** WASM WARNING: function unsupported", x);
	 },
      
	 trace: (x) => {
	    console.log("TRACE: " + x);
	 },

	 internalError: (errno, val) => {
	    console.error("*** INTERNAL-ERROR(" + errno +"):",
			  format(internalErrors[errno], val));
	 },

	 performanceNow: () => performance.now(),
      }
   }

   js_io(self) {
      return self.notImplemented("js_io");
   }

   
   js_socket(self) {
      return self.notImplemented("js_socket");
   }

   js_system(self) {
      return self.notImplemented("js_system");
   }

   js_unicode(self) {
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

   js_bignum(self) {
      return {
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
	    return self.storeString(value.toString(), addr);
	 },
	 string_to_bignum: (offset, len, radix) => {
	    const str = self.loadString(offset, len);
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
   }

   js_math(self) {
      return {
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
	    return Number.parseFloat(self.loadString(addr, len));
	 }
      };
   }

   js_date(self) {
      return {
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

	 isDst: (dt) => new Date(dt.valueOf()) !== dt.valueOf(),
	 getTime: (dt) => dt.valueOf(),
	 secondsToString: (sec, addr) => {
	    const buf = new Date(sec * 1000).toString();

	    self.storeString(buf, addr);
	    return buf.length;
	 },
	 secondsToUTCString: (sec, addr) => {
	    const buf = new Date(sec * 1000).toUTCString();

	    self.storeString(buf, addr);
	    return buf.length;
	 },
	 

	 day_name: (day, longFormat, addr) =>
	    self.storeString(
	       new Date(Date.UTC(2021, 1, day + 1))
		  .toLocaleDateString(
		     self.locale, {
			weekday: (longFormat ? "long" : "short")
		     }), addr),

	 month_name: (month, longFormat, addr) =>
	    self.storeString(
	       new Date(Date.UTC(2021, month))
		  .toLocaleDateString(
		     self.locale, {
			month: (longFormat ? "long" : "short")
		     }), addr)
      };
   }

}

/*=====================================================================*/
/*    .../prgm/project/bigloo/wasm/runtime/Wlib/runtime-common.mjs     */
/*    -------------------------------------------------------------    */
/*    Author      :  manuel serrano                                    */
/*    Creation    :  Wed Sep  4 06:42:43 2024                          */
/*    Last change :  Tue Jun  3 08:28:12 2025 (serrano)                */
/*    Copyright   :  2024-25 manuel serrano                            */
/*    -------------------------------------------------------------    */
/*    Bigloo-wasm JavaScript binding common.                           */
/*=====================================================================*/

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
export const __js_unicode = {
   // character functions
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
}

/*---------------------------------------------------------------------*/
/*    __js_math ...                                                    */
/*---------------------------------------------------------------------*/
export const __js_math = {
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
export const __js_date = {
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
export const __js_bignum = {
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
   seed_rand: seedRandom,
   rand_bignum: randBignum,
   rand_fixnum: randFixnum,
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


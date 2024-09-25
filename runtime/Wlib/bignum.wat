;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/bignum.wat         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 12:51:44 2024                          */
;*    Last change :  Wed Sep 25 18:17:00 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript bignum implementation                                 */
;*=====================================================================*/

(module $__runtime_bignum
   
   (import "__js_bignum" "zerobx" (global $zerobx externref))
   (import "__js_bignum" "long_to_bignum" (func $long_to_bignum (param i64) (result externref)))
   (import "__js_bignum" "string_to_bignum" (func $string_to_bignum (param i32 i32 i32 i32) (result externref)))
   (import "__js_bignum" "bignum_add" (func $bignum_add (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_sub" (func $bignum_sub (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_mul" (func $bignum_mul (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_cmp" (func $bignum_cmp (param externref externref) (result i32)))
   (import "__js_bignum" "bignum_to_string" (func $bignum_to_string (param externref i32) (result i32)))

   ;; bgl_long_to_bignum
   (func $bgl_long_to_bignum (export "bgl_long_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $long_to_bignum (local.get $n)))))

   ;;; bgl_jsstring_to_bignum
   (func $bgl_jsstring_to_bignum (export "bgl_jsstring_to_bignum")
      (param $section i32)
      (param $offset i32)
      (param $len i32)
      (result (ref $bignum))
      (return (struct.new $bignum
		 (call $string_to_bignum
		    (local.get $section)
		    (local.get $offset)
		    (local.get $len)
		    (i32.const 10)))))
   
   ;;; bgl_string_to_bignum
   (func $bgl_string_to_bignum (export "bgl_string_to_bignum")
      (param $s (ref $bstring))
      (param $radix i32)
      (result (ref $bignum))
      (call $store_string (local.get $s) (i32.const 128))
      (return
	 (struct.new $bignum
	    (call $string_to_bignum
	       (i32.const 0)
	       (i32.const 128)
	       (array.len (local.get $s))
	       (local.get $radix)))))
   
   ;; bgl_bignum_to_string
   (func $bgl_bignum_to_string (export "bgl_bignum_to_string")
      (param $n (ref $bignum))
      (result (ref $bstring))
      (return_call $load_string
	 (i32.const 128)
	 (call $bignum_to_string
	    (struct.get $bignum $bx (local.get $n))
	    (i32.const 128))))
   
   ;; bgl_bignum_add
   (func $bgl_bignum_add (export "bgl_bignum_add")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_add
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_sub
   (func $bgl_bignum_sub (export "bgl_bignum_sub")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_sub
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_mul
   (func $bgl_bignum_mul (export "bgl_bignum_mul")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_mul
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_cmp
   (func $bgl_bignum_cmp (export "bgl_bignum_cmp")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result i32)
      (return_call $bignum_cmp
	 (struct.get $bignum $bx (local.get $x))
	 (struct.get $bignum $bx (local.get $y))))
   
   ;; BGL_SAFE_PLUS_FX
   (func $BGL_SAFE_PLUS_FX (export "BGL_SAFE_PLUS_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $I64_TO_BINT
	 (i64.add (local.get $x) (local.get $y))))

   ;; BGL_SAFE_MUL_FX
   (func $BGL_SAFE_MUL_FX (export "BGL_SAFE_MUL_FX")
     (param $x i64)
     (param $y i64)
     (result (ref eq))
     (return_call $I64_TO_BINT
	(i64.mul (local.get $x) (local.get $y))))

   ;; BGL_SAFE_MINUS_FX
   (func $BGL_SAFE_MINUS_FX (export "BGL_SAFE_MINUS_FX")
     (param $x i64)
     (param $y i64)
     (result (ref eq))
     (return_call $I64_TO_BINT
	(i64.sub (local.get $x) (local.get $y))))

   ;; BGL_SAFE_QUOTIENT_FX
   (func $BGL_SAFE_QUOTIENT_FX (export "BGL_SAFE_QUOTIENT_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $I64_TO_BINT
	 (i64.div_s (local.get $x) (local.get $y)))))

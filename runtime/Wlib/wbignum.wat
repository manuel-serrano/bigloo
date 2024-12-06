;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wbignum.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 12:51:44 2024                          */
;*    Last change :  Fri Dec  6 10:34:45 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM/JavaScript bignum implementation                            */
;*=====================================================================*/

(module $__bigloo_bignum
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $bignum
      (struct (field $bx externref)))
      
   ;; -----------------------------------------------------------------
   ;; JavaScript imports 
   ;; -----------------------------------------------------------------
   
   (import "__js_bignum" "zerobx" (global $zerobx externref))
   (import "__js_bignum" "zerobxp" (func $zerobxp (param externref) (result i32)))
   (import "__js_bignum" "bgl_safe_bignum_to_fixnum" (func $bgl_safe_bignum_to_fixnum (param externref) (param i32) (result i64)))
   (import "__js_bignum" "long_to_bignum" (func $long_to_bignum (param i64) (result externref)))
   (import "__js_bignum" "string_to_bignum" (func $string_to_bignum (param i32 i32 i32) (result externref)))
   (import "__js_bignum" "bignum_add" (func $bignum_add (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_sub" (func $bignum_sub (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_mul" (func $bignum_mul (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_cmp" (func $bignum_cmp (param externref externref) (result i32)))
   (import "__js_bignum" "bignum_to_string" (func $bignum_to_string (param externref i32) (result i32)))


   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------

   (global $bignum-default-value
      (export "BGL_BIGNUM_DEFAULT_VALUE") (ref $bignum)
      (struct.new $bignum (global.get $zerobx)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------

   ;; BXZERO
   (func $BXZERO (export "BXZERO")
      (param $n (ref $bignum))
      (result i32)
      (return_call $zerobxp (struct.get $bignum $bx (local.get $n))))
   
   ;; bgl_long_to_bignum
   (func $bgl_long_to_bignum (export "bgl_long_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $long_to_bignum (local.get $n)))))

   ;; bgl_jsstring_to_bignum
   (func $bgl_jsstring_to_bignum (export "bgl_jsstring_to_bignum")
      (param $section i32)
      (param $offset i32)
      (param $len i32)
      (result (ref $bignum))
      (return (struct.new $bignum
		 (call $string_to_bignum
		    (local.get $offset)
		    (local.get $len)
		    (i32.const 10)))))
   
   ;; bgl_string_to_bignum
   (func $bgl_string_to_bignum (export "bgl_string_to_bignum")
      (param $s (ref $bstring))
      (param $radix i32)
      (result (ref $bignum))
      (call $store_string (local.get $s) (i32.const 128))
      (return
	 (struct.new $bignum
	    (call $string_to_bignum
	       (i32.const 128)
	       (array.len (local.get $s))
	       (local.get $radix)))))

   ;; bgl_string_to_integer_obj
   (func $bgl_string_to_integer_obj (export "bgl_string_to_integer_obj")
      (param $s (ref $bstring))
      (param $radix i32)
      (result (ref eq))
      (return_call $BGL_SAFE_BX_TO_FX
	 (call $bgl_string_to_bignum (local.get $s) (local.get $radix))))
   
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

   ;; BGL_SAFE_BX_TO_FX
   (func $BGL_SAFE_BX_TO_FX (export "BGL_SAFE_BX_TO_FX")
      (param $n (ref $bignum))
      (result (ref eq))

      (local $tmp i64)
      (local $bx externref)

      (local.set $bx (struct.get $bignum $bx (local.get $n)))
      (local.set $tmp (call $bgl_safe_bignum_to_fixnum (local.get $bx) (global.get $MAXVALFX_BITSIZE)))

      (if (i64.eqz (local.get $tmp))
	  (then
	     (if (call $zerobxp (local.get $bx))
		 (then (return (call $make_bint (i64.const 0))))
		 (else (return (local.get $n)))))
	  (else
	   (return (call $make_bint (local.get $tmp))))))

   ;; INTEGER_SIGN
   (func $INTEGER_SIGN
      (param $x i64)
      (result i32)
      (if (i64.gt_s (local.get $x) (i64.const 0))
	  (then (return (i32.const 1)))
	  (else
	   (if (i64.eqz (local.get $x))
	       (then (return (i32.const 0)))
	       (else (return (i32.const -1)))))))
   
   ;; BGL_SAFE_PLUS_FX
   (func $BGL_SAFE_PLUS_FX (export "BGL_SAFE_PLUS_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      
      (local $sx i32)
      (local $sy i32)
      (local $t i64)
      
      (local.set $sx (call $INTEGER_SIGN (local.get $x)))
      (local.set $sy (call $INTEGER_SIGN (local.get $y)))
      (local.set $t (i64.add (local.get $x) (local.get $y)))
      
      (if (i32.eq (local.get $sx) (local.get $sy))
	  (then
	     (if (i32.eq (local.get $sx) (call $INTEGER_SIGN (local.get $t)))
		 (then
		    (return_call $make_bint (local.get $t)))
		 (else
		    (return_call $bgl_bignum_add
		       (call $bgl_long_to_bignum (local.get $x))
		       (call $bgl_long_to_bignum (local.get $y))))))
	  (else (return_call $make_bint (local.get $t)))))

   ;; BGL_SAFE_MINUS_FX
   (func $BGL_SAFE_MINUS_FX (export "BGL_SAFE_MINUS_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))

      (return_call $BGL_SAFE_PLUS_FX
	 (local.get $x)
	 (i64.sub (i64.const 0) (local.get $y))))

   ;; BGL_SAFE_MUL_FX
   (func $BGL_SAFE_MUL_FX (export "BGL_SAFE_MUL_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      
      (local $t i64)
      
      (if (i64.eqz (local.get $y))
	  (then (return_call $make_bint (local.get $y)))
	  (else
	   (local.set $t (i64.mul (local.get $x) (local.get $y)))
	   (if (i64.eq (i64.div_s (local.get $t) (local.get $y)) (local.get $x))
	       (then (return_call $make_bint (local.get $t)))
	       (else
		(return_call $bgl_bignum_mul
		   (call $bgl_long_to_bignum (local.get $x))
		   (call $bgl_long_to_bignum (local.get $y))))))))

   ;; BGL_SAFE_QUOTIENT_FX
   (func $BGL_SAFE_QUOTIENT_FX (export "BGL_SAFE_QUOTIENT_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $make_bint
	 (i64.div_s (local.get $x) (local.get $y)))))

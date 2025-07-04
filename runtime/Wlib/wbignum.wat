;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wbignum.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 12:51:44 2024                          */
;*    Last change :  Mon Jun 23 05:26:17 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
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
   (import "__js_bignum" "bxpositivep" (func $bxpositivep (param externref) (result i32)))
   (import "__js_bignum" "bxnegativep" (func $bxnegativep (param externref) (result i32)))
   (import "__js_bignum" "bignum_odd" (func $bignum_odd (param externref) (result i32)))
   (import "__js_bignum" "bignum_even" (func $bignum_even (param externref) (result i32)))
   (import "__js_bignum" "safe_bignum_to_fixnum" (func $js_safe_bignum_to_fixnum (param externref) (param i32) (result f64)))
   (import "__js_bignum" "bignum_to_long" (func $js_bignum_to_long (param externref) (result f64)))
   (import "__js_bignum" "bignum_to_long" (func $js_bignum_to_llong (param externref) (result f64)))
   (import "__js_bignum" "bignum_to_flonum" (func $bgl_bignum_to_flonum (param externref) (result f64)))
   (import "__js_bignum" "seed_rand" (func $seed_rand (param i32)))
   (import "__js_bignum" "rand_bignum" (func $rand_bignum (param externref) (result externref)))
   (import "__js_bignum" "rand_fixnum" (func $rand_fixnum (result i32)))
   (import "__js_bignum" "double_to_bignum" (func $js_double_to_bignum (param f64) (result externref)))
   (import "__js_bignum" "string_to_bignum" (func $string_to_bignum (param i32 i32 i32) (result externref)))
   (import "__js_bignum" "bignum_abs" (func $bignum_abs (param externref) (result externref)))
   (import "__js_bignum" "bignum_gcd" (func $bignum_gcd (param externref) (result externref)))
   (import "__js_bignum" "bignum_lcm" (func $bignum_lcm (param externref) (result externref)))
   (import "__js_bignum" "bignum_neg" (func $bignum_neg (param externref) (result externref)))
   (import "__js_bignum" "bignum_add" (func $bignum_add (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_sub" (func $bignum_sub (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_mul" (func $bignum_mul (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_xor" (func $bignum_xor (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_or" (func $bignum_or (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_and" (func $bignum_and (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_not" (func $bignum_not (param externref) (result externref)))
    (import "__js_bignum" "bignum_lsh" (func $bignum_lsh (param externref) (param f64) (result externref)))
   (import "__js_bignum" "bignum_rsh" (func $bignum_rsh (param externref) (param f64) (result externref)))
   (import "__js_bignum" "bignum_mask" (func $bignum_mask (param externref) (param f64) (result externref)))
   (import "__js_bignum" "bignum_quotient" (func $bignum_quotient (param externref externref) (result externref)))
   (import "__js_bignum" "bignum_remainder" (func $bignum_remainder (param externref externref) (result externref)))
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
   
   ;; BXPOSITIVE
   (func $BXPOSITIVE (export "BXPOSITIVE")
      (param $n (ref $bignum))
      (result i32)
      (return_call $bxpositivep (struct.get $bignum $bx (local.get $n))))

   ;; BXNEGATIVE
   (func $BXNEGATIVE (export "BXNEGATIVE")
      (param $n (ref $bignum))
      (result i32)
      (return_call $bxnegativep (struct.get $bignum $bx (local.get $n))))

   ;; bgl_bignum_to_long
   (func $bgl_bignum_to_long (export "bgl_bignum_to_long")
      (param $bx externref)
      (result i64)
      (i64.trunc_f64_s (call $js_bignum_to_long (local.get $bx))))
      
   ;; bgl_bignum_to_llong
   (func $bgl_bignum_to_llong (export "bgl_bignum_to_llong")
      (param $n (ref $bignum))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_bignum_to_long (struct.get $bignum $bx (local.get $n)))))
      
   ;; bgl_bignum_to_int64
   (func $bgl_bignum_to_int64 (export "bgl_bignum_to_int64")
      (param $n (ref $bignum))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_bignum_to_long (struct.get $bignum $bx (local.get $n)))))
      
   ;; bgl_bignum_to_uint64
   (func $bgl_bignum_to_uint64 (export "bgl_bignum_to_uint64")
      (param $n (ref $bignum))
      (result i64)
      (i64.trunc_f64_u
	 (call $js_bignum_to_long (struct.get $bignum $bx (local.get $n)))))
      
   ;; bgl_long_to_bignum
   (func $bgl_long_to_bignum (export "bgl_long_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $js_double_to_bignum (f64.convert_i64_s (local.get $n))))))

   ;; bgl_llong_to_bignum
   (func $bgl_llong_to_bignum (export "bgl_llong_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return_call $bgl_long_to_bignum (local.get $n)))

   ;; bgl_int64_to_bignum
   (func $bgl_int64_to_bignum (export "bgl_int64_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $js_double_to_bignum (f64.convert_i64_s (local.get $n))))))

   ;; bgl_uint64_to_bignum
   (func $bgl_uint64_to_bignum (export "bgl_uint64_to_bignum")
      (param $n i64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $js_double_to_bignum (f64.convert_i64_u (local.get $n))))))

   ;; bgl_flonum_to_bignum
   (func $bgl_flonum_to_bignum (export "bgl_flonum_to_bignum")
      (param $n f64)
      (result (ref $bignum))
      (return (struct.new $bignum (call $js_double_to_bignum (local.get $n)))))

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
      (param $r i32)
      (result (ref $bstring))
      (return_call $load_string
	 (i32.const 128)
	 (call $bignum_to_string
	    (struct.get $bignum $bx (local.get $n))
	    (i32.const 128))))

   ;; bgl_bignum_abs
   (func $bgl_bignum_abs (export "bgl_bignum_abs")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_abs
	       (struct.get $bignum $bx (local.get $x))))))
   
   ;; bgl_bignum_gcd
   (func $bgl_bignum_gcd (export "bgl_bignum_gcd")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_gcd
	       (struct.get $bignum $bx (local.get $x))))))
   
   ;; bgl_bignum_lcm
   (func $bgl_bignum_lcm (export "bgl_bignum_lcm")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_lcm
	       (struct.get $bignum $bx (local.get $x))))))
   
   ;; bgl_bignum_neg
   (func $bgl_bignum_neg (export "bgl_bignum_neg")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_neg
	       (struct.get $bignum $bx (local.get $x))))))
   
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
   
   ;; bgl_bignum_div
   (func $bgl_bignum_div (export "bgl_bignum_div")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (local $q (ref $bignum))
      (local $r (ref $bignum))
      (local.set $q
	 (struct.new $bignum
	    (call $bignum_quotient
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y)))))
      (local.set $r
	 (struct.new $bignum
	    (call $bignum_remainder
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y)))))
      (call $BGL_MVALUES_NUMBER_SET (i32.const 2))
      (call $BGL_MVALUES_VAL_SET (i32.const 1) (local.get $r))
      (return (local.get $q)))
   
   ;; bgl_bignum_xor
   (func $bgl_bignum_xor (export "bgl_bignum_xor")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_xor
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_or
   (func $bgl_bignum_or (export "bgl_bignum_or")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_or
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_and
   (func $bgl_bignum_and (export "bgl_bignum_and")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_and
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_not
   (func $bgl_bignum_not (export "bgl_bignum_not")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_not
	       (struct.get $bignum $bx (local.get $x))))))
   
   ;; bgl_bignum_lsh
   (func $bgl_bignum_lsh (export "bgl_bignum_lsh")
      (param $x (ref $bignum))
      (param $n i64)
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_lsh
	       (struct.get $bignum $bx (local.get $x))
	       (f64.convert_i64_s (local.get $n))))))
   
   ;; bgl_bignum_rsh
   (func $bgl_bignum_rsh (export "bgl_bignum_rsh")
      (param $x (ref $bignum))
      (param $n i64)
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_rsh
	       (struct.get $bignum $bx (local.get $x))
	       (f64.convert_i64_s (local.get $n))))))
   
   ;; bgl_bignum_mask
   (func $bgl_bignum_mask (export "bgl_bignum_mask")
      (param $x (ref $bignum))
      (param $n i64)
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_mask
	       (struct.get $bignum $bx (local.get $x))
	       (f64.convert_i64_s (local.get $n))))))
   
   ;; bgl_bignum_cmp
   (func $bgl_bignum_cmp (export "bgl_bignum_cmp")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result i32)
      (return_call $bignum_cmp
	 (struct.get $bignum $bx (local.get $x))
	 (struct.get $bignum $bx (local.get $y))))

   ;; bgl_bignum_quotient
   (func $bgl_bignum_quotient (export "bgl_bignum_quotient")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_quotient
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))
   
   ;; bgl_bignum_remainder
   (func $bgl_bignum_remainder (export "bgl_bignum_remainder")
      (param $x (ref $bignum))
      (param $y (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $bignum_remainder
	       (struct.get $bignum $bx (local.get $x))
	       (struct.get $bignum $bx (local.get $y))))))

   ;; bgl_seed_rand
   (func $bgl_seed_rand (export "bgl_seed_rand")
      (param $x i64)
      (call $seed_rand (i32.wrap_i64 (local.get $x))))
   
   ;; bgl_rand_bignum
   (func $bgl_rand_bignum (export "bgl_rand_bignum")
      (param $x (ref $bignum))
      (result (ref $bignum))
      (return
	 (struct.new $bignum
	    (call $rand_bignum
	       (struct.get $bignum $bx (local.get $x))))))

   ;; rand
   (func $rand (export "rand")
      (result i32)
      (return_call $rand_fixnum))

   ;; BGL_SAFE_BX_TO_FX
   (func $BGL_SAFE_BX_TO_FX (export "BGL_SAFE_BX_TO_FX")
      (param $n (ref $bignum))
      (result (ref eq))

      (local $tmp i64)
      (local $bx externref)

      (local.set $bx (struct.get $bignum $bx (local.get $n)))
      (local.set $tmp (i64.trunc_f64_s (call $js_safe_bignum_to_fixnum (local.get $bx) (global.get $MAXVALFX_BITSIZE))))

      (if (i64.eqz (local.get $tmp))
	  (then
	     (if (call $zerobxp (local.get $bx))
		 (then (return (call $BINT (i64.const 0))))
		 (else (return (local.get $n)))))
	  (else
	   (return (call $BINT (local.get $tmp))))))

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
		    (return_call $BINT (local.get $t)))
		 (else
		    (return_call $bgl_bignum_add
		       (call $bgl_long_to_bignum (local.get $x))
		       (call $bgl_long_to_bignum (local.get $y))))))
	  (else (return_call $BINT (local.get $t)))))

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
	  (then (return_call $BINT (local.get $y)))
	  (else
	   (local.set $t (i64.mul (local.get $x) (local.get $y)))
	   (if (i64.eq (i64.div_s (local.get $t) (local.get $y)) (local.get $x))
	       (then (return_call $BINT (local.get $t)))
	       (else
		(return_call $bgl_bignum_mul
		   (call $bgl_long_to_bignum (local.get $x))
		   (call $bgl_long_to_bignum (local.get $y))))))))

   ;; BGL_SAFE_QUOTIENT_FX
   (func $BGL_SAFE_QUOTIENT_FX (export "BGL_SAFE_QUOTIENT_FX")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BINT
	 (i64.div_s (local.get $x) (local.get $y))))

   ;; BGL_SAFE_PLUS_ELONG
   (func $BGL_SAFE_PLUS_ELONG (export "BGL_SAFE_PLUS_ELONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_PLUS_FX (local.get $x) (local.get $y)))

   ;; BGL_SAFE_MINUS_ELONG
   (func $BGL_SAFE_MINUS_ELONG (export "BGL_SAFE_MINUS_ELONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_MINUS_FX (local.get $x) (local.get $y)))

   ;; BGL_SAFE_MUL_ELONG
   (func $BGL_SAFE_MUL_ELONG (export "BGL_SAFE_MUL_ELONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_MUL_FX (local.get $x) (local.get $y)))
   
   ;; BGL_SAFE_QUOTIENT_ELONG
   (func $BGL_SAFE_QUOTIENT_ELONG (export "BGL_SAFE_QUOTIENT_ELONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $make_belong (i64.div_s (local.get $x) (local.get $y))))
   
   ;; BGL_SAFE_PLUS_LLONG
   (func $BGL_SAFE_PLUS_LLONG (export "BGL_SAFE_PLUS_LLONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_PLUS_FX (local.get $x) (local.get $y)))

   ;; BGL_SAFE_MINUS_LLONG
   (func $BGL_SAFE_MINUS_LLONG (export "BGL_SAFE_MINUS_LLONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_MINUS_FX (local.get $x) (local.get $y)))

   ;; BGL_SAFE_MUL_LLONG
   (func $BGL_SAFE_MUL_LLONG (export "BGL_SAFE_MUL_LLONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $BGL_SAFE_MUL_FX (local.get $x) (local.get $y)))

   ;; BGL_SAFE_QUOTIENT_LLONG
   (func $BGL_SAFE_QUOTIENT_LLONG (export "BGL_SAFE_QUOTIENT_LLONG")
      (param $x i64)
      (param $y i64)
      (result (ref eq))
      (return_call $make_bllong (i64.div_s (local.get $x) (local.get $y))))

   (func $bgl_bignum_odd (export "bgl_bignum_odd")
      (param $n (ref $bignum))
      (result i32)
      (return_call $bignum_odd (struct.get $bignum $bx (local.get $n))))

   (func $bgl_bignum_even (export "bgl_bignum_even")
      (param $n (ref $bignum))
      (result i32)
      (return_call $bignum_even (struct.get $bignum $bx (local.get $n))))
   
   )

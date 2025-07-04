;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wstring.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 28 06:41:16 2024                          */
;*    Last change :  Fri Jul  4 11:26:49 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM strings                                                     */
;*=====================================================================*/

(module $__bigloo_string
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $bstring (array (mut i8)))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $bstring-default-value
      (export "BGL_BSTRING_DEFAULT_VALUE") (ref $bstring)
      (array.new_fixed $bstring 0))
   
   ;; -----------------------------------------------------------------
   ;; Macros
   ;; -----------------------------------------------------------------
   
   (func $STRINGP (export "STRINGP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $bstring) (local.get $o)))
   
   (func $STRING_LENGTH (export "STRING_LENGTH")
      (param $s (ref $bstring))
      (result i64)
      (i64.extend_i32_u (array.len (local.get $s))))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   ;; string_to_bstring_len
   (func $string_to_bstring_len (export "string_to_bstring_len")
      (param $s (ref $bstring))
      (param $len i32)
      (result (ref $bstring))
      
      (local $new (ref $bstring))
      (local.set $new (array.new_default $bstring (local.get $len)))
      (array.copy $bstring $bstring
	 (local.get $new)
	 (i32.const 0)
	 (local.get $s)
	 (i32.const 0)
	 (local.get $len))
      
      (return (local.get $new)))
   
   ;; bgl_string_shrink
   (func $bgl_string_shrink (export "bgl_string_shrink")
      (param $s (ref $bstring))
      (param $l i64)
      (result (ref $bstring))
      
      (if (i64.eq (call $STRING_LENGTH (local.get $s)) (local.get $l))
	  (then
	     (return (local.get $s)))
	  (else
	   (return_call $string_to_bstring_len (local.get $s)
	      (i32.wrap_i64 (local.get $l)))))
      (unreachable))

   ;; string_append
   (func $string_append 
      (export "string_append") 
      (param $a (ref $bstring)) 
      (param $b (ref $bstring))
      (result (ref $bstring))
      (local $r (ref $bstring))
      (local.set $r
	 (array.new_default $bstring 
	    (i32.add 
	       (array.len (local.get $a))
	       (array.len (local.get $b)))))
      (array.copy $bstring $bstring (local.get $r) (i32.const 0) (local.get $a) (i32.const 0) (array.len (local.get $a)))
      (array.copy $bstring $bstring (local.get $r) (array.len (local.get $a)) (local.get $b) (i32.const 0) (array.len (local.get $b)))
      (local.get $r))

   ;; string_append_3
   (func $string_append_3
      (export "string_append_3")
      (param $a (ref $bstring)) 
      (param $b (ref $bstring))
      (param $c (ref $bstring))
      (result (ref $bstring))
      (local $r (ref $bstring))
      (local $l1 i32)
      (local $l2 i32)
      (local $l3 i32)
      (local.set $l1 (array.len (local.get $a)))
      (local.set $l2 (array.len (local.get $b)))
      (local.set $l3 (array.len (local.get $c)))
      (local.set $r
	 (array.new_default $bstring 
	    (i32.add 
	       (i32.add (local.get $l1) (local.get $l2))
	       (local.get $l3))))
      (array.copy $bstring $bstring (local.get $r) (i32.const 0) (local.get $a) (i32.const 0) (local.get $l1))
      (array.copy $bstring $bstring (local.get $r) (local.get $l1) (local.get $b) (i32.const 0) (local.get $l2))
      (array.copy $bstring $bstring (local.get $r) (i32.add (local.get $l1) (local.get $l2)) (local.get $c) (i32.const 0) (local.get $l3))
      (local.get $r))

   ;; c_substring
   (func $c_substring
      (export "c_substring")
      (param $str (ref $bstring))
      (param $min i64)
      (param $max i64)
      (result (ref $bstring))
      (local $len i32)
      (local $r (ref $bstring))
      (local.set $len (i32.wrap_i64 (i64.sub (local.get $max) (local.get $min))))
      (local.set $r (array.new_default $bstring (local.get $len)))
      (array.copy $bstring $bstring
	 (local.get $r)
	 (i32.const 0)
	 (local.get $str)
	 (i32.wrap_i64 (local.get $min))
	 (local.get $len))
      (local.get $r))

   ;; BUCS2
   (func $BUCS2
      (export "BUCS2")
      (param $v i32)
      (result (ref $bucs2))
      (struct.new $bucs2 (local.get $v)))
   
   )
  

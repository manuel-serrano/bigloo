;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wstring.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 28 06:41:16 2024                          */
;*    Last change :                                                    */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM strings                                                     */
;*=====================================================================*/

(module $__runtime_strings

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------

   (type $bstring (array (mut i8)))
   (type $ucs2string (array (mut i16)))
   
   (type $regexp (struct))

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
      (i64.extend_i32_s (array.len (local.get $s))))

   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
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
   )
  

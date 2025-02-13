;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wunicode.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  5 09:51:39 2025                          */
;*    Last change :  Thu Feb  6 09:25:57 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM unicode strings and characters                              */
;*=====================================================================*/

(module $__bigloo_unicode
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------

   (rec
      (type $__dummy_bucs2 (field $dummy i16))
      (type $bucs2 (struct (field $v i16))))
   (rec
      (type $__dummy_ucs2string (field $dummy i8))
      (type $ucs2string (array (mut i16))))

   ;; -----------------------------------------------------------------
   ;; JavaScript imports 
   ;; -----------------------------------------------------------------
   
   (import "__js_unicode" "ucs2_definedp" (func $js_ucs2_definedp (param i32) (result i32)))
   (import "__js_unicode" "ucs2_letterp" (func $js_ucs2_letterp (param i32) (result i32)))
   (import "__js_unicode" "ucs2_digitp" (func $js_ucs2_digitp (param i32) (result i32)))
   (import "__js_unicode" "ucs2_whitespacep" (func $js_ucs2_whitespacep (param i32) (result i32)))
   (import "__js_unicode" "ucs2_upperp" (func $js_ucs2_upperp (param i32) (result i32)))
   (import "__js_unicode" "ucs2_lowerp" (func $js_ucs2_lowerp (param i32) (result i32)))
   (import "__js_unicode" "ucs2_toupper" (func $js_ucs2_toupper (param i32) (result i32)))
   (import "__js_unicode" "ucs2_tolower" (func $js_ucs2_tolower (param i32) (result i32)))

;;   (import "__js_unicode" "ucs2_strcmp" (func $js_ucs2_strcmp (param i32) (param i32) (param i32) (param i32) (result i32)))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
  (global $ucs2string-default-value
     (export "BGL_UCS2STRING_DEFAULT_VALUE") (ref $ucs2string)
     (array.new_fixed $ucs2string 0))
  
   ;; -----------------------------------------------------------------
   ;; Macros
   ;; -----------------------------------------------------------------

   (func $UCS2P (export "UCS2P")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $bucs2) (local.get $o)))

   (func $UCS2_STRINGP (export "UCS2_STRINGP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $ucs2string) (local.get $o)))
   
   (func $UCS2_STRING_LENGTH (export "UCS2_STRING_LENGTH")
      (param $s (ref $ucs2string))
      (result i64)
      (i64.extend_i32_u (array.len (local.get $s))))


   ;; -----------------------------------------------------------------
   ;; UCS2 character functions
   ;; -----------------------------------------------------------------

   (func $ucs2_definedp (export "ucs2_definedp")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_definedp (local.get $c)))
   
   (func $ucs2_letterp (export "ucs2_letterp")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_letterp (local.get $c)))
   
   (func $ucs2_digitp (export "ucs2_digitp")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_digitp (local.get $c)))
   
   (func $ucs2_whitespacep (export "ucs2_whitespacep")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_whitespacep (local.get $c)))
   
   (func $ucs2_upperp (export "ucs2_upperp")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_upperp (local.get $c)))
   
   (func $ucs2_lowerp (export "ucs2_lowerp")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_lowerp (local.get $c)))
   
   (func $ucs2_toupper (export "ucs2_toupper")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_toupper (local.get $c)))
   
   (func $ucs2_tolower (export "ucs2_tolower")
      (param $c i32)
      (result i32)
      (return_call $js_ucs2_tolower (local.get $c)))
   
   ;; -----------------------------------------------------------------
   ;; UCS2 string functions
   ;; -----------------------------------------------------------------

;*    (func $ucs2_strcmp                                               */
;*       (export "ucs2_strcmp")                                        */
;*       (param $x (ref $ucs2string))                                  */
;*       (param $y (ref $ucs2string))                                  */
;*       (result i32)                                                  */
;*                                                                     */
;*       (call $store_ucs2string                                       */
;* 	 (local.get $x)                                                */
;* 	 (i32.const 128))                                              */
;*                                                                     */
;*       (return_call $js_ucs2_strcmp                                  */
;* 	 (i32.const 128) (i32.mul (array.len (local.get $x)) (i32.const 2)) */
;* 	 (i32.const 128) (i32.mul (array.len (local.get $x)) (i32.const 2)))) */
   
    )

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wregexp.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 19 09:18:29 2024                          */
;*    Last change :  Thu Dec 19 09:36:04 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM regexp                                                      */
;*=====================================================================*/

(module $__bigloo_regexp
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
 
   (type $regexp
      (struct
	 (field $pat (ref $bstring))
	 (field $preg (mut (ref eq)))
	 (field $capturecount (mut i32))))
 
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $regexp-default-value
      (export "BGL_REGEXP_DEFAULT_VALUE") (ref $regexp)
      (struct.new $regexp
	 (global.get $bstring-default-value)
	 (global.get $BUNSPEC)
	 (i32.const 0)))

   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   (func $bgl_make_regexp (export "bgl_make_regexp")
      (param $p (ref $bstring))
      (result (ref $regexp))
      (struct.new $regexp (local.get $p) (global.get $BUNSPEC) (i32.const 0)))

   (func $BGL_REGEXP_PAT (export "BGL_REGEXP_PAT")
      (param $rx (ref $regexp))
      (result (ref $bstring))
      (struct.get $regexp $pat (local.get $rx)))

   (func $BGL_REGEXP_PREG (export "BGL_REGEXP_PREG")
      (param $rx (ref $regexp))
      (result (ref eq))
      (struct.get $regexp $preg (local.get $rx)))

   (func $BGL_REGEXP_PREG_SET (export "BGL_REGEXP_PREG_SET")
      (param $rx (ref $regexp))
      (param $preg (ref eq))
      (result (ref eq))
      (struct.set $regexp $preg (local.get $rx) (local.get $preg))
      (local.get $rx))

   (func $BGL_REGEXP_CAPTURECOUNT (export "BGL_REGEXP_CAPTURECOUNT")
      (param $rx (ref $regexp))
      (result i32)
      (struct.get $regexp $capturecount (local.get $rx))))











   
  

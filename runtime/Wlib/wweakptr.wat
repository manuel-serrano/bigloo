;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wweakptr.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 17 09:38:18 2024                          */
;*    Last change :  Tue Dec 17 09:57:29 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM weak pointers (currently faked).                            */
;*=====================================================================*/

(module $__bigloo_weakptr

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $weakptr
      (struct
	 (field $data (mut (ref eq)))
	 (field $ref (mut (ref eq)))))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))

   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------

  (global $weakptr-default-value
     (export "BGL_WEAKPTR_DEFAULT_VALUE") (ref $weakptr)
     (struct.new $weakptr (global.get $BUNSPEC) (global.get $BUNSPEC)))
  
   (func $BGL_WEAKPTRP (export "BGL_WEAKPTRP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $weakptr) (local.get $o)))
   
   (func $bgl_make_weakptr (export "bgl_make_weakptr")
      (param $data (ref eq))
      (param $ref (ref eq))
      (result (ref $weakptr))
      (struct.new $weakptr (local.get $data) (local.get $ref)))

   (func $bgl_weakptr_data (export "bgl_weakptr_data")
      (param $wp (ref $weakptr))
      (result (ref eq))
      (struct.get $weakptr $data (local.get $wp)))

   (func $bgl_weakptr_data_set (export "bgl_weakptr_data_set")
      (param $wp (ref $weakptr))
      (param $v (ref eq))
      (struct.set $weakptr $data (local.get $wp) (local.get $v)))

   (func $bgl_weakptr_ref (export "bgl_weakptr_ref")
      (param $wp (ref $weakptr))
      (result (ref eq))
      (struct.get $weakptr $ref (local.get $wp)))

   (func $bgl_weakptr_ref_set (export "bgl_weakptr_ref_set")
      (param $wp (ref $weakptr))
      (param $v (ref eq))
      (struct.set $weakptr $ref (local.get $wp) (local.get $v))))

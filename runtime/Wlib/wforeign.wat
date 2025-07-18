;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wforeign.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Fri Jul 18 08:49:45 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM foreign objects                                             */
;*=====================================================================*/

(module $__bigloo_foreign
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (rec
      (type $cobj
	 (struct))
      (type $foreign
	 (struct
	    (field $id (ref $symbol))
	    (field $ptr i32))))
   
   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $OBJ_TO_COBJ "obj->cobj")
   (data $ILL_OBJ_TYPE "Illegal object type")

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BGL_BINT_DEFAULT_VALUE" (global $bint-default-value (ref eq)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "BOOLEANP" (func $BOOLEANP (param (ref eq)) (result i32)))
   (import "__bigloo" "CBOOL" (func $CBOOL (param (ref $bbool)) (result i32)))
   (import "__bigloo" "INTEGERP" (func $INTEGERP (param (ref eq)) (result i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "OBJ_TO_INT" (func $OBJ_TO_INT (param (ref eq)) (result i64)))
   (import "__bigloo" "STRINGP" (func $STRINGP (param (ref eq)) (result i32)))
   (import "__bigloo" "BSTRING_TO_STRING" (func $BSTRING_TO_STRING (param (ref $bstring)) (result (ref $bstring))))
   (import "__bigloo" "string_to_bstring" (func $string_to_bstring (param (ref $bstring)) (result (ref $bstring))))
   (import "__bigloo" "CHARP" (func $CHARP (param (ref eq)) (result i32)))
   (import "__bigloo" "CCHAR" (func $CCHAR (param (ref $bchar)) (result i32)))
   
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $foreign-default-value
      (export "BGL_FOREIGN_DEFAULT_VALUE") (ref $foreign)
      (struct.new $foreign
	 (global.get $symbol-default-value)
	 (i32.const 0)))
   
   ;; -----------------------------------------------------------------
   ;; Predicates
   ;; -----------------------------------------------------------------
   
   (func $FOREIGNP (export "FOREIGNP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $foreign) (local.get $o)))

   (func $FOREIGN_TO_OBJ
      (param $o (ref $foreign))
      (result i32)
      (return (struct.get $foreign $ptr (local.get $o))))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (func $obj_to_cobj (export "obj_to_cobj")
      (param $o (ref eq))
      (result i32)
      
      (if (call $INTEGERP (local.get $o))
	  (then (return (i32.wrap_i64 (call $OBJ_TO_INT (local.get $o))))))
      (if (call $BOOLEANP (local.get $o))
	  (then (return_call $CBOOL (ref.cast (ref $bbool) (local.get $o)))))
      (if (call $CHARP (local.get $o))
	  (then (return_call $CCHAR (ref.cast (ref $bchar) (local.get $o)))))
      (if (call $FOREIGNP (local.get $o))
	  (then (return_call $FOREIGN_TO_OBJ (ref.cast (ref $foreign) (local.get $o)))))
      (call $the_failure
	 (array.new_data $bstring $OBJ_TO_COBJ (i32.const 0) (i32.const 9))
	 (array.new_data $bstring $ILL_OBJ_TYPE (i32.const 0) (i32.const 21))
	 (local.get $o))
      (unreachable))
   
   )

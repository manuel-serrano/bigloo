;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wforeign.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Wed Sep 10 07:49:01 2025 (serrano)                */
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
	 (sub (struct
		 (field $id (ref $symbol))
		 (field $obj externref)))))
   
   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $OBJ_TO_COBJ "obj->cobj")
   (data $ILL_OBJ_TYPE "Illegal object type")

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js" "nil" (global $externref-nil externref))
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BGL_BINT_DEFAULT_VALUE" (global $bint-default-value (ref eq)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "BOOLEANP" (func $BOOLEANP (param (ref eq)) (result i32)))
   (import "__bigloo" "INTEGERP" (func $INTEGERP (param (ref eq)) (result i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "OBJ_TO_INT" (func $OBJ_TO_INT (param (ref eq)) (result i64)))
   (import "__bigloo" "OBJ_TO_BOOL" (func $OBJ_TO_BOOL (param (ref eq)) (result i32)))
   (import "__bigloo" "OBJ_TO_CHAR" (func $OBJ_TO_CHAR (param (ref eq)) (result i32)))
   (import "__bigloo" "STRINGP" (func $STRINGP (param (ref eq)) (result i32)))
   (import "__bigloo" "BSTRING_TO_STRING" (func $BSTRING_TO_STRING (param (ref $bstring)) (result (ref $bstring))))
   (import "__bigloo" "string_to_bstring" (func $string_to_bstring (param (ref $bstring)) (result (ref $bstring))))
   (import "__bigloo" "CHARP" (func $CHARP (param (ref eq)) (result i32)))
   
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------

   (global $externref-default-value
      (export "BGL_EXTERNREF_DEFAULT_VALUE") externref
	 (global.get $externref-nil))
   
   (global $foreign-default-value
      (export "BGL_FOREIGN_DEFAULT_VALUE") (ref $foreign)
      (struct.new $foreign
	 (global.get $symbol-default-value)
	 (global.get $externref-default-value)))
   
   ;; -----------------------------------------------------------------
   ;; Predicates
   ;; -----------------------------------------------------------------
   
   (func $FOREIGNP (export "FOREIGNP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $foreign) (local.get $o)))

   (func $FOREIGN_NULLP (export "FOREIGN_NULLP")
      (param $o (ref eq))
      (result i32)
      (if (result i32) (ref.test (ref $foreign) (local.get $o))
	  (then
	     (if (result i32) (ref.is_null (struct.get $foreign $obj (ref.cast (ref $foreign) (local.get $o))))
		 (then
		    (i32.const 1))
		 (else
		  (i32.const 0))))
	  (else
	   (i32.const 0))))

   (func $FOREIGN_EQ (export "FOREIGN_EQ")
      (param $x (ref $foreign))
      (param $y (ref $foreign))
      (result i32)
      (return (ref.eq (local.get $x) (local.get $y))))

   (func $FOREIGN_ID (export "FOREIGN_ID")
      (param $o (ref $foreign))
      (result (ref $symbol))
      (return (struct.get $foreign $id (local.get $o))))
   
   (func $FOREIGN_PTR_NULLP (export "FOREIGN_PTR_NULLP")
      (param $o externref)
      (result i32)
      (ref.is_null (local.get $o)))

   (func $FOREIGN_TO_COBJ (export "FOREIGN_TO_COBJ")
      (param $o (ref $foreign))
      (result externref)
      (struct.get $foreign $obj (local.get $o)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (func $obj_to_cobj (export "obj_to_cobj")
      (param $o (ref eq))
      (result i32)
      
      (if (call $INTEGERP (local.get $o))
	  (then (return (i32.wrap_i64 (call $OBJ_TO_INT (local.get $o))))))
      (if (call $BOOLEANP (local.get $o))
	  (then (return_call $OBJ_TO_BOOL (local.get $o))))
      (if (call $CHARP (local.get $o))
	  (then (return_call $OBJ_TO_CHAR (local.get $o))))
      (if (call $FOREIGNP (local.get $o))
	  (then (return (i32.const 666))))
      (call $the_failure
	 (array.new_data $bstring $OBJ_TO_COBJ (i32.const 0) (i32.const 9))
	 (array.new_data $bstring $ILL_OBJ_TYPE (i32.const 0) (i32.const 21))
	 (local.get $o))
      (unreachable))

   (func $void_star_to_obj (export "void_star_to_obj")
      (param $o externref)
      (result (ref eq))
      (return (struct.new $foreign (global.get $symbol-default-value) (local.get $o))))

   (func $obj_to_void_star (export "obj_to_void_star")
      (param $o (ref eq))
      (result externref)
      (return (struct.get $foreign $obj (ref.cast (ref $foreign) (local.get $o)))))
   )

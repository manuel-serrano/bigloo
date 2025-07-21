;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wobject.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:02:42 2024                          */
;*    Last change :  Mon Jul 21 14:20:24 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM objects and classes                                         */
;*=====================================================================*/

(module $__bigloo_object
   
   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (elem declare func $generic_entry)
   (elem declare func $generic_entry1)
   (elem declare func $generic_entry2)
   (elem declare func $generic_entry3)
   (elem declare func $generic_entry4)
   (elem declare func $generic_entry5)

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   ;; Classes
   (type $class
      (struct
	 (field $name (ref $symbol))
	 (field $module (ref $symbol))
	 (field $new_fun (ref eq))
	 (field $alloc_fun (ref $procedure))
	 (field $nil_fun (mut (ref $procedure)))
	 (field $nil (mut (ref eq)))
	 (field $constructor (ref eq))
	 (field $super (ref eq))
	 (field $subclasses (mut (ref eq)))
	 (field $shrink (ref eq))
	 (field $evdata (mut (ref eq)))
	 (field $ancestors (ref $vector))
	 (field $virtual_fields (ref $vector))
	 (field $direct_fields (mut (ref $vector)))
	 (field $all_fields (mut (ref $vector)))
	 (field $hash i64)
	 (field $index i64)
	 (field $depth i64)))
   
   (type $BgL_objectz00_bglt
      (sub (struct
	      (field $header (mut i64))
	      (field $widening (mut (ref eq))))))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "class-nil@__object" (func $class-nil@__object (param (ref $class)) (result (ref eq))))
   (import "__bigloo" "MAKE_FX_PROCEDURE" (func $MAKE_FX_PROCEDURE (param (ref func)) (param i32) (param i32) (result (ref $procedure))))
   (import "__bigloo" "MAKE_VA_PROCEDURE" (func $MAKE_VA_PROCEDURE (param (ref func)) (param i32) (param i32) (result (ref $procedure))))
   (import "__bigloo" "PROCEDURE_SET" (func $PROCEDURE_SET (param (ref $procedure)) (param i32) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "PROCEDURE_REF" (func $PROCEDURE_REF (param (ref $procedure)) (param i32) (result (ref eq))))
   (import "__bigloo" "bgl_funcall0" (func $funcall0 (param (ref $procedure)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall1" (func $funcall1 (param (ref $procedure)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall2" (func $funcall2 (param (ref $procedure)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall3" (func $funcall3 (param (ref $procedure)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall4" (func $funcall4 (param (ref $procedure)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall5" (func $funcall5 (param (ref $procedure)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $class-default-value
      (export "BGL_CLASS_DEFAULT_VALUE") (ref $class)
      (struct.new $class
	 ;; name
	 (global.get $symbol-default-value)
	 ;; module
	 (global.get $symbol-default-value)
	 ;; new_fun
	 (global.get $BUNSPEC)
	 ;; alloc_fun
	 (global.get $procedure-default-value)
	 ;; nil_fun
	 (global.get $procedure-default-value)
	 ;; nil
	 (global.get $BUNSPEC)
	 ;; constructor
	 (global.get $BUNSPEC)
	 ;; super
	 (global.get $BUNSPEC)
	 ;; subclasses
	 (global.get $pair-default-value)
	 ;; shrink
	 (global.get $BUNSPEC)
	 ;; evdata
	 (global.get $BUNSPEC)
	 ;; ancestors
	 (global.get $vector-default-value)
	 ;; virtual_fields
	 (global.get $vector-default-value)
	 ;; direct_fields
	 (global.get $vector-default-value)
	 ;; all_fields
	 (global.get $vector-default-value)
	 ;; hash
	 (i64.const 0)
	 ;; index
	 (i64.const 0)
	 ;; depth
	 (i64.const 0)))
   
   (func $BGL_CLASS_INSTANCE_DEFAULT_VALUE
      (export "BGL_CLASS_INSTANCE_DEFAULT_VALUE")
      (param $clazz (ref $class))
      (result (ref $BgL_objectz00_bglt))
      (local $ctor (ref $procedure))
      (local.set $ctor (struct.get $class $nil_fun (local.get $clazz)))
      (ref.cast (ref $BgL_objectz00_bglt)
	 (call $class-nil@__object (local.get $clazz))))
;*       (ref.cast (ref $BgL_objectz00_bglt)                           */
;* 	 (call $BGl_classzd2nilzd2zz__objectz00 (local.get $clazz))))  */
   
   ;; --------------------------------------------------------
   ;; Library functions
   ;; --------------------------------------------------------

   ;; BGL_OBJECTP
   (func $BGL_OBJECTP (export "BGL_OBJECTP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $BgL_objectz00_bglt) (local.get $o)))
      
   ;; bgl_make_class
   (func $bgl_make_class (export "bgl_make_class")
      (param $name (ref $symbol))
      (param $module (ref $symbol))
      (param $num i64)
      (param $inheritance-num i64)
      (param $super (ref eq))
      (param $subclasses (ref eq))
      (param $alloc (ref $procedure))
      (param $hash i64)
      (param $direct-fields (ref $vector))
      (param $all-fields (ref $vector))
      (param $constructor (ref eq))
      (param $virtual-fields (ref $vector))
      (param $new (ref eq))
      (param $nil (ref $procedure))
      (param $shrink (ref eq))
      (param $depth i64)
      (param $evdata (ref eq))
      (result (ref $class))
      
      (local $self (ref $class))
      (local $ancestors (ref $vector))
      (local.set $ancestors
	 (array.new $vector (global.get $BFALSE)
	    (i32.add (i32.wrap_i64 (local.get $depth)) (i32.const 1))))
      (if (i64.gt_u (local.get $depth) (i64.const 0))
	  (then 
	     (array.copy 
		$vector $vector
		(local.get $ancestors)
		(i32.const 0)
		(struct.get $class $ancestors
		   (ref.cast (ref $class) (local.get $super)))
		(i32.const 0)
		(i32.wrap_i64 (local.get $depth)))))
      
      (local.set $self 
	 (struct.new $class
	    (local.get $name)
	    (local.get $module)
	    (local.get $new)
	    (local.get $alloc)
	    (local.get $nil)
	    (global.get $BFALSE)
	    (local.get $constructor)
	    (local.get $super)
	    (local.get $subclasses)
	    (local.get $shrink)
	    (local.get $evdata)
	    (local.get $ancestors)
	    (local.get $virtual-fields)
	    (local.get $direct-fields)
	    (local.get $all-fields)
	    (local.get $hash)
	    (local.get $num)
	    (local.get $depth)))
      
      (array.set $vector (local.get $ancestors)
	 (i32.wrap_i64 (local.get $depth)) (local.get $self))
      (local.get $self))

   ;; BGL_CLASS_SUBCLASSES_SET
   (func $BGL_CLASS_SUBCLASSES_SET (export "BGL_CLASS_SUBCLASSES_SET")
      (param $class (ref $class))
      (param $subclasses (ref eq))
      (result (ref eq))
      (struct.set $class $subclasses
	 (local.get $class) (local.get $subclasses))
      (global.get $BUNSPEC))

   ;; BGL_CLASS_DIRECT_FIELDS_SET
   (func $BGL_CLASS_DIRECT_FIELDS_SET (export "BGL_CLASS_DIRECT_FIELDS_SET")
      (param $class (ref $class))
      (param $direct_fields (ref $vector))
      (result (ref eq))
      (struct.set $class $direct_fields
	 (local.get $class) (local.get $direct_fields))
      (global.get $BUNSPEC))

   ;; BGL_CLASS_ALL_FIELDS_SET
   (func $BGL_CLASS_ALL_FIELDS_SET (export "BGL_CLASS_ALL_FIELDS_SET")
      (param $class (ref $class))
      (param $all_fields (ref $vector))
      (result (ref eq))
      (struct.set $class $all_fields
	 (local.get $class) (local.get $all_fields))
      (global.get $BUNSPEC))

   ;; BGL_CLASS_EVDATA_SET
   (func $BGL_CLASS_EVDATA_SET (export "BGL_CLASS_EVDATA_SET")
      (param $class (ref $class))
      (param $evdata (ref eq))
      (result (ref eq))
      (struct.set $class $evdata (local.get $class) (local.get $evdata))
      (global.get $BUNSPEC))

   ;; bgl_make_generic
   (func $bgl_make_generic (export "bgl_make_generic")
      (param $proc (ref $procedure))
      (result (ref $procedure))
      (local $arity i32)
      (local $res (ref $procedure))
      (local.set $arity (struct.get $procedure $arity (local.get $proc)))
      ;; dummy initialization
      (local.set $res (global.get $procedure-default-value))
      (block $done
	 (block $default
	    (block $-5
	       (block $-4
		  (block $-3
		     (block $-2
			(block $-1
			   (block $0
			      (block $1
				 (block $2
				    (block $3
				       (block $4
					  (block $5
					     (br_table $-5 $-4 $-3 $-2 $-1
						$0
						$1 $2 $3 $4 $5 $default
						(i32.add (local.get $arity)
						   (i32.const 5))))
					  ;; $5
					  (local.set $res
					     (call $MAKE_FX_PROCEDURE
						(ref.func $generic_entry5)
						(local.get $arity)
						(i32.const 4)))
					  (br $done))
				       ;; $4
				       (local.set $res
					  (call $MAKE_FX_PROCEDURE
					     (ref.func $generic_entry4)
					     (local.get $arity)
					     (i32.const 4)))
				       (br $done))
				    ;; $3
				    (local.set $res
				       (call $MAKE_FX_PROCEDURE
					  (ref.func $generic_entry3)
					  (local.get $arity)
					  (i32.const 4)))
				    (br $done))
				 ;; $2
				 (local.set $res
				    (call $MAKE_FX_PROCEDURE
				       (ref.func $generic_entry2)
				       (local.get $arity)
				       (i32.const 4)))
				 (br $done))
			      ;; $1
			      (local.set $res
				 (call $MAKE_FX_PROCEDURE
				    (ref.func $generic_entry1)
				    (local.get $arity)
				    (i32.const 4)))
			      (br $done))
			   ;; $0
			   (throw $fail)
			   (unreachable))
			;; $-1
			(throw $fail)
			(unreachable))
		     ;; $-2
		     (local.set $res
			(call $MAKE_VA_PROCEDURE
			   (ref.func $generic_entry2)
			   (local.get $arity)
			   (i32.const 4)))
		     (br $done))
		  ;; $-3
		  (local.set $res
		     (call $MAKE_VA_PROCEDURE
			(ref.func $generic_entry3)
			(local.get $arity)
			(i32.const 4)))
		  (br $done))
	       ;; $-4
	       (local.set $res
		  (call $MAKE_VA_PROCEDURE
		     (ref.func $generic_entry4)
		     (local.get $arity)
		     (i32.const 4)))
	       (br $done))
	    ;; $-5
	    (local.set $res
	       (call $MAKE_VA_PROCEDURE
		  (ref.func $generic_entry5)
		  (local.get $arity)
		  (i32.const 4)))
	    (br $done))
	 (if (i32.lt_s (local.get $arity) (i32.const 0))
	     (then
		(local.set $res
		   (call $MAKE_VA_PROCEDURE
		      (ref.func $generic_entry)
		      (local.get $arity)
		      (i32.const 4))))
	     (else
	      (local.set $res
		 (call $MAKE_FX_PROCEDURE
		    (ref.func $generic_entry)
		    (local.get $arity)
		    (i32.const 4))))))
      (call $PROCEDURE_SET (local.get $res) (i32.const 3) (local.get $proc))
      (return (local.get $res)))

   (func $generic_entry1
      (param $proc (ref $procedure))
      (param $a1 (ref eq))
      (result (ref eq))
      (return_call $funcall1
	 (ref.cast (ref $procedure)
	    (call $PROCEDURE_REF (local.get $proc) (i32.const 3)))
	 (local.get $a1)))

   (func $generic_entry2
      (param $proc (ref $procedure))
      (param $a1 (ref eq))
      (param $a2 (ref eq))
      (result (ref eq))
      (return_call $funcall2
	 (ref.cast (ref $procedure)
	    (call $PROCEDURE_REF (local.get $proc) (i32.const 3)))
	 (local.get $a1)
	 (local.get $a2)))

   (func $generic_entry3
      (param $proc (ref $procedure))
      (param $a1 (ref eq))
      (param $a2 (ref eq))
      (param $a3 (ref eq))
      (result (ref eq))
      (return_call $funcall3
	 (ref.cast (ref $procedure)
	    (call $PROCEDURE_REF (local.get $proc) (i32.const 3)))
	 (local.get $a1)
	 (local.get $a2)
	 (local.get $a3)))

   (func $generic_entry4
      (param $proc (ref $procedure))
      (param $a1 (ref eq))
      (param $a2 (ref eq))
      (param $a3 (ref eq))
      (param $a4 (ref eq))
      (result (ref eq))
      (return_call $funcall4
	 (ref.cast (ref $procedure)
	    (call $PROCEDURE_REF (local.get $proc) (i32.const 3)))
	 (local.get $a1)
	 (local.get $a2)
	 (local.get $a3)
	 (local.get $a4)))

   (func $generic_entry5
      (param $proc (ref $procedure))
      (param $a1 (ref eq))
      (param $a2 (ref eq))
      (param $a3 (ref eq))
      (param $a4 (ref eq))
      (param $a5 (ref eq))
      (result (ref eq))
      (return_call $funcall5
	 (ref.cast (ref $procedure)
	    (call $PROCEDURE_REF (local.get $proc) (i32.const 3)))
	 (local.get $a1)
	 (local.get $a2)
	 (local.get $a3)
	 (local.get $a4)
	 (local.get $a5)))

   (func $generic_entry
      (param $proc (ref $procedure))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   )

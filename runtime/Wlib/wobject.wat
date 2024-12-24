;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wobject.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:02:42 2024                          */
;*    Last change :  Sun Dec 22 07:10:44 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM objects and classes                                         */
;*=====================================================================*/

(module $__bigloo_object
   
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
      (if (i64.lt_u (local.get $depth) (i64.const 0))
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
   
   )

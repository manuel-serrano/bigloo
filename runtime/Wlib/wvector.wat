;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wvector.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 22 07:27:20 2024                          */
;*    Last change :  Tue Dec 24 09:47:48 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM vectors                                                     */
;*=====================================================================*/

(module $__bigloo_vector
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   (type $vector (array (mut (ref eq))))
   (type $hvector (array (mut f64)))
   (type $s8vector (array (mut i8)))
   (type $s16vector (array (mut i16)))
   (type $s32vector (array (mut i32)))
   (type $s64vector (array (mut i64)))
   (type $u8vector (array (mut i8)))
   (type $u16vector (array (mut i16)))
   (type $u32vector (array (mut i32)))
   (type $u64vector (array (mut i64)))
   (type $f32vector (array (mut f32)))
   (type $f64vector (array (mut f64)))
   
   (type $bvector (array (mut i32)))
   (type $lvector (array (mut i64)))
   (type $dvector (array (mut f64)))
   
   (type $tvector-i32 (array (mut i32)))
   (type $tvector-i64 (array (mut i64)))
   (type $tvector-f64 (array (mut f64)))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $vector-default-value
      (export "BGL_VECTOR_DEFAULT_VALUE") (ref $vector)
      (array.new_fixed $vector 0))
   (global $hvector-default-value
      (export "BGL_HVECTOR_DEFAULT_VALUE") (ref $hvector)
      (array.new_fixed $hvector 0))
   (global $u8vector-default-value
      (export "BGL_U8VECTOR_DEFAULT_VALUE") (ref $u8vector)
      (array.new_fixed $u8vector 0))
   (global $s8vector-default-value
      (export "BGL_S8VECTOR_DEFAULT_VALUE") (ref $s8vector)
      (array.new_fixed $s8vector 0))
   (global $u16vector-default-value
      (export "BGL_U16VECTOR_DEFAULT_VALUE") (ref $u16vector)
      (array.new_fixed $u16vector 0))
   (global $s16vector-default-value
      (export "BGL_S16VECTOR_DEFAULT_VALUE") (ref $s16vector)
      (array.new_fixed $s16vector 0))
   (global $u32vector-default-value
      (export "BGL_U32VECTOR_DEFAULT_VALUE") (ref $u32vector)
      (array.new_fixed $u32vector 0))
   (global $s32vector-default-value
      (export "BGL_S32VECTOR_DEFAULT_VALUE") (ref $s32vector)
      (array.new_fixed $s32vector 0))
   (global $u64vector-default-value
      (export "BGL_U64VECTOR_DEFAULT_VALUE") (ref $u64vector)
      (array.new_fixed $u64vector 0))
   (global $s64vector-default-value
      (export "BGL_S64VECTOR_DEFAULT_VALUE") (ref $s64vector)
      (array.new_fixed $s64vector 0))
   (global $f32vector-default-value
      (export "BGL_F32VECTOR_DEFAULT_VALUE") (ref $f32vector)
      (array.new_fixed $f32vector 0))
   (global $f64vector-default-value
      (export "BGL_F64VECTOR_DEFAULT_VALUE") (ref $f64vector)
      (array.new_fixed $f64vector 0))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   (func $bgl_fill_vector (export "bgl_fill_vector")
      (param $v (ref $vector))
      (param $start i64)
      (param $end i64)
      (param $o (ref eq))
      (result (ref eq))
      (array.fill $vector 
	 (ref.cast (ref $vector) (local.get $v)) ;; FIXME: remove the cast
	 (i32.wrap_i64 (local.get $start)) 
	 (local.get $o)
	 (i32.wrap_i64 (i64.sub (local.get $end) (local.get $start))))
      (global.get $BUNSPEC))
   
   ;; --------------------------------------------------------
   ;; Typed vector functions
   ;; --------------------------------------------------------
   
   ;; TODO: better implementation of tvector descr

   (func $BGL_HVECTOR_LENGTH (export "BGL_HVECTOR_LENGTH")
      (param $v arrayref)
      (result i64)
      (return (i64.extend_i32_u (array.len (local.get $v)))))
	 
   (global $tvector_descr_i8 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i16 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i32 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i64 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_f32 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_f64 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_eqref (mut (ref eq)) (global.get $BUNSPEC))
   
   (func $TVECTOR_DESCR (export "TVECTOR_DESCR")
      (param $v arrayref)
      (result (ref eq))
      (if (ref.test (ref $tvector-i32) (local.get $v))
	  (then (return (global.get $tvector_descr_i32))))
      (if (ref.test (ref $tvector-i64) (local.get $v))
	  (then (return (global.get $tvector_descr_i64))))
      (if (ref.test (ref $tvector-f64) (local.get $v))
	  (then (return (global.get $tvector_descr_f64))))
      (call $not_implemented (i32.const -1))
      (return (global.get $BUNSPEC)))
   
   (func $TVECTOR_DESCR_SET (export "TVECTOR_DESCR_SET")
      (param $v arrayref)
      (param $desc (ref eq))
      (result (ref eq))
      (if (ref.test (ref $tvector-i32) (local.get $v))
	  (then (global.set $tvector_descr_i32 (local.get $desc))))
      (if (ref.test (ref $tvector-i64) (local.get $v))
	  (then (global.set $tvector_descr_i64 (local.get $desc))))
      (if (ref.test (ref $tvector-f64) (local.get $v))
	  (then (global.set $tvector_descr_f64 (local.get $desc))))
      (local.get $desc))
   )

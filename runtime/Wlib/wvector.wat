;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wvector.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 22 07:27:20 2024                          */
;*    Last change :  Fri Jul 18 15:09:25 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM vectors                                                     */
;*=====================================================================*/

(module $__bigloo_vector
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   (type $vector (array (mut (ref eq))))
   (type $hvector (array (mut i8)))
   
   (rec
      (type $s8vector (array (mut i8)))
      (type $s16vector (array (mut i16)))
      (type $s32vector (array (mut i32)))
      (type $s64vector (array (mut i64))))
   (rec
      (type $__dummy_hvector (struct (field $dummy (ref $hvector))))
      (type $u8vector (array (mut i8)))
      (type $u16vector (array (mut i16)))
      (type $u32vector (array (mut i32)))
      (type $u64vector (array (mut i64))))
   
   (type $f32vector (array (mut f32)))
   (type $f64vector (array (mut f64)))

   (type $bvector (array (mut i32)))
   (type $lvector (array (mut i64)))
   (type $dvector (array (mut f64)))
   
   (type $tvector-i8 (array (mut i8)))
   (type $tvector-i16 (array (mut i16)))
   (type $tvector-i32 (array (mut i32)))
   (type $tvector-i64 (array (mut i64)))
   (type $tvector-f64 (array (mut f64)))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BEOF" (global $BEOF (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BOOLEANP" (func $BOOLEANP (param (ref eq)) (result i32)))

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
   ;; Predicates
   ;; -----------------------------------------------------------------

   (func $VECTORP (export "VECTORP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $vector) (local.get $o)))
  
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
   
   (global $s8vector_descr (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i8 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i16 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i32 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_i64 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_f64 (mut (ref eq)) (global.get $BUNSPEC))
   (global $tvector_descr_eqref (mut (ref eq)) (global.get $BUNSPEC))

   (func $BGL_HVECTORP (export "BGL_HVECTORP")
      (param $v (ref eq))
      (result i32)
      (if (ref.test (ref $f64vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $s64vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $u64vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $s32vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $u32vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $s16vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $u16vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $s8vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $u8vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $f32vector) (local.get $v))
	  (then (return (i32.const 1))))
      (return (i32.const 0)))

   (func $BGL_HVECTOR_IDENT (export "BGL_HVECTOR_IDENT")
      (param $v (ref eq))
      (result i32)
      (if (ref.test (ref $f64vector) (local.get $v))
	  (then (return (i32.const 9))))
      (if (ref.test (ref $s64vector) (local.get $v))
	  (then (return (i32.const 6))))
      (if (ref.test (ref $u64vector) (local.get $v))
	  (then (return (i32.const 7))))
      (if (ref.test (ref $s32vector) (local.get $v))
	  (then (return (i32.const 4))))
      (if (ref.test (ref $u32vector) (local.get $v))
	  (then (return (i32.const 5))))
      (if (ref.test (ref $s16vector) (local.get $v))
	  (then (return (i32.const 2))))
      (if (ref.test (ref $u16vector) (local.get $v))
	  (then (return (i32.const 3))))
      (if (ref.test (ref $s8vector) (local.get $v))
	  (then (return (i32.const 0))))
      (if (ref.test (ref $u8vector) (local.get $v))
	  (then (return (i32.const 1))))
      (if (ref.test (ref $f32vector) (local.get $v))
	  (then (return (i32.const 8))))
      (return (i32.const 0)))

   (func $BGL_HVECTOR_LENGTH (export "BGL_HVECTOR_LENGTH")
      (param $v (ref array))
      (result i64)
      (return (i64.extend_i32_s (array.len (local.get $v)))))
   
   (func $TVECTORP (export "TVECTORP")
      (param $v (ref eq))
      (result i32)
      (return
	 (if (result i32) (ref.test (ref array) (local.get $v))
	     (then (i32.eqz (call $BGL_HVECTORP (local.get $v))))
	     (else (i32.const 0)))))
	     
   (func $TVECTOR_DESCR (export "TVECTOR_DESCR")
      (param $v arrayref)
      (result (ref eq))
      (if (ref.test (ref $tvector-f64) (local.get $v))
	  (then (return (global.get $tvector_descr_f64))))
      (if (ref.test (ref $tvector-i64) (local.get $v))
	  (then (return (global.get $tvector_descr_i64))))
      (if (ref.test (ref $tvector-i32) (local.get $v))
	  (then (return (global.get $tvector_descr_i32))))
      (if (ref.test (ref $tvector-i16) (local.get $v))
	  (then (return (global.get $tvector_descr_i16))))
      (if (ref.test (ref $tvector-i8) (local.get $v))
	  (then (return (global.get $tvector_descr_i8))))
      (if (ref.test (ref $s8vector) (local.get $v))
	  (then (return (global.get $s8vector_descr))))
      (return (global.get $BUNSPEC)))
   
   (func $TVECTOR_DESCR_SET (export "TVECTOR_DESCR_SET")
      (param $v (ref array))
      (param $desc (ref eq))
      (result (ref eq))
      (if (ref.test (ref $tvector-f64) (local.get $v))
	  (then (global.set $tvector_descr_f64 (local.get $desc))))
      (if (ref.test (ref $tvector-i64) (local.get $v))
	  (then (global.set $tvector_descr_i64 (local.get $desc))))
      (if (ref.test (ref $tvector-i32) (local.get $v))
	  (then (global.set $tvector_descr_i32 (local.get $desc))))
      (if (ref.test (ref $tvector-i16) (local.get $v))
	  (then (global.set $tvector_descr_i16 (local.get $desc))))
      (if (ref.test (ref $tvector-i8) (local.get $v))
	  (then (global.set $tvector_descr_i8 (local.get $desc))))
      (if (ref.test (ref $s8vector) (local.get $v))
	  (then (global.set $s8vector_descr (local.get $desc))))
      (local.get $desc))

   (func $BGL_S8VECTOR_COPY (export "BGL_S8VECTOR_COPY")
      (param $target (ref $s8vector))
      (param $tstart i64)
      (param $source (ref $s8vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $s8vector $s8vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_U8VECTOR_COPY (export "BGL_U8VECTOR_COPY")
      (param $target (ref $u8vector))
      (param $tstart i64)
      (param $source (ref $u8vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $u8vector $u8vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_S16VECTOR_COPY (export "BGL_S16VECTOR_COPY")
      (param $target (ref $s16vector))
      (param $tstart i64)
      (param $source (ref $s16vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $s16vector $s16vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_U16VECTOR_COPY (export "BGL_U16VECTOR_COPY")
      (param $target (ref $u16vector))
      (param $tstart i64)
      (param $source (ref $u16vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $u16vector $u16vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_S32VECTOR_COPY (export "BGL_S32VECTOR_COPY")
      (param $target (ref $s32vector))
      (param $tstart i64)
      (param $source (ref $s32vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $s32vector $s32vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_U32VECTOR_COPY (export "BGL_U32VECTOR_COPY")
      (param $target (ref $u32vector))
      (param $tstart i64)
      (param $source (ref $u32vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $u32vector $u32vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_S64VECTOR_COPY (export "BGL_S64VECTOR_COPY")
      (param $target (ref $s64vector))
      (param $tstart i64)
      (param $source (ref $s64vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $s64vector $s64vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_U64VECTOR_COPY (export "BGL_U64VECTOR_COPY")
      (param $target (ref $u64vector))
      (param $tstart i64)
      (param $source (ref $u64vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $u64vector $u64vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_F32VECTOR_COPY (export "BGL_F32VECTOR_COPY")
      (param $target (ref $f32vector))
      (param $tstart i64)
      (param $source (ref $f32vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $f32vector $f32vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   (func $BGL_F64VECTOR_COPY (export "BGL_F64VECTOR_COPY")
      (param $target (ref $f64vector))
      (param $tstart i64)
      (param $source (ref $f64vector))
      (param $sstart i64)
      (param $send i64)
      (array.copy $f64vector $f64vector (local.get $target)
	 (i32.wrap_i64 (local.get $tstart))
	 (local.get $source)
	 (i32.wrap_i64 (local.get $sstart))
	 (i32.wrap_i64 (local.get $send))))
   
   )

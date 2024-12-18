;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wintegers.wat      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 09:16:42 2024                          */
;*    Last change :  Wed Dec 18 18:45:02 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM integers implementation independent (i.e., not fixnum).     */
;*=====================================================================*/

(module $__bigloo_integers
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $belong (struct (field $v i64)))
   (type $bllong (struct (field $v i64)))
   (type $bint8 (struct (field $v i8)))
   (type $buint8 (struct (field $v i8)))
   (type $bint16 (struct (field $v i16)))
   (type $buint16 (struct (field $v i16)))
   (type $bint32 (struct (field $v i32)))
   (type $buint32 (struct (field $v i32)))
   (type $bint64 (struct (field $v i64)))
   (type $buint64 (struct (field $v i64)))
   (type $buint (struct (field $v i64)))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $bint8-default-value
      (export "BGL_BINT8_DEFAULT_VALUE") (ref $bint8)
      (struct.new $bint8 (i32.const 0)))
   (global $bint16-default-value
      (export "BGL_BINT16_DEFAULT_VALUE") (ref $bint16)
      (struct.new $bint16 (i32.const 0)))
   (global $bint32-default-value
      (export "BGL_BINT32_DEFAULT_VALUE") (ref $bint32)
      (struct.new $bint32 (i32.const 0)))
   (global $bint64-default-value
      (export "BGL_BINT64_DEFAULT_VALUE") (ref $bint64)
      (struct.new $bint64 (i64.const 0)))
   (global $buint8-default-value
      (export "BGL_BUINT8_DEFAULT_VALUE") (ref $buint8)
      (struct.new $buint8 (i32.const 0)))
   (global $buint16-default-value
      (export "BGL_BUINT16_DEFAULT_VALUE") (ref $buint16)
      (struct.new $buint16 (i32.const 0)))
   (global $buint32-default-value
      (export "BGL_BUINT32_DEFAULT_VALUE") (ref $buint32)
      (struct.new $buint32 (i32.const 0)))
   (global $buint64-default-value
      (export "BGL_BUINT64_DEFAULT_VALUE") (ref $buint64)
      (struct.new $buint64 (i64.const 0)))
   (global $belong-default-value
      (export "BGL_BELONG_DEFAULT_VALUE") (ref $belong)
      (struct.new $belong (i64.const 0)))
   (global $bllong-default-value
      (export "BGL_BLLONG_DEFAULT_VALUE") (ref $bllong)
      (struct.new $bllong (i64.const 0)))
   
   (global $MAXVALELONG (export "MAXVALELONG") i64
      (i64.const 9223372036854775807))
   (global $MAXVALLLONG (export "MAXVALLLONG") i64
      (i64.const 9223372036854775807))
   
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   
   ;; make_belong
   (func $make_belong (export "make_belong")
      (param $x i64)
      (result (ref $belong))
      (struct.new $belong (local.get $x)))
   
   ;; make_bllong
   (func $make_bllong (export "make_bllong")
      (param $x i64)
      (result (ref $bllong))
      (struct.new $bllong (local.get $x)))
   
   )


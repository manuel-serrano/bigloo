;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wmmap.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 28 06:41:16 2024                          */
;*    Last change :  Thu Dec 26 08:50:35 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM mmap                                                        */
;*=====================================================================*/

(module $__bigloo_mmap
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   (type $mmap
      (struct
	 (field $bytes (ref $bstring))
	 (field $name (ref eq))
	 (field $rp (mut i64))
	 (field $wp (mut i64))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $mmap-default-value
      (export "BGL_MMAP_DEFAULT_VALUE") (ref $mmap)
      (struct.new $mmap
	 (global.get $bstring-default-value)
	 (global.get $BUNSPEC)
	 (i64.const 0)
	 (i64.const 0)))

   ;; -----------------------------------------------------------------
   ;; Macros
   ;; -----------------------------------------------------------------
   
   (func $BGL_MMAPP (export "BGL_MMAPP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $mmap) (local.get $o)))
   
   (func $BGL_MMAP_LENGTH (export "BGL_MMAP_LENGTH")
      (param $s (ref $mmap))
      (result i64)
      (return (call $STRING_LENGTH (struct.get $mmap $bytes (local.get $s)))))
   
   (func $BGL_MMAP_NAME (export "BGL_MMAP_NAME")
      (param $s (ref $mmap))
      (result (ref eq))
      (return (struct.get $mmap $bytes (local.get $s))))
   
   (func $BGL_MMAP_TO_STRING (export "BGL_MMAP_TO_STRING")
      (param $s (ref $mmap))
      (result (ref $bstring))
      (return (struct.get $mmap $bytes (local.get $s))))
   
   (func $BGL_MMAP_REF (export "BGL_MMAP_REF")
      (param $s (ref $mmap))
      (param $i i64)
      (result i32)
      (array.get $bstring (struct.get $mmap $bytes (local.get $s))
	 (i32.wrap_i64 (local.get $i))))

   (func $BGL_MMAP_SET (export "BGL_MMAP_SET")
      (param $s (ref $mmap))
      (param $i i64)
      (param $c i32)
      (result (ref eq))
      (array.set $bstring (struct.get $mmap $bytes (local.get $s))
	 (i32.wrap_i64 (local.get $i))
	 (local.get $c))
      (return (local.get $s)))

   (func $BGL_MMAP_RP_GET (export "BGL_MMAP_RP_GET")
      (param $s (ref $mmap))
      (result i64)
      (struct.get $mmap $rp (local.get $s)))
   
   (func $BGL_MMAP_RP_SET (export "BGL_MMAP_RP_SET")
      (param $s (ref $mmap))
      (param $i i64)
      (struct.set $mmap $rp (local.get $s) (local.get $i)))
   
   (func $BGL_MMAP_WP_GET (export "BGL_MMAP_WP_GET")
      (param $s (ref $mmap))
      (result i64)
      (struct.get $mmap $wp (local.get $s)))
   
   (func $BGL_MMAP_WP_SET (export "BGL_MMAP_WP_SET")
      (param $s (ref $mmap))
      (param $i i64)
      (struct.set $mmap $rp (local.get $s) (local.get $i)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (func $bgl_string_to_mmap (export "bgl_string_to_mmap")
      (param $s (ref $bstring))
      (param $r i32)
      (param $w i32)
      (result (ref $mmap))
      (struct.new $mmap
	 (local.get $s)
	 (local.get $s)
	 (i64.const 0)
	 (i64.const 0)))

   (func $bgl_close_mmap (export "bgl_close_mmap")
      (param $m (ref $mmap))
      (result (ref eq))
      (local.get $m))
	 
   )
  

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wmmap.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 28 06:41:16 2024                          */
;*    Last change :  Wed Jul 16 09:52:11 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM mmap                                                        */
;*=====================================================================*/

(module $__bigloo_mmap

   (memory 0)
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (rec
      (type $mmap
	 (sub
	    (struct
	       (field $name (ref eq))
	       (field $rp (mut i64))
	       (field $wp (mut i64)))))
      
      (type $mmap-string
	 (sub $mmap
	    (struct
	       (field $name (ref eq))
	       (field $rp (mut i64))
	       (field $wp (mut i64))
	       (field $bytes (ref $bstring)))))
      
      (type $mmap-file
	 (sub $mmap
	    (struct
	       (field $name (ref eq))
	       (field $rp (mut i64))
	       (field $wp (mut i64))
	       (field $length (mut i32))
	       (field $fin (mut i32))
	       (field $fout (mut i32))))))
   
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js_io" "open_file" (func $js_open_file (param i32 i32 i32) (result i32)))
   (import "__js_io" "close_file" (func $js_close_file (param i32)))
   (import "__js_io" "read_file" (func $js_read_file (param i32 i32 i32 i32) (result i32)))
   (import "__js_io" "file_size" (func $js_file_size (param i32) (result i32)))
   (import "__js_io" "write_char" (func $js_write_char (param i32 i32 i32) (result i32)))

   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "STRING_LENGTH" (func $STRING_LENGTH (param (ref $bstring)) (result i64)))
   (import "__bigloo" "bgl_load_string_in_buffer" (func $load_string_in_buffer (param i32) (param i32) (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))

   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $mmap-default-value
      (export "BGL_MMAP_DEFAULT_VALUE") (ref $mmap)
      (struct.new $mmap
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
   
   (func $BGL_MMAP_NAME (export "BGL_MMAP_NAME")
      (param $o (ref $mmap))
      (result (ref eq))
      (return (struct.get $mmap $name (local.get $o))))
   
   (func $BGL_MMAP_LENGTH (export "BGL_MMAP_LENGTH")
      (param $o (ref $mmap))
      (result i64)
      (if (ref.test (ref $mmap-string) (local.get $o))
	  (then
	     (return_call $STRING_LENGTH
		(struct.get $mmap-string $bytes
		   (ref.cast (ref $mmap-string) (local.get $o)))))
	  (else
	   (return
	      (i64.extend_i32_u
		 (struct.get $mmap-file $length
		    (ref.cast (ref $mmap-file) (local.get $o)))))))
      (unreachable))
   
   (func $BGL_MMAP_TO_STRING (export "BGL_MMAP_TO_STRING")
      (param $o (ref $mmap))
      (result (ref $bstring))
      
      (local $fd i32)
      (local $buf (ref $bstring))
      (local $len i32)
      (local $i i32)
      (local $r i32)
      
      (return
	 (if (result (ref $bstring)) (ref.test (ref $mmap-string) (local.get $o))
	     (then
		(struct.get $mmap-string $bytes
		   (ref.cast (ref $mmap-string) (local.get $o))))
	     (else
	      (local.set $fd
		 (struct.get $mmap-file $fin
		    (ref.cast (ref $mmap-file) (local.get $o))))
	      (local.set $len
		 (struct.get $mmap-file $length
		    (ref.cast (ref $mmap-file) (local.get $o))))
	      (local.set $buf
		 (array.new_default $bstring (local.get $len)))
	      
	      (loop $while
		 (if (i32.lt_s (local.get $i) (local.get $len))
		     (then
			(local.set $r
			   (call $js_read_file (local.get $fd)
			      (i32.const 128)
			      (i32.const 8192)
			      (local.get $i)))
			(call $load_string_in_buffer
			   (i32.const 128)
			   (local.get $r)
			   (local.get $buf)
			   (local.get $i))
			(local.set $i (i32.add (local.get $i) (local.get $r)))
			(br $while))))
	      
	      (local.get $buf)))))
   
   (func $BGL_MMAP_REF (export "BGL_MMAP_REF")
      (param $o (ref $mmap))
      (param $i i64)
      (result i32)

      (return
	 (if (result i32) (ref.test (ref $mmap-string) (local.get $o))
	     (then
		(array.get_u $bstring
		   (struct.get $mmap-string $bytes
		      (ref.cast (ref $mmap-string) (local.get $o)))
		   (i32.wrap_i64 (local.get $i))))
	     (else
	      (if (result i32)
		  (i32.eq
		     (call $js_read_file
			(struct.get $mmap-file $fin
			   (ref.cast (ref $mmap-file) (local.get $o)))
			(i32.const 128)
			(i32.const 1)
			(i32.wrap_i64 (local.get $i)))
		     (i32.const 1))
		  (then
		     (i32.load8_s (i32.const 128)))
		  (else
		   (i32.const -1)))))))

   (func $BGL_MMAP_SET (export "BGL_MMAP_SET")
      (param $o (ref $mmap))
      (param $i i64)
      (param $c i32)
      (result (ref eq))
      (if (ref.test (ref $mmap-string) (local.get $o))
	  (then
	     (array.set $bstring
		(struct.get $mmap-string $bytes
		   (ref.cast (ref $mmap-string) (local.get $o)))
		(i32.wrap_i64 (local.get $i))
		(local.get $c)))
	  (else
	   (drop
	      (call $js_write_char
		 (struct.get $mmap-file $fout
		    (ref.cast (ref $mmap-file) (local.get $o)))
		 (local.get $c)
		 (i32.wrap_i64 (local.get $i))))))
      (return (local.get $o)))

   (func $BGL_MMAP_RP_GET (export "BGL_MMAP_RP_GET")
      (param $o (ref $mmap))
      (result i64)
      (struct.get $mmap $rp (local.get $o)))
   
   (func $BGL_MMAP_RP_SET (export "BGL_MMAP_RP_SET")
      (param $o (ref $mmap))
      (param $i i64)
      (struct.set $mmap $rp (local.get $o) (local.get $i)))
   
   (func $BGL_MMAP_WP_GET (export "BGL_MMAP_WP_GET")
      (param $o (ref $mmap))
      (result i64)
      (struct.get $mmap $wp (local.get $o)))
   
   (func $BGL_MMAP_WP_SET (export "BGL_MMAP_WP_SET")
      (param $o (ref $mmap))
      (param $i i64)
      (struct.set $mmap $wp (local.get $o) (local.get $i)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

   (func $bgl_open_mmap
      (export "bgl_open_mmap")
      (param $fname (ref $bstring))
      (param $r i32)
      (param $w i32)
      (result (ref $mmap))
      
      (local $mmap (ref $mmap-file))
      (local.set $mmap
	 (struct.new $mmap-file
	    (local.get $fname)
	    (i64.const 0)
	    (i64.const 0)
	    (i32.const -1)
	    (i32.const -1)
	    (i32.const -1)))
      
      (if (local.get $r)
	  (then
	     (call $store_string (local.get $fname) (i32.const 128))
	     (struct.set $mmap-file $fin (local.get $mmap)
		(call $js_open_file
		   (i32.const 128)
		   (array.len (local.get $fname))
		   (i32.const 0)))
	     (struct.set $mmap-file $length (local.get $mmap)
		(call $js_file_size
		   (struct.get $mmap-file $fin (local.get $mmap))))))
      (if (local.get $w)
	  (then
	     (call $store_string (local.get $fname) (i32.const 128))
	     (struct.set $mmap-file $fout (local.get $mmap)
		(call $js_open_file
		   (i32.const 128)
		   (array.len (local.get $fname))
		   (i32.const 2)))))
      
      (return (local.get $mmap)))
   
   (func $bgl_string_to_mmap (export "bgl_string_to_mmap")
      (param $s (ref $bstring))
      (param $r i32)
      (param $w i32)
      (result (ref $mmap))
      (struct.new $mmap-string
	 (local.get $s)
	 (i64.const 0)
	 (i64.const 0)
	 (local.get $s)))

   (func $bgl_close_mmap (export "bgl_close_mmap")
      (param $o (ref $mmap))
      (result (ref eq))
      (if (ref.test (ref $mmap-file) (local.get $o))
	  (then
	     (if (i32.ge_s (struct.get $mmap-file $fin
			      (ref.cast (ref $mmap-file) (local.get $o)))
		    (i32.const 0))
		 (then
		    (call $js_close_file
		       (struct.get $mmap-file $fin
			  (ref.cast (ref $mmap-file) (local.get $o))))
		    (struct.set $mmap-file $fin
		       (ref.cast (ref $mmap-file) (local.get $o))
		       (i32.const -1))))
	     (if (i32.ge_s (struct.get $mmap-file $fout
			      (ref.cast (ref $mmap-file) (local.get $o)))
		    (i32.const 0))
		 (then
		    (call $js_close_file
		       (struct.get $mmap-file $fout
			  (ref.cast (ref $mmap-file) (local.get $o))))
		    (struct.set $mmap-file $fin
		       (ref.cast (ref $mmap-file) (local.get $o))
		       (i32.const -1))))))

      (global.get $BTRUE))
	 
   )
  

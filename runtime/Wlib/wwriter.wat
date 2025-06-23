;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wwriter.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 07:31:11 2024                          */
;*    Last change :  Sun Jun 22 09:40:50 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Native objects printing.                                         */
;*=====================================================================*/

(module $__bigloo_writer

   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------

    ;; PUTC
   (func $PUTC
      (param $c i32)
      (param $op (ref $output-port))
      (result (ref eq))

      (local $buf (ref $bstring))
      (local $index i32)
      
      (local.set $index (struct.get $output-port $index (local.get $op)))
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      
      (if (i32.ge_s (local.get $index) (array.len (local.get $buf)))
	  (then
	     (return_call $bgl_output_flush_char (local.get $op)
		(local.get $c)))
	  (else
	   (array.set $bstring (local.get $buf)
	      (local.get $index) (local.get $c))
	   (struct.set $output-port $index (local.get $op)
	      (i32.add (local.get $index) (i32.const 1)))
	   (return (local.get $op)))))

   ;; PUTS
   (func $PUTS
      (param $s (ref $bstring))
      (param $op (ref $output-port))

      (local $buf (ref $bstring))
      (local $index i32)
      (local $sz i32)
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $index (struct.get $output-port $index (local.get $op)))
      (local.set $sz (array.len (local.get $s)))

      (if (i32.lt_u (i32.add (local.get $index) (local.get $sz))
	     (array.len (local.get $buf)))
	  (then
	     (array.copy $bstring $bstring
		(local.get $buf)
		(local.get $index)
		(local.get $s)
		(i32.const 0)
		(local.get $sz))
	     (struct.set $output-port $index (local.get $op)
		(i32.add (struct.get $output-port $index (local.get $op))
		   (local.get $sz))))
	  (else
	   (drop
	      (call $bgl_output_flush (local.get $op)
		 (local.get $s) (i32.const 0) (local.get $sz))))))
	     
   ;; bgl_display_substring
   (func $bgl_display_substring (export "bgl_display_substring")
      (param $o (ref $bstring))
      (param $start i64)
      (param $end i64)
      (param $op (ref $output-port))
      (result (ref eq))

      (return_call $bgl_write (local.get $op)
	 (local.get $o) (i32.wrap_i64 (local.get $start))
	 (i32.wrap_i64 (i64.sub (local.get $end) (local.get $start)))))
	 
   ;; bgl_display_string
   (func $bgl_display_string (export "bgl_display_string")
      (param $o (ref $bstring))
      (param $op (ref $output-port))
      (result (ref eq))

      (return_call $bgl_write (local.get $op)
	 (local.get $o) (i32.const 0)
	 (array.len (local.get $o))))

   ;; bgl_write_string
   (func $bgl_write_string (export "bgl_write_string")
      (param $o (ref $bstring))
      (param $esc i32)
      (param $op (ref $output-port))
      (result (ref eq))

      (if (local.get $esc)
	  (then (drop (call $PUTC (i32.const 35) (local.get $op)))))

      (drop (call $PUTC (i32.const 34) (local.get $op)))
      (drop (call $bgl_write (local.get $op) (local.get $o)
	      (i32.const 0)
	      (array.len (local.get $o))))
      (drop (call $PUTC (i32.const 34) (local.get $op)))

      (return (local.get $op)))

   ;; bgl_display_fixnum, see output-generic.sch
   ;; bgl_display_elong, see output-generic.sch
   ;; bgl_write_elong, see output-generic.sch
   ;; bgl_display_llong, see output-generic.sch
   ;; bgl_write_llong, see output-generic.sch
   ;; bgl_display_bignum, see output-generic.sch
   ;; bgl_write_bignum, see output-generic.sch

   ;; bgl_write_char, see output-generic.sch
   ;; bgl_display_char
   (func $bgl_display_char (export "bgl_display_char")
      (param $c i32)
      (param $op (ref $output-port))
      (result (ref eq))
      (return_call $PUTC (local.get $c) (local.get $op)))

   )

      

      

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/writer.wat         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 07:31:11 2024                          */
;*    Last change :  Fri Sep 27 07:42:49 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Native objects printing.                                         */
;*=====================================================================*/

(module $__runtime_writer

   ;; bgl_display_char
   (func $bgl_display_char (export "bgl_display_char")
      (param $c i32)
      (param $op (ref $output-port))
      (result (ref eq))
      (local $new_buffer (ref $bstring))
      (local $buffer (ref $bstring))
      (local $index i32)
      (local $len i32)
      
      (local.set $buffer
	 (struct.get $output-port $buffer (local.get $op)))
      (local.set $index
	 (struct.get $output-port $index (local.get $op)))
      (local.set $len
	 (array.len (local.get $buffer)))
      
      (if (i32.lt_u (local.get $index) (local.get $len))
	  (then
	     (array.set $bstring (local.get $buffer) (local.get $index)
		(local.get $c))
	     (struct.set $output-port $index (local.get $op)
		(i32.add (local.get $index) (i32.const 1))))
	  (else
	   (if (ref.test (ref $file-output-port) (local.get $op))
	       (then
		  (call $flush_file_output_port
		     (ref.cast (ref $file-output-port) (local.get $op)))
		  (call $js_write_char
		     (struct.get $file-output-port $fd
			(ref.cast (ref $file-output-port) (local.get $op)))
		     (local.get $c)))
	       (else
		;; allocate space for new buffer
		(local.set $new_buffer
		   (array.new_default $bstring
		      (i32.add
			 (array.len (local.get $buffer))
			 (i32.const 16))))
		;; copy data to new buffer
		(array.copy $bstring $bstring
		   (local.get $new_buffer)
		   (i32.const 0)
		   (local.get $buffer)
		   (i32.const 0)
		   (local.get $len))
		(array.set $bstring
		   (local.get $new_buffer)
		   (local.get $len)
		   (local.get $c))
		(struct.set $string-output-port $buffer
		   (ref.cast (ref $string-output-port) (local.get $op))
		   (local.get $new_buffer))))))
      (local.get $op))

   ;; bgl_display_bignum
   (func $bgl_display_bignum (export "bgl_display_bignum")
      (param $n (ref $bignum))
      (param $port (ref $output-port))
      (result (ref eq))
      (if (ref.test (ref $file-output-port) (local.get $port))
	  (then
	     (call $js_write_bignum
		(struct.get $file-output-port $fd
		   (ref.cast (ref $file-output-port) (local.get $port)))
		(struct.get $bignum $bx (local.get $n)))
	     (return (local.get $port)))
	  (else
	   (return_call $bgl_display_string
	      (call $load_string
		 (i32.const 128)
		 (call $bignum_to_string
		    (struct.get $bignum $bx (local.get $n))
		    (i32.const 128)))
	      (local.get $port)))))

  ;; display_substring_file_port
  (func $display_substring_file_port
     (param $text (ref $bstring))
     (param $start i32)
     (param $end i32)
     (param $port (ref $file-output-port))
     (local $buffer (ref $bstring))
     (local $index i32)
     (local $len i32)
     (local $size i32)
     
     (local.set $buffer
	(struct.get $output-port $buffer (local.get $port)))
     (local.set $index
	(struct.get $output-port $index (local.get $port)))
     (local.set $len
	(array.len (local.get $buffer)))
     (local.set $size
	(i32.sub (local.get $end) (local.get $start)))

     (if (i32.lt_u (i32.add (local.get $index) (local.get $size))
	    (local.get $len))
	 (then
	    (array.copy $bstring $bstring
	       (local.get $buffer)
	       (local.get $index)
	       (local.get $text)
	       (local.get $start)
	       (local.get $size))
	    (struct.set $file-output-port $index
	       (local.get $port)
	       (i32.add (local.get $index) (local.get $size))))
	 (else
	  (call $flush_file_output_port (local.get $port))
	  (call $memcpy
	     (i32.const 128)
	       (local.get $text)
	       (local.get $start)
	       (local.get $size))
	  (call $js_write_file
	     (struct.get $file-output-port $fd (local.get $port))
	     (i32.const 128)
	     (local.get $size)))))

  ;; display_substring_string_port
  (func $display_substring_string_port
     (param $text (ref $bstring))
     (param $start i32)
     (param $end i32)
     (param $port (ref $string-output-port))
     
     (local $length i32)
     (local $new_buffer (ref $bstring))
     
     ;; allocate space for new buffer.
     (local.set $length (i32.sub (local.get $end) (local.get $start)))
     (local.set $new_buffer
	(array.new_default $bstring
	   (i32.add 
	      (array.len
		 (struct.get $string-output-port
		    $buffer (local.get $port)))
	      (local.get $length))))
     
     ;; Copy data to new buffer.
     (array.copy $bstring $bstring 
	(local.get $new_buffer) (i32.const 0) 
	(struct.get $string-output-port
	   $buffer (local.get $port)) (i32.const 0)
	(local.get $length))
     (array.copy $bstring $bstring
	(local.get $new_buffer) (local.get $length)
	(local.get $text) (local.get $start)
	(local.get $length))
     
     (struct.set $string-output-port $buffer
	(local.get $port)
	(local.get $new_buffer)))


   ;; bgl_display_substring
   (func $bgl_display_substring (export "bgl_display_substring")
      (param $text (ref $bstring))
      (param $start i64)
      (param $end i64)
      (param $port (ref $output-port))
      (result (ref eq))
      
      (if (ref.test (ref $file-output-port) (local.get $port))
	  (then 
	     (call $display_substring_file_port 
		(ref.cast (ref $bstring) (local.get $text))
		(i32.wrap_i64 (local.get $start))
		(i32.wrap_i64 (local.get $end))
		(ref.cast (ref $file-output-port) (local.get $port))))
	  (else
	   (call $display_substring_string_port 
	      (ref.cast (ref $bstring) (local.get $text))
	      (i32.wrap_i64 (local.get $start))
	      (i32.wrap_i64 (local.get $end))
	      (ref.cast (ref $string-output-port) (local.get $port)))))
      
      (local.get $port))

   ;; bgl_display_string
   (func $bgl_display_string (export "bgl_display_string")
      (param $text (ref $bstring))
      (param $port (ref $output-port))
      (result (ref eq))
      (call $bgl_display_substring
	 (local.get $text)
	 (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $text)))
	 (local.get $port)))


   ;; bgl_write_input_port
   (func $bgl_write_input_port (export "bgl_write_input_port")
      (param o (ref $input_port))
      (param op (ref $output_port))

      

      

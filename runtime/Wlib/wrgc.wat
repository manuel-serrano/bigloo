;*=====================================================================*/
;*    /priv/serrano2/bigloo/wasm/runtime/Wlib/wrgc.wat                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 08:51:40 2024                          */
;*    Last change :  Mon Sep 30 08:58:14 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM rgc                                                         */
;*=====================================================================*/

(module $__runtime_rgc
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   ;; rgc
   (type $rgc
      (struct
	 (field $eof (mut i32))
	 (field $filepos (mut i32))
	 (field $forward (mut i32))
	 (field $bufpos (mut i32))
	 (field $matchstart (mut i32))
	 (field $matchstop (mut i32))
	 (field $lastchar (mut i32))
	 (field $buf (mut (ref $bstring)))))
   
   (global $rgc-default-value
      (export "BGL_RGC_DEFAULT_VALUE") (ref $rgc)
      (struct.new $rgc
	 ;; eof
	 (i32.const 0)
	 ;; filepos
	 (i32.const 0)
	 ;; forward
	 (i32.const 0)
	 ;; bufpos
	 (i32.const 0)
	 ;; matchstart
	 (i32.const 0)
	 ;; matchstop
	 (i32.const 0)
	 ;; lastchar
	 (i32.const 0)
	 ;; buffer
	 (global.get $bstring-default-value)))
   
   ;; -----------------------------------------------------------------
   ;; Common macros
   ;; -----------------------------------------------------------------
   (func $RGC_BUFFER_GET_CHAR (export "RGC_BUFFER_GET_CHAR")
      (param $port (ref $input-port))
      (param $index i64)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get $bstring 
	 (struct.get $rgc $buf (local.get $rgc)) 
	 (i32.wrap_i64 (local.get $index))))
   
   (func $RGC_BUFFER_MATCH_LENGTH (export "RGC_BUFFER_MATCH_LENGTH")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.extend_i32_u (i32.sub
			   (struct.get $rgc $matchstop (local.get $rgc)) 
			   (struct.get $rgc $matchstart (local.get $rgc)))))
   
   (func $RGC_SET_FILEPOS (export "RGC_SET_FILEPOS")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (struct.set $rgc $filepos (local.get $rgc)
	 (i32.add
	    (struct.get $rgc $filepos (local.get $rgc))
	    (i32.sub
	       (struct.get $rgc $matchstop (local.get $rgc))
	       (struct.get $rgc $matchstart (local.get $rgc)))))
      (i64.extend_i32_u (struct.get $rgc $filepos (local.get $rgc))))
   
   (func $RGC_START_MATCH (export "RGC_START_MATCH")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (struct.set $rgc $matchstart (local.get $rgc)
	 (struct.get $rgc $matchstop (local.get $rgc)))
      (struct.set $rgc $forward (local.get $rgc)
	 (struct.get $rgc $matchstop (local.get $rgc)))
      (i64.extend_i32_u (struct.get $rgc $matchstop (local.get $rgc))))
   
   (func $RGC_STOP_MATCH (export "RGC_STOP_MATCH")
      (param $port (ref $input-port))
      (param $forward i64)
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (struct.set $rgc $matchstop (local.get $rgc)
	 (i32.wrap_i64 (local.get $forward)))
      (local.get $forward))
   
   (func $RGC_BUFFER_POSITION (export "RGC_BUFFER_POSITION")
      (param $port (ref $input-port))
      (param $forward i64)
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.sub
	 (local.get $forward)
	 (i64.extend_i32_u (struct.get $rgc $matchstart (local.get $rgc)))))
   
   (func $RGC_BUFFER_FORWARD (export "RGC_BUFFER_FORWARD")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.extend_i32_u (struct.get $rgc $forward (local.get $rgc))))
   
   (func $RGC_BUFFER_BUFPOS (export "RGC_BUFFER_BUFPOS")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.extend_i32_u (struct.get $rgc $bufpos (local.get $rgc))))
   
   (func $RGC_BUFFER_CHARACTER (export "RGC_BUFFER_CHARACTER")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (struct.get $rgc $matchstart (local.get $rgc))))
   
   (func $RGC_BUFFER_BYTE (export "RGC_BUFFER_BYTE")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (struct.get $rgc $matchstart (local.get $rgc))))
   
   (func $RGC_BUFFER_BYTE_REF (export "RGC_BUFFER_BYTE_REF")
      (param $port (ref $input-port))
      (param $offset i32)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (i32.add
	    (struct.get $rgc $matchstart (local.get $rgc))
	    (local.get $offset))))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   ;; rgc_buffer_substring
   (func $rgc_buffer_substring (export "rgc_buffer_substring")
      (param $port (ref $input-port))
      (param $offset i64)
      (param $end i64)
      (result (ref $bstring))
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (call $c_substring
	 (struct.get $rgc $buf (local.get $rgc))
	 (i64.add
	    (i64.extend_i32_u (struct.get $rgc $matchstart (local.get $rgc)))
	    (local.get $offset))
	 (i64.add
	    (i64.extend_i32_u (struct.get $rgc $matchstart (local.get $rgc)))
	    (local.get $end))))
   
   ;; rgc_buffer_unget_char
   (func $rgc_buffer_unget_char (export "rgc_buffer_unget_char")
      (param $port (ref $input-port))
      (param $c i32)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (struct.set $rgc $filepos (local.get $rgc)
	 (i32.sub
	    (struct.get $rgc $filepos (local.get $rgc))
	    (i32.const 1)))
      
      (if (i32.lt_u (i32.const 0)
	     (struct.get $rgc $matchstop (local.get $rgc)))
	  (then
	     (struct.set $rgc $matchstop (local.get $rgc)
		(i32.sub
		   (struct.get $rgc $matchstop (local.get $rgc))
		   (i32.const 1))))
	  (else
	   (array.set $bstring 
	      (struct.get $rgc $buf (local.get $rgc))
	      (i32.const 0)
	      (local.get $c))))
      (local.get $c))
   
   ;; rgc_buffer_bol_p
   (func $rgc_buffer_bol_p (export "rgc_buffer_bol_p")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (if (result i32)
	  (i32.gt_u 
	     (struct.get $rgc $matchstart (local.get $rgc))
	     (i32.const 0))
	  (then
	     (i32.eq
		(array.get $bstring
		   (struct.get $rgc $buf (local.get $rgc))
		   (i32.sub
		      (struct.get $rgc $matchstart (local.get $rgc))
		      (i32.const 1)))
		(i32.const 0x0A #;(ASCII NEWLINE '\n'))))
	  (else
	   (i32.eq
	      (struct.get $rgc $lastchar (local.get $rgc))
	      (i32.const 0x0A #;(ASCII NEWLINE '\n'))))))
   
   ;; rgc_buffer_eol_p
   (func $rgc_buffer_eol_p (export "rgc_buffer_eol_p")
      (param $port (ref $input-port))
      (param $forward i64)
      (param $bufpos i64)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (if (result i32)
	  (i64.eq (local.get $forward) (local.get $bufpos))
	  (then
	     (if (result i32)
		 (call $rgc_fill_buffer (local.get $port))
		 (then
		    (return_call $rgc_buffer_eol_p 
		       (local.get $port)
		       (i64.extend_i32_u (struct.get $rgc $forward (local.get $rgc)))
		       (i64.extend_i32_u (struct.get $rgc $bufpos (local.get $rgc)))))
		 (else
		  (i32.const 0 #;FALSE))))
	  (else
	   (struct.set $rgc $forward (local.get $rgc) (i32.wrap_i64 (local.get $forward)))
	   (struct.set $rgc $bufpos (local.get $rgc) (i32.wrap_i64 (local.get $bufpos)))
	   (i32.eq
	      (array.get $bstring 
		 (struct.get $rgc $buf (local.get $rgc)) 
		 (i32.wrap_i64 (local.get $forward)))
	      (i32.const 0x0A #;(ASCII NEWLINE '\n'))))))
   
   ;; rgc_buffer_bof_p
   (func $rgc_buffer_bof_p (export "rgc_buffer_bof_p")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i32.eqz (struct.get $rgc $filepos (local.get $rgc))))
   
   ;; rgc_buffer_eof_p
   (func $rgc_buffer_eof_p (export "rgc_buffer_eof_p")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i32.and
	 (struct.get $rgc $eof (local.get $rgc))
	 (i32.eq
	    (struct.get $rgc $matchstop (local.get $rgc))
	    (struct.get $rgc $bufpos (local.get $rgc)))))
   
   ;; rgc_buffer_eof2_p
   (func $rgc_buffer_eof2_p (export "rgc_buffer_eof2_p")
      (param $port (ref $input-port))
      (param $forward i64)
      (param $bufpos i64)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (if (result i32)
	  (i64.lt_u (local.get $forward) (local.get $bufpos))
	  (then
	     (struct.set $rgc $forward (local.get $rgc)
		(i32.wrap_i64 (local.get $forward)))
	     (struct.set $rgc $bufpos (local.get $rgc)
		(i32.wrap_i64 (local.get $bufpos)))
	     (i32.const 0 #;FALSE))
	  (else
	   (if (result i32)
	       (struct.get $rgc $eof (local.get $rgc))
	       (then
		  (struct.set $rgc $forward (local.get $rgc)
		     (i32.wrap_i64 (local.get $forward)))
		  (struct.set $rgc $bufpos (local.get $rgc)
		     (i32.wrap_i64 (local.get $bufpos)))
		  (i32.const 1 #;TRUE))
	       (else
		;; NOT (rgc_fill_buffer(port))
		(i32.sub
		   (i32.const 1)
		   (call $rgc_fill_buffer (local.get $port))))))))
   
   ;; rgc_double_buffer
   (func $rgc_double_buffer
      (param $rgc (ref $rgc))
      (local $buffer (ref $bstring))
      (local.set $buffer (struct.get $rgc $buf (local.get $rgc)))
      (struct.set $rgc $buf 
	 (local.get $rgc)
	 (array.new_default $bstring 
	    (i32.mul (array.len (local.get $buffer)) (i32.const 2))))
      (array.copy $bstring $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (i32.const 0)
	 (local.get $buffer)
	 (i32.const 0)
	 (array.len (local.get $buffer))))
   
   ;; rgc_size_fill_file_buffer
   (func $rgc_size_fill_file_buffer
      (param $rgc (ref $rgc))
      (param $fd i32)
      (param $bufpos i32)
      (param $size i32)
      (result i32)
      (local $nbread i32)
      (local.set $nbread
	 (call $js_read_file
	    (local.get $fd)
	    (i32.const 128)
	    (local.get $size)))
      (if (i32.le_s (local.get $nbread) (i32.const 0))
	  ;; TODO: emit exceptions in case of error (when nbread < 0)
	  (then
	     (struct.set $rgc $eof (local.get $rgc) (i32.const 1 #;TRUE)))
	  (else
	   (call $load_string_in_buffer
	      (i32.const 128)
	      (local.get $nbread)
	      (struct.get $rgc $buf (local.get $rgc))
	      (local.get $bufpos))
	   (local.set $bufpos
	      (i32.add (local.get $bufpos) (local.get $nbread)))))
      
      (struct.set $rgc $bufpos (local.get $rgc) (local.get $bufpos))
      
      (if (result i32)
	  (i32.le_s (local.get $nbread) (i32.const 0))
	  (then (i32.const 0 #;FALSE))
	  (else (i32.const 1 #;TRUE))))
   
   ;; rgc_fill_file_buffer
   (func $rgc_fill_file_buffer
      (param $port (ref $file-input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local $bufsize i32)
      (local $bufpos i32)
      (local $matchstart i32)
      (local $movesize i32)
      (local $i i32)
      (local $buffer (ref $bstring))
      
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (local.set $bufsize (array.len (struct.get $rgc $buf (local.get $rgc))))
      (local.set $bufpos (struct.get $rgc $bufpos (local.get $rgc)))
      (local.set $matchstart (struct.get $rgc $matchstart (local.get $rgc)))
      (local.set $buffer (struct.get $rgc $buf (local.get $rgc)))
      
      (if (i32.gt_u (local.get $matchstart) (i32.const 0))
	  (then
	     (local.set $movesize
		(i32.sub (local.get $bufpos) (local.get $matchstart)))
	     
	     (local.set $i (i32.const 0))
	     (loop $for_loop
		(if (i32.lt_u (local.get $i) (local.get $movesize))
		    (then
		       (array.set $bstring 
			  (local.get $buffer) 
			  (local.get $i) 
			  (array.get $bstring
			     (local.get $buffer)
			     (i32.add 
				(local.get $matchstart) 
				(local.get $i)))
			  (local.set $i (i32.add (local.get $i) (i32.const 1)))
			  (br $for_loop)))))
	     (local.set $bufpos
		(i32.sub (local.get $bufpos) (local.get $matchstart)))
	     (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
	     (struct.set $rgc $matchstop (local.get $rgc) 
		(i32.sub 
		   (struct.get $rgc $matchstop (local.get $rgc))
		   (local.get $matchstart)))
	     (struct.set $rgc $forward (local.get $rgc) 
		(i32.sub 
		   (struct.get $rgc $forward (local.get $rgc))
		   (local.get $matchstart)))
	     (struct.set $rgc $matchstart (local.get $rgc)
		(array.get $bstring (local.get $buffer)
		   (i32.sub (local.get $matchstart) (i32.const 1))))
	     
	     (return_call $rgc_size_fill_file_buffer
		(local.get $rgc)
		(struct.get $file-input-port $fd (local.get $port))
		(local.get $bufpos) 
		(i32.sub (local.get $bufsize) (local.get $bufpos)))))
      
      (if (i32.lt_u (local.get $bufpos) (local.get $bufsize))
	  (then 
	     (return_call $rgc_size_fill_file_buffer 
		(local.get $rgc)
		(struct.get $file-input-port $fd (local.get $port))
		(local.get $bufpos) 
		(i32.sub (local.get $bufsize) (local.get $bufpos)))))
      
      (call $rgc_double_buffer (local.get $rgc))
      (return_call $rgc_fill_file_buffer (local.get $port)))
   
   ;; rgc_fill_buffer
   (func $rgc_fill_buffer (export "rgc_fill_buffer")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (struct.set $rgc $forward (local.get $rgc)
	 (struct.get $rgc $bufpos (local.get $rgc)))
      (if (struct.get $rgc $eof (local.get $rgc))
	  (then (return (i32.const 0 #;FALSE))))
      
      (if (ref.test (ref $file-input-port) (local.get $port))
	  (then 
	     (return_call $rgc_fill_file_buffer 
		(ref.cast (ref $file-input-port) (local.get $port)))))
      
      (i32.const 1 #;TRUE))
   
   ;; rgc_file_charready
   (func $rgc_file_charready
      (param $port (ref $file-input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (if (struct.get $rgc $eof (local.get $rgc))
	  (then (return (i32.const 0 #;FALSE))))
      
      ;; FIXME: in java we also check the file position
      (i32.lt_u 
	 (i32.add 
	    (struct.get $rgc $forward (local.get $rgc)) 
	    (i32.const 1))
	 (struct.get $rgc $bufpos (local.get $rgc))))
   
   ;; bgl_rgc_charready
   (func $bgl_rgc_charready (export "bgl_rgc_charready")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (if (ref.test (ref $file-input-port) (local.get $port))
	  (then (return_call $rgc_file_charready
		   (ref.cast (ref $file-input-port) (local.get $port)))))
      
      (i32.lt_u (struct.get $rgc $forward (local.get $rgc))
	 (struct.get $rgc $bufpos (local.get $rgc))))
   
   )


   

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wrgc.wat           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 08:51:40 2024                          */
;*    Last change :  Fri Jan 10 09:43:01 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM rgc                                                         */
;*=====================================================================*/

(module $__runtime_rgc
   
   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $READ "read")
   (data $IO_ERROR "IO error")
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   ;; rgc
   (type $rgc
      (struct
	 (field $eof (mut i32))
	 (field $filepos (mut i32))
	 (field $fillbarrier (mut i32))
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
	 ;; fillbarrier
	 (i32.const -1)
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
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__bigloo" "default_io_bufsize" (global $default_io_bufsize i64))

   
   ;; -----------------------------------------------------------------
   ;; Common macros
   ;; -----------------------------------------------------------------
   (func $RGC_BUFFER_SET
      (param $rgc (ref $rgc))
      (param $offset i32)
      (param $c i32)

      (array.set $bstring 
	 (struct.get $rgc $buf (local.get $rgc)) 
	 (local.get $offset)
	 (local.get $c)))

   (func $RGC_BUFFER_SIZE
      (param $rgc (ref $rgc))
      (result i32)
      (array.len (struct.get $rgc $buf (local.get $rgc))))
		    
   (func $RGC_BUFFER_GET_CHAR (export "RGC_BUFFER_GET_CHAR")
      (param $port (ref $input-port))
      (param $index i64)
      (result i32)
      
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (array.get_u $bstring 
	 (struct.get $rgc $buf (local.get $rgc)) 
	 (i32.wrap_i64 (local.get $index))))
   
   (func $RGC_BUFFER_MATCH_LENGTH (export "RGC_BUFFER_MATCH_LENGTH")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.extend_i32_u
	 (i32.sub
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

   (func $RGC_MATCHSTART (export "RGC_MATCHSTART")
      (param $port (ref $input-port))
      (result i64)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i64.extend_i32_u (struct.get $rgc $matchstart (local.get $rgc))))
      
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
      (array.get_u $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (struct.get $rgc $matchstart (local.get $rgc))))
   
   (func $RGC_BUFFER_BYTE (export "RGC_BUFFER_BYTE")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get_u $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (struct.get $rgc $matchstart (local.get $rgc))))
   
   (func $RGC_BUFFER_BYTE_REF (export "RGC_BUFFER_BYTE_REF")
      (param $port (ref $input-port))
      (param $offset i32)
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (array.get_u $bstring 
	 (struct.get $rgc $buf (local.get $rgc))
	 (i32.add
	    (struct.get $rgc $matchstart (local.get $rgc))
	    (local.get $offset))))

   (func $RGC_BUFFER_AVAILABLE (export "RGC_BUFFER_AVAILABLE")
      (param $port (ref $input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (i32.sub (struct.get $rgc $bufpos (local.get $rgc))
	 (struct.get $rgc $matchstop (local.get $rgc))))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------

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
		(array.get_u $bstring
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
	      (array.get_u $bstring 
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
   
   ;; rgc_shift_buffer
   (func $rgc_shift_buffer
      (param $rgc (ref $rgc))
      
      (local $bufpos i32)
      (local $matchstart i32)
      (local $buffer (ref $bstring))
      (local.set $bufpos (struct.get $rgc $bufpos (local.get $rgc)))
      (local.set $matchstart (struct.get $rgc $matchstart (local.get $rgc)))
      (local.set $buffer (struct.get $rgc $buf (local.get $rgc)))
      
      (struct.set $rgc $lastchar (local.get $rgc)
	 (array.get_u $bstring (local.get $buffer)
	    (i32.sub (local.get $matchstart) (i32.const 1))))

      (array.copy $bstring $bstring
	 (local.get $buffer) (i32.const 0)
	 (local.get $buffer) (local.get $matchstart)
	 (i32.sub (local.get $bufpos) (local.get $matchstart)))

      (struct.set $rgc $bufpos (local.get $rgc)
	 (i32.sub (struct.get $rgc $bufpos (local.get $rgc))
	    (local.get $matchstart)))
      (struct.set $rgc $matchstop (local.get $rgc)
	 (i32.sub (struct.get $rgc $matchstop (local.get $rgc))
	    (local.get $matchstart)))
      (struct.set $rgc $forward (local.get $rgc)
	 (i32.sub (struct.get $rgc $forward (local.get $rgc))
	    (local.get $matchstart)))
      (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0)))

   ;; sysread
   (func $sysread
      (param $port (ref $input-port))
      (param $buf (ref $bstring))
      (param $o i32)
      (param $size i32)
      (result i32)
      
      (local $r i32)
      (local $sysread (ref null $sysread_t))
      (local.set $sysread (struct.get $input-port $sysread (local.get $port)))
      
      (local.set $r
	 (call_ref $sysread_t (local.get $port)
	    (local.get $buf)
	    (local.get $o)
	    (local.get $size)
	    (ref.cast (ref $sysread_t)
	       (struct.get $input-port $sysread (local.get $port)))))
      
      (if (i32.lt_s (local.get $r) (i32.const 0))
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $READ (i32.const 0) (i32.const 5))
		      (array.new_data $bstring $IO_ERROR (i32.const 0) (i32.const 8))
		      (local.get $port)))
	     (unreachable))
	  (else
	   (return (local.get $r)))))
		       
   ;; rgc_fillsize_buffer
   (func $rgc_fillsize_buffer
      (param $port (ref $input-port))
      (param $bufpos i32)
      (param $size i32)
      (result i32)

      (local $fb i32)
      (local $rgc (ref $rgc))
      (local $r i32)
      (local $buf (ref $bstring))

      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (local.set $fb (struct.get $rgc $fillbarrier (local.get $rgc)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))

      (if (i32.eqz (local.get $fb))
	  (then
	     (struct.set $rgc $bufpos (local.get $rgc) (local.get $bufpos))
	     (return (i32.const 0))))

      (if (i32.gt_s (local.get $fb) (i32.const 0))
	  (then
	     (if (i32.gt_s (local.get $size) (local.get $fb))
		 (then
		    (local.set $size (local.get $fb))))))

      (local.set $r
	 (call $sysread (local.get $port) (local.get $buf) (local.get $bufpos) (local.get $size)))

      (if (i32.gt_s (local.get $fb) (i32.const 0))
	  (then
	     (struct.set $rgc $fillbarrier (local.get $rgc)
		(i32.sub (local.get $fb) (local.get $r)))))

      (local.set $bufpos (i32.add (local.get $bufpos) (local.get $r)))
      (struct.set $rgc $bufpos (local.get $rgc) (local.get $bufpos))

      (return (i32.gt_s (local.get $r) (i32.const 0))))

   ;; rgc_fill_buffer
   (func $rgc_fill_buffer (export "rgc_fill_buffer")
      (param $port (ref $input-port))
      (result i32)
      
      (local $bufsize i32)
      (local $bufpos i32)
      (local $rgc (ref $rgc))

      (if (call $BGL_PORT_CLOSED_P (local.get $port))
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $READ (i32.const 0) (i32.const 4))
		      (array.new_data $bstring $PORT_CLOSED (i32.const 0) (i32.const 11))
		      (local.get $port)))
	     (unreachable)))
      
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      (local.set $bufpos (struct.get $rgc $bufpos (local.get $rgc)))
      
      ;; the read reached end-of-buffer, update the forward ptr
      (struct.set $rgc $forward (local.get $rgc) (local.get $bufpos))
	 
      ;; the input port that has seen its eof cannot be filled anymore 
      (if (struct.get $rgc $eof (local.get $rgc))
	  (then
	     (return (i32.const 0)))
	  (else
	   (local.set $bufsize (array.len (struct.get $rgc $buf (local.get $rgc))))
	   (if (i32.lt_u (local.get $bufpos) (local.get $bufsize))
	       (then
		  ;; the buffer is not full, fill it
		  (return_call $rgc_fillsize_buffer
		     (local.get $port)
		     (local.get $bufpos)
		     (i32.sub (local.get $bufsize) (local.get $bufpos))))
	       (else
		(if (i32.gt_u (struct.get $rgc $matchstart (local.get $rgc)) (i32.const 0))
		    ;; we are in the middle of a match, shift the buffer first
		    (then
		       (call $rgc_shift_buffer (local.get $rgc))
		       (local.set $bufpos (struct.get $rgc $bufpos (local.get $rgc)))
		       (return_call $rgc_fillsize_buffer
			  (local.get $port)
			  (local.get $bufpos)
			  (i32.sub (local.get $bufsize) (local.get $bufpos))))
		    (else
		     ;; the current token is too large for the buffer
		     ;; we have to enlarge it.
		     ;; Note: see rgc_size_fil_buffer for other
		     ;; enlarge_buffer         
		     (call $rgc_double_buffer (local.get $rgc))
		     (local.set $bufsize (array.len (struct.get $rgc $buf (local.get $rgc))))
		     (return_call $rgc_fillsize_buffer
			(local.get $port)
			(local.get $bufpos)
			(i32.sub (local.get $bufsize) (local.get $bufpos))))))))))
   
   ;; rgc_file_charready
   (func $rgc_file_charready
      (param $port (ref $file-input-port))
      (result i32)
      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $port)))
      
      (if (struct.get $rgc $eof (local.get $rgc))
	  (then (return (i32.const 0 #;FALSE))))

      (if (i32.eqz (struct.get $rgc $bufpos (local.get $rgc)))
	  (then
	     (drop (call $rgc_fill_buffer (local.get $port)))))
      
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

      (if (call $BGL_PORT_CLOSED_P (local.get $port))
	  (then (return (i32.const 0))))
      
      (if (i32.lt_u (struct.get $rgc $matchstop (local.get $rgc))
	     (struct.get $rgc $bufpos (local.get $rgc)))
	  (then (return (i32.const 1))))
      
      (if (ref.test (ref $file-input-port) (local.get $port))
	  (then (return_call $rgc_file_charready
		   (ref.cast (ref $file-input-port) (local.get $port)))))
      
      (i32.lt_u (struct.get $rgc $forward (local.get $rgc))
	 (struct.get $rgc $bufpos (local.get $rgc))))

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

   ;; rgc_buffer_symbol, see rgc-generic.sch
   
   ;; rgc_buffer_flonum
   (func $rgc_buffer_flonum (export "rgc_buffer_flonum")
      (param $ip (ref $input-port))
      (result f64)
      
      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local $stop i32)
      (local $start i32)
      
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))
      (local.set $stop (struct.get $rgc $matchstop (local.get $rgc)))
      (local.set $start (struct.get $rgc $matchstart (local.get $rgc)))
      
      (call $store_substring (local.get $buf)
	 (i64.extend_i32_u (local.get $start))
	 (i64.extend_i32_u (local.get $stop))
	 (i32.const 128))
      
      (return
	 (return_call $js_strtod (i32.const 128)
	    (i32.sub (local.get $stop) (local.get $start)))))
      
   ;; rgc_buffer_fixnum
   (func $rgc_buffer_fixnum (export "rgc_buffer_fixnum")
      (param $ip (ref $input-port))
      (result i64)
      
      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local $stop i32)
      (local $start i32)
      (local $sign i64)
      (local $res i64)
      (local $current i32)
      
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))
      (local.set $stop (struct.get $rgc $matchstop (local.get $rgc)))
      (local.set $start (struct.get $rgc $matchstart (local.get $rgc)))
      (local.set $res (i64.const 0))
      (local.set $sign (i64.const 1))
      
      ;; the sign
      (if (i32.eq (array.get_u $bstring (local.get $buf) (local.get $start))
	     (i32.const 43)) ;; #\+
	  (then
	     (local.set $start (i32.add (local.get $start) (i32.const 1))))
	  (else
	   (if (i32.eq (array.get_u $bstring (local.get $buf) (local.get $start))
		  (i32.const 45)) ;; #\-
	       (then
		  (local.set $sign (i64.const -1))
		  (local.set $start (i32.add (local.get $start) (i32.const 1)))))))
      
      (loop $while
	 (if (i32.lt_s (local.get $start) (local.get $stop))
	     (then
		(local.set $current
		   (i32.sub (array.get_u $bstring (local.get $buf) (local.get $start))
		      (i32.const 48))) ;; #\0
		(local.set $res
		   (i64.add (i64.mul (local.get $res) (i64.const 10))
		      (i64.extend_i32_s (local.get $current))))
		(local.set $start
		   (i32.add (local.get $start) (i32.const 1)))
		(br $while))))
      
      (return (i64.mul (local.get $res) (local.get $sign))))

      
   ;; rgc_buffer_integer
   (func $rgc_buffer_integer (export "rgc_buffer_integer")
      (param $ip (ref $input-port))
      (result (ref eq))

      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local $stop i32)
      (local $start i32)
      (local $res i64)
      (local $sign i64)
      (local $maxvalfx i64)
      (local $maxvalelong i64)
      (local $maxvalllong i64)
      (local $current i32)

      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))
      (local.set $stop (struct.get $rgc $matchstop (local.get $rgc)))
      (local.set $start (struct.get $rgc $matchstart (local.get $rgc)))
      (local.set $res (i64.const 0))
      (local.set $sign (i64.const 1))

      ;; the sign
      (if (i32.eq (array.get_u $bstring (local.get $buf) (local.get $start))
	     (i32.const 43)) ;; #\+
	  (then
	     (local.set $start (i32.add (local.get $start) (i32.const 1))))
	  (else
	   (if (i32.eq (array.get_u $bstring (local.get $buf) (local.get $start))
		  (i32.const 45)) ;; #\-
	       (then
		  (local.set $sign (i64.const -1))
		  (local.set $start (i32.add (local.get $start) (i32.const 1)))))))

      ;; skip the 0 padding
      (loop $while
	 (if (i32.lt_s (local.get $start) (local.get $stop))
	     (then 
		(if (i32.eq
		       (array.get_u $bstring (local.get $buf) (local.get $start))
		       (i32.const 48)) ;; #\0
		    (then
		       (local.set $start
			  (i32.add (local.get $start) (i32.const 1)))
		       (br $while))))))

      ;; the true number decoding
      (loop $while
	 (if (i32.lt_s (local.get $start) (local.get $stop))
	     (then
		(local.set $current
		   (i32.sub (array.get_u $bstring (local.get $buf) (local.get $start))
		      (i32.const 48))) ;; #\0
		(if (i64.gt_s (local.get $res)
		       (i64.sub (i64.div_s (global.get $MAXVALELONG)
				   (i64.const 10))
			  (i64.const 9)))
		    (then
		       (return_call $buffer_llong
			  (local.get $ip)
			  (local.get $buf)
			  (local.get $start)
			  (local.get $stop)
			  (local.get $res)
			  (local.get $sign)))
		    (else
		     (local.set $res
			(i64.add (i64.mul (local.get $res) (i64.const 10))
			   (i64.extend_i32_s (local.get $current))))
		     (local.set $start
			(i32.add (local.get $start) (i32.const 1)))
		     (br $while))))))

      (if (i64.le_s (local.get $res) (global.get $MAXVALFX))
	  (then
	     (return_call $BINT
		(i64.mul (local.get $res) (local.get $sign))))
	  (else
	   (return_call $make_belong
	      (i64.mul (local.get $res) (local.get $sign))))))

   ;; buffer_llong
   (func $buffer_llong
      (param $ip (ref $input-port))
      (param $buf (ref $bstring))
      (param $start i32)
      (param $stop i32)
      (param $lres i64)
      (param $sign i64)
      (result (ref eq))

      (local $current i32)

      (loop $while
	 (if (i32.lt_s (local.get $start) (local.get $stop))
	     (then
		(local.set $current
		   (i32.sub
		      (array.get_u $bstring (local.get $buf) (local.get $start))
		      (i32.const 48))) ;; #\0
		(if (i64.gt_s (local.get $lres)
		       (i64.sub
			  (i64.div_s (global.get $MAXVALLLONG) (i64.const 10))
			  (i64.const 9)))
		    (then
		       (return_call $rgc_buffer_bignum (local.get $ip)))
		    (else
		     (local.set $lres
			(i64.add (i64.mul (local.get $lres) (i64.const 10))
			   (i64.extend_i32_s (local.get $current))))
		     (local.set $start (i32.add (local.get $start) (i32.const 1)))
		     (br $while))))))

      (return_call $make_bllong
	 (i64.mul (local.get $lres) (local.get $sign))))

   ;; rgc_buffer_bignum
   (func $rgc_buffer_bignum (export "rgc_buffer_bignum")
      (param $ip (ref $input-port))
      (result (ref $bignum))

      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local $stop i32)
      (local $start i32)
      (local $sz i32)
      (local $tmp (ref $bstring))

      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))
      (local.set $stop (struct.get $rgc $matchstop (local.get $rgc)))
      (local.set $start (struct.get $rgc $matchstart (local.get $rgc)))
      (local.set $sz (i32.sub (local.get $stop) (local.get $start)))
      (local.set $tmp (array.new_default $bstring (local.get $sz)))

      (array.copy $bstring $bstring
	 (local.get $tmp)
	 (i32.const 0)
	 (local.get $buf)
	 (local.get $start)
	 (local.get $sz))

      (return_call $bgl_string_to_bignum (local.get $tmp) (i32.const 10)))

   ;; rgc_blit_string
   (func $bgl_rgc_blit_string (export "bgl_rgc_blit_string")
      (param $p (ref $input-port))
      (param $s (ref $bstring))
      (param $o i64)
      (param $l i64)
      (result i64)
      
      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local $avail i32)
      (local $o0 i64)
      (local $size i64)
      (local $r i64)

      (if (call $BGL_PORT_CLOSED_P (local.get $p))
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $READ (i32.const 0) (i32.const 4))
		      (array.new_data $bstring $PORT_CLOSED (i32.const 0) (i32.const 11))
		      (local.get $p)))
	     (unreachable)))

      (local.set $rgc (struct.get $input-port $rgc (local.get $p)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))
      (local.set $avail (call $RGC_BUFFER_AVAILABLE (local.get $p)))
      
      (call $RGC_START_MATCH (local.get $p))

      (if (struct.get $rgc $eof (local.get $rgc))
	  (then
	     (if (i32.gt_u (i32.wrap_i64 (local.get $l)) (local.get $avail))
		 (then (local.set $l (i64.extend_i32_u (local.get $avail)))))))

      (if (i32.ge_u (local.get $avail) (i32.wrap_i64 (local.get $l)))
	  (then
	     (array.copy $bstring $bstring
		(local.get $s)
		(i32.wrap_i64 (local.get $o))
		(local.get $buf)
		(struct.get $rgc $matchstart (local.get $rgc))
		(i32.wrap_i64 (local.get $l)))
	     (struct.set $rgc $matchstart (local.get $rgc)
		(i32.add (struct.get $rgc $matchstart (local.get $rgc))
		   (i32.wrap_i64 (local.get $l))))
	     (struct.set $rgc $forward (local.get $rgc)
		(struct.get $rgc $matchstart (local.get $rgc)))
	     (call $RGC_STOP_MATCH (local.get $p)
		(i64.extend_i32_u
		   (struct.get $rgc $matchstart (local.get $rgc))))
	     
	     (struct.set $rgc $filepos (local.get $rgc)
		(i32.add (struct.get $rgc $filepos (local.get $rgc))
		   (i32.wrap_i64 (local.get $l))))

	     (return (local.get $l)))
	  (else
	   (local.set $o0 (local.get $o))

	   (if (i32.gt_s (local.get $avail) (i32.const 0))
	       (then
		  (array.copy $bstring $bstring
		     (local.get $s)
		     (i32.wrap_i64 (local.get $o))
		     (local.get $buf)
		     (struct.get $rgc $matchstart (local.get $rgc))
		     (local.get $avail))
		  (local.set $o
		     (i64.add (local.get $o)
			(i64.extend_i32_u (local.get $avail))))
		  (local.set $l
		     (i64.sub (local.get $l)
			(i64.extend_i32_u (local.get $avail))))))

	   (if (i64.gt_s (local.get $l) (i64.const 0))
	       (then
		  (loop $while
		     (if (i32.eqz (struct.get $rgc $eof (local.get $rgc)))
			 (then
			    (if (i64.lt_s (local.get $l)
				   (global.get $default_io_bufsize))
				(then
				   (local.set $size (local.get $l)))
				(else
				 (local.set $size
				    (global.get $default_io_bufsize))))
			    (local.set $r
			       (i64.extend_i32_u
				  (call $sysread
				     (local.get $p)
				     (local.get $s)
				     (i32.const 0)
				     (i32.wrap_i64 (local.get $size)))))

			    (if (i64.eqz (local.get $r))
				(then
				   (struct.set $rgc $eof
				      (local.get $rgc) (i32.const 1)))
				(else
				 (local.set $o
				    (i64.add (local.get $o) (local.get $r)))
				 (local.set $l
				    (i64.sub (local.get $l) (local.get $r)))
				 (if (i64.gt_s (local.get $l) (i64.const 0))
				     (then
					(br $while))))))))))
	   
	   (struct.set $rgc $forward (local.get $rgc) (i32.const 0))
	   (struct.set $rgc $bufpos (local.get $rgc) (i32.const 0))
	   (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
	   (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
	   (struct.set $rgc $lastchar (local.get $rgc) (i32.const 13))
	   (struct.set $rgc $filepos (local.get $rgc)
	      (i32.add (struct.get $rgc $filepos (local.get $rgc))
		 (i32.wrap_i64 (i64.sub (local.get $o) (local.get $o0)))))
	   
	   (return (i64.sub (local.get $o) (local.get $o0))))))

   ;; rgc_reserve_space
   (func $rgc_reserve_space
      (param $rgc (ref $rgc))
      (param $amount i32)
      
      (local $matchstop i32)
      (local $diff i32)
      (local $bufpos i32)
      
      (local.set $matchstop (struct.get $rgc $matchstop (local.get $rgc)))
      
      (if (i32.ge_u (local.get $matchstop) (local.get $amount))
	  (then (return)))
      
      (local.set $bufpos (struct.get $rgc $bufpos (local.get $rgc)))
      (if (i32.ge_u (i32.add (local.get $matchstop)
		       (i32.sub (call $RGC_BUFFER_SIZE (local.get $rgc))
			  (local.get $bufpos)))
	     (local.get $amount))
	  (then
	     (local.set $diff
		(i32.sub (local.get $amount) (local.get $matchstop)))
	     (array.copy $bstring $bstring
		(struct.get $rgc $buf (local.get $rgc))
		(local.get $amount)
		(struct.get $rgc $buf (local.get $rgc))
		(local.get $matchstop)
		(i32.sub (local.get $bufpos) (local.get $matchstop)))
	     (struct.set $rgc $bufpos (local.get $rgc)
		(i32.add (local.get $bufpos) (local.get $diff)))
	     (struct.set $rgc $matchstop (local.get $rgc)
		(i32.add (struct.get $rgc $matchstop (local.get $rgc))
		   (local.get $diff))))
	  (else
	   (call $rgc_double_buffer (local.get $rgc))
	   (return_call $rgc_reserve_space (local.get $rgc)
	      (local.get $amount)))))

   ;; rgc_buffer_insert_substring
   (func $rgc_buffer_insert_substring
      (export "rgc_buffer_insert_substring")
      (param $ip (ref $input-port))
      (param $str (ref $bstring))
      (param $from i64)
      (param $to i64)
      (result i32)

      (local $rgc (ref $rgc))
      (local $matchstop i32)
      (local $len i32)
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $len (i32.wrap_i64 (i64.sub (local.get $to) (local.get $from))))
      
      (if (call $BGL_PORT_CLOSED_P (local.get $ip))
	  (then (return (i32.const 0))))

      (if (i64.ge_u (local.get $from) (local.get $to))
	  (then (return (i32.const 1))))
	  
      (call $rgc_reserve_space (local.get $rgc) (local.get $len))

      (local.set $matchstop (struct.get $rgc $matchstop (local.get $rgc)))

      (array.copy $bstring $bstring
	 (struct.get $rgc $buf (local.get $rgc))
	 (i32.sub (local.get $matchstop) (local.get $len))
	 (local.get $str)
	 (i32.wrap_i64 (local.get $from))
	 (local.get $len))
	 
      (if (i32.ge_u (struct.get $rgc $filepos (local.get $rgc))
	     (local.get $len))
	  (then
	     (struct.set $rgc $filepos (local.get $rgc)
		(i32.sub (struct.get $rgc $filepos (local.get $rgc))
		   (local.get $len))))
	  (else
	   (struct.set $rgc $filepos (local.get $rgc) (i32.const 0))))
      
      (local.set $matchstop (i32.sub (local.get $matchstop) (local.get $len)))
      (struct.set $rgc $matchstop (local.get $rgc) (local.get $matchstop))
      (struct.set $rgc $forward (local.get $rgc) (local.get $matchstop))
      (struct.set $rgc $matchstart (local.get $rgc) (local.get $matchstop))
      
      (return (i32.const 1)))
   
   ;; rgc_buffer_insert_char
   (func $rgc_buffer_insert_char
      (export "rgc_buffer_insert_char")
      (param $ip (ref $input-port))
      (param $c i32)
      (result i32)
      
      (local $rgc (ref $rgc))
      (local $matchstop i32)
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      
      (if (call $BGL_PORT_CLOSED_P (local.get $ip))
	  (then (return (i32.const 0))))
      
      (call $rgc_reserve_space (local.get $rgc) (i32.const 1))
      
      (local.set $matchstop (struct.get $rgc $matchstop (local.get $rgc)))
      
      (call $RGC_BUFFER_SET (local.get $rgc)
	 (i32.sub (local.get $matchstop) (i32.const 1))
	 (local.get $c))
      
      (if (i32.ge_u (struct.get $rgc $filepos (local.get $rgc))
	     (i32.const 1))
	  (then
	     (struct.set $rgc $filepos (local.get $rgc)
		(i32.sub (struct.get $rgc $filepos (local.get $rgc))
		   (i32.const 1))))
	  (else
	   (struct.set $rgc $filepos (local.get $rgc) (i32.const 0))))
      
      (local.set $matchstop (i32.sub (local.get $matchstop) (i32.const 1)))
      (struct.set $rgc $matchstop (local.get $rgc) (local.get $matchstop))
      (struct.set $rgc $forward (local.get $rgc) (local.get $matchstop))
      (struct.set $rgc $matchstart (local.get $rgc) (local.get $matchstop))
      
      (return (i32.const 1)))
   
   )

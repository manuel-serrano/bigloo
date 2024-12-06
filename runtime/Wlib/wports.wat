;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wports.wat         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 10:34:00 2024                          */
;*    Last change :  Fri Dec  6 09:08:54 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Input/Output Ports WASM implementation.                          */
;*=====================================================================*/

(module $__bigloo_ports

   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $string-output-port-name "string")
   (data $string-input-port-name "string")
   (data $stdout-name "stdout")
   (data $stderr-name "stderr")
   (data $stdin-name "stdin")
   
   (global $BGL_IONB i32 (i32.const 0))  ;; unubuffered
   (global $BGL_IOLBF i32 (i32.const 1)) ;; line buffered
   (global $BGL_IOFBF i32 (i32.const 2)) ;; fully buffered
   (global $BGL_IOEBF i32 (i32.const 3)) ;; extensible buffer

   (global $WHENCE_SEEK_CUR i32 (i32.const 0))
   (global $WHENCE_SEEK_END i32 (i32.const 1))

   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js" "close_fd" (func $js_close_fd (param i32)))
   (import "__js" "isatty" (func $js_isatty (param i32) (result i32)))

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------

   ;; sysclose_t
   (type $sysclose_t
      (func (param (ref eq))))

   ;; sysseek_t
   (type $sysseek_t
      (func (param (ref eq)) (param i32) (param i32)))

   ;; syswrite_t
   (type $syswrite_t
      (func (param (ref eq))
	 (param (ref $bstring))
	 (param i32)
	 (param i32)
	 (result i32)))

   ;; sysflush_t
   (type $sysflush_t
      (func (param (ref eq))
	 (result (ref eq))))
   
   ;; port
   (type $port
      (sub
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t))))))

   (global $port-default-value
      (export "BGL_PORT_DEFAULT_VALUE") (ref $port)
      (struct.new $port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)))

   ;; output-port
   (type $output-port
      (sub $port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $buf (mut (ref $bstring)))
	    (field $index (mut i32))
	    (field $bufmode (mut i32))
	    (field $syswrite (mut (ref null $syswrite_t)))
	    (field $fhook (mut (ref eq)))
	    (field $sysflush (mut (ref null $sysflush_t)))
	    (field $flushbuf (mut (ref eq)))
	    (field $err (mut i32)))))

   (global $output-port-default-value
      (export "BGL_OUTPUT_PORT_DEFAULT_VALUE") (ref $output-port)
      (struct.new $output-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; buf
	 (global.get $bstring-default-value)
	 ;; index
	 (i32.const 0)
	 ;; bufmode
	 (global.get $BGL_IONB)
	 ;; syswrite
	 (ref.null $syswrite_t)
	 ;; fhook
	 (global.get $BUNSPEC)
	 ;; sysflush
	 (ref.null $sysflush_t)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; error
	 (i32.const 0)))

   ;; $fd-output-port
   (type $fd-output-port
      (sub $output-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $buf (mut (ref $bstring)))
	    (field $index (mut i32))
	    (field $bufmode (mut i32))
	    (field $syswrite (mut (ref null $syswrite_t)))
	    (field $fhook (mut (ref eq)))
	    (field $sysflush (mut (ref null $sysflush_t)))
	    (field $flushbuf (mut (ref eq)))
	    (field $err (mut i32))
	    (field $fd i32))))

   ;; $file-output-port
   (type $file-output-port
      (sub final $fd-output-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $buf (mut (ref $bstring)))
	    (field $index (mut i32))
	    (field $bufmode (mut i32))
	    (field $syswrite (mut (ref null $syswrite_t)))
	    (field $fhook (mut (ref eq)))
	    (field $sysflush (mut (ref null $sysflush_t)))
	    (field $flushbuf (mut (ref eq)))
	    (field $err (mut i32))
	    (field $fd i32))))

   (global $file-output-port-default-value
      (export "BGL_FILE_OUTPUT_PORT_DEFAULT_VALUE") (ref $file-output-port)
      (struct.new $file-output-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; buf
	 (global.get $bstring-default-value)
	 ;; index
	 (i32.const 0)
	 ;; bufmode
	 (global.get $BGL_IONB)
	 ;; syswrite
	 (ref.null $syswrite_t)
	 ;; fhook
	 (global.get $BUNSPEC)
	 ;; sysflush
	 (ref.null $sysflush_t)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; err
	 (i32.const 0)
	 ;; fd
	 (i32.const -1)))

   ;; string-output-port
   (type $string-output-port
      (sub final $output-port
	 (struct
	       (field $name (mut (ref $bstring)))
	       (field $chook (mut (ref eq)))
	       (field $isclosed (mut i32))
	       (field $sysclose (mut (ref null $sysclose_t)))
	       (field $sysseek (mut (ref null $sysseek_t)))
	       (field $buf (mut (ref $bstring)))
	       (field $index (mut i32))
	       (field $bufmode (mut i32))
	       (field $syswrite (mut (ref null $syswrite_t)))
	       (field $fhook (mut (ref eq)))
	       (field $sysflush (mut (ref null $sysflush_t)))
	       (field $flushbuf (mut (ref eq)))
	       (field $err (mut i32)))))

   (global $string-output-port-default-value
      (export "BGL_STRING_OUTPUT_PORT_DEFAULT_VALUE") (ref $string-output-port)
      (struct.new $string-output-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; buf
	 (global.get $bstring-default-value)
	 ;; index
	 (i32.const 0)
	 ;; bufmode
	 (global.get $BGL_IONB)
	 ;; syswrite
	 (ref.null $syswrite_t)
	 ;; fhook
	 (global.get $BUNSPEC)
	 ;; sysflush
	 (ref.null $sysflush_t)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; err
	 (i32.const 0)))

   ;; input-port
   (type $input-port
      (sub $port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $rgc (ref $rgc)))))

   (global $input-port-default-value
      (export "BGL_INPUT_PORT_DEFAULT_VALUE") (ref $input-port)
      (struct.new $input-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; userseek
	 (global.get $BUNSPEC)
	 ;; rgc
	 (global.get $rgc-default-value)))

   ;; fd-input-port
   (type $fd-input-port
      (sub $input-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $rgc (ref $rgc))
	    (field $fd i32))))

   ;; file-input-port
   (type $file-input-port
      (sub $fd-input-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $rgc (ref $rgc))
	    (field $fd i32))))

   (global $file-input-port-default-value
      (export "BGL_FILE_INPUT_PORT_DEFAULT_VALUE") (ref $file-input-port)
      (struct.new $file-input-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; userseek
	 (global.get $BUNSPEC)
	 ;; rgc
	 (global.get $rgc-default-value)
	 ;; fd
	 (i32.const -1)))

   ;; console-input-port
   (type $console-input-port
      (sub final $file-input-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $rgc (ref $rgc))
	    (field $fd i32))))

   ;; string-input-port
   (type $string-input-port
      (sub final $input-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $rgc (ref $rgc))
	    (field $offset (mut i64)))))

   (global $string-input-port-default-value
      (export "BGL_STRING_INPUT_PORT_DEFAULT_VALUE") (ref $string-input-port)
      (struct.new $string-input-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 1)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; userseek
	 (global.get $BUNSPEC)
	 ;; rgc
	 (global.get $rgc-default-value)
	 ;; offset
	 (i64.const 0)))

   ;; -----------------------------------------------------------------
   ;; Common macros
   ;; -----------------------------------------------------------------
   (func $EOF_OBJECTP (export "EOF_OBJECTP")
      (param $v (ref eq))
      (result i32)
      (ref.eq (local.get $v) (global.get $BEOF)))

   (func $BGL_PORT_CLOSED_P
      (param $op (ref $port))
      (result i32)

      (return (struct.get $port $isclosed (local.get $op))))

   (func $BGL_OUTPUT_PORT_CNT
      (param $op (ref $output-port))
      (result i32)
      
      (return
	 (i32.sub (array.len (struct.get $output-port $buf (local.get $op)))
	    (struct.get $output-port $index (local.get $op)))))

   (func $BGL_OUTPUT_PORT_FLUSHBUF
      (param $op (ref $output-port))
      (result (ref eq))

      (return (struct.get $output-port $flushbuf (local.get $op))))

   (func $BGL_INPUT_PORT_BUFFER (export "BGL_INPUT_PORT_BUFFER")
      (param $ip (ref $input-port))
      (result (ref $bstring))

      (return
	 (struct.get $rgc $buf
	    (struct.get $input-port $rgc (local.get $ip)))))
      
   (func $BGL_INPUT_PORT_BUFSIZ (export "BGL_INPUT_PORT_BUFSIZ")
      (param $ip (ref $input-port))
      (result i64)

      (return_call $STRING_LENGTH
	 (call $BGL_INPUT_PORT_BUFFER (local.get $ip))))
   
   (func $BGL_INPUT_PORT_USEEK (export "BGL_INPUT_PORT_USEEK")
      (param $ip (ref $input-port))
      (result (ref eq))

      (return (struct.get $input-port $userseek (local.get $ip))))

   (func $BGL_INPUT_PORT_USEEK_SET (export "BGL_INPUT_PORT_USEEK_SET")
      (param $ip (ref $input-port))
      (param $seek (ref eq))

      (struct.set $input-port $userseek (local.get $ip) (local.get $seek)))

   (func $INPUT_PORT_FILEPOS (export "INPUT_PORT_FILEPOS")
      (param $ip (ref $input-port))
      (result i64)
      (return (i64.extend_i32_u
		 (struct.get $rgc $filepos
		    (struct.get $input-port $rgc (local.get $ip))))))
   
   (func $INPUT_PORT_TOKENPOS (export "INPUT_PORT_TOKENPOS")
      (param $ip (ref $input-port))
      (result i64)
      (return (i64.sub (call $INPUT_PORT_FILEPOS (local.get $ip))
		 (call $RGC_BUFFER_MATCH_LENGTH (local.get $ip)))))

   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------

   (global $_stdout (mut (ref $fd-output-port))
      (global.get $file-output-port-default-value))
   
   (global $_stderr (mut (ref $fd-output-port))
      (global.get $file-output-port-default-value))
   
   (global $_stdin (mut (ref $input-port))
      (global.get $file-input-port-default-value))
   
   (global $stdout_from (mut i32)
      (i32.const 0))

   ;; -----------------------------------------------------------------
   ;; Primitive IO operations 
   ;; -----------------------------------------------------------------

   ;; _CLOSE
   (func $_CLOSE
      (param $op (ref eq))
      (if (ref.test (ref $fd-output-port) (local.get $op))
	  (then (call $js_close_fd
		   (struct.get $fd-output-port $fd
		      (ref.cast (ref $fd-output-port) (local.get $op)))))
	  (else
	   (if (ref.test (ref $file-input-port) (local.get $op))
	       (then
		  (call $js_close_fd
		     (struct.get $file-input-port $fd
			(ref.cast (ref $file-input-port)
			   (local.get $op)))))))))
      
   ;; _LSEEK
   (func $_LSEEK
      (param $op (ref eq))
      (param $i i32)
      (param $j i32))
   
   ;; bgl_syswrite
   (func $bgl_syswrite
      (param $p (ref eq))
      (param $buf (ref $bstring))
      (param $start i32)
      (param $count i32)
      (result i32)

      (local $op (ref $output-port))
      (local.set $op (ref.cast (ref $output-port) (local.get $p)))

      (call $memcpy
	 (i32.const 128)
	 (local.get $buf)
	 (local.get $start)
	 (local.get $count))

      (call $js_write_file
	 (struct.get $fd-output-port $fd
	    (ref.cast (ref $fd-output-port) (local.get $op)))
	 (i32.const 128)
	 (local.get $count))

      (return (local.get $count)))

   ;; bgl_input_string_seek
   (func $bgl_input_string_seek
      (param $p (ref eq))
      (param $posi32 i32)
      (param $whence i32)

      (local $ip (ref $string-input-port))
      (local $rgc (ref $rgc))
      (local $offset i64)
      (local $pos i64)
      (local.set $ip (ref.cast (ref $string-input-port) (local.get $p)))
      (local.set $rgc (struct.get $string-input-port $rgc (local.get $ip)))
      (local.set $offset (struct.get $string-input-port $offset (local.get $ip)))
      (local.set $pos (i64.extend_i32_u (local.get $posi32)))
      
      (if (i64.eq (local.get $pos)
	     (call $BGL_INPUT_PORT_BUFSIZ (local.get $ip)))
	  (then
	     (struct.set $rgc $eof (local.get $rgc) (i32.const 1))
	     (return))
	  (else
	   (if (i64.ge_s (local.get $pos) (i64.const 0))
	       (then
		  (if (i64.lt_s (local.get $pos)
			 (call $BGL_INPUT_PORT_BUFSIZ (local.get $ip)))
		      (then
			 (struct.set $rgc $filepos (local.get $rgc)
			    (i32.wrap_i64
			       (i64.add (local.get $pos) (local.get $offset))))
			 (struct.set $rgc $matchstart (local.get $rgc)
			    (i32.wrap_i64
			       (i64.add (local.get $pos) (local.get $offset))))
			 (struct.set $rgc $matchstop (local.get $rgc)
			    (i32.wrap_i64
			       (i64.add (local.get $pos) (local.get $offset))))
			 (struct.set $rgc $forward (local.get $rgc)
			    (i32.wrap_i64
			       (i64.add (local.get $pos) (local.get $offset))))
			 (return)))))))
      
      (throw $fail))


   ;; reset_console
   (func $reset_console (export "reset_console")
      (param $ip (ref $input-port))
      (result (ref eq))

      (local $rgc (ref $rgc))
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))

      (if (ref.test (ref $console-input-port) (local.get $ip))
	  (then
	     (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
	     (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
	     (struct.set $rgc $bufpos (local.get $rgc) (i32.const 0))
	     (struct.set $rgc $lastchar (local.get $rgc) (i32.const 0x0A))))

      (return (global.get $BUNSPEC)))
   
   ;; reset_eof
   (func $reset_eof (export "reset_eof")
      (param $ip (ref $input-port))
      (result i32)

      (local $rgc (ref $rgc))

      (if (result i32) (ref.test (ref $console-input-port) (local.get $ip))
	  (then
	     (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
	     (struct.set $rgc $eof (local.get $rgc) (i32.const 0))
	     (drop (call $reset_console (local.get $ip)))
	     (i32.const 1))
	  (else
	   (i32.const 0))))
      

;*    ;; sysclose_string_output_port                                   */
;*    (func $syclose_string_output_port                                */
;*       (param $op (ref $port))                                       */
;*       (local $sp (ref $string-output-port))                         */
;*       (local $r (ref eq))                                           */
;*       (local.set $sp (ref.cast $string-output-port (local.get $op))) */
;*                                                                     */
;*       (local.set $r                                                 */
;* 	 (struct.get $string-output-port $buf (local.get $p)))         */
;*       (struct.set $string-output-port $buf (local.get $p)           */
;* 	 (array.new_fixed $bstring 0))                                 */
;*                                                                     */
;*       (return $r))                                                  */
;*                                                                     */
;*    ;; sysclose_fd_output_port                                       */
;*    (func $syclose_fd_output_port                                    */
;*       (param $op (ref $port))                                       */
;*       (local $fp (ref $fd-output-port))                             */
;*       (local.set $fp (ref.cast $fd-output-port (local.get $op)))    */
;*                                                                     */
;*       (call $js_close_fd                                            */
;* 	 (struct.get $fd-output-port $fd (local.get $fp)))             */
;*                                                                     */
;*       (return (local.get $op)))                                     */

   ;; bgl_close_input_port 
   (func $bgl_close_input_port (export "bgl_close_input_port")
      (param $ip (ref $input-port))
      (result (ref eq))

      (local $proc (ref $procedure))
      
      (if (call $BGL_PORT_CLOSED_P (local.get $ip))
	  ;; the port is already closed
	  (then
	     (return (local.get $ip))))
      
      ;; mark it closed
      (struct.set $input-port $isclosed (local.get $ip) (i32.const 1))
      
      ;; chook
      (if (ref.test (ref $procedure)
	     (struct.get $input-port $chook (local.get $ip)))
	  (then
	     (drop
		(call $funcall1
		   (ref.cast (ref $procedure)
		      (struct.get $input-port $chook (local.get $ip)))
		   (local.get $ip)))))
      
      ;; and close it for good
      (if (ref.test (ref $sysclose_t)
	     (struct.get $input-port $sysclose (local.get $ip)))
	  (then
	     (call_ref $sysclose_t
		(local.get $ip)
		(struct.get $input-port $sysclose
		   (local.get $ip)))
	     (return (local.get $ip)))
	  (else
	   (return (local.get $ip)))))
      
   ;; bgl_close_output_port
   (func $bgl_close_output_port (export "bgl_close_output_port")
      (param $op (ref $output-port))
      (result (ref eq))
      (local $res (ref eq))

      (local.set $res (local.get $op))

      (if (call $BGL_PORT_CLOSED_P (local.get $op))
	  ;; the port is already closed
	  (then
	     (return (local.get $op)))
	  (else
	   (if (ref.test (ref $fd-output-port) (local.get $op))
	       (then
		  (if (i32.le_s
			 (struct.get $fd-output-port $fd
			    (ref.cast (ref $fd-output-port) (local.get $op)))
			 (i32.const 2))
		      ;; _stdout or _stderr
		      (then
			 (call $output_flush (local.get $op)
			    (ref.null none) (i32.const 0)
			    (i32.const 0) (i32.const 0) (i32.const 0))
			 ;; never close stdout nor stderr
			 (return (local.get $op))))))

	   (if (ref.test (ref $string-output-port) (local.get $op))
	       (then
		  (local.set $res
		     (call $bgl_string_shrink
			(struct.get $string-output-port $buf
			   (ref.cast (ref $string-output-port) (local.get $op)))
			(i64.extend_i32_s
			   (struct.get $string-output-port $index
			      (ref.cast (ref $string-output-port) (local.get $op)))))))
	       (else
		(if (i32.eqz (struct.get $output-port $err (local.get $op)))
		    (then
		       (drop
			  (call $output_flush (local.get $op)
			     (ref.null none) (i32.const 0)
			     (i32.const 0) (i32.const 0) (i32.const 0)))))))
	   
	   ;; mark it closed
	   (struct.set $output-port $isclosed (local.get $op) (i32.const 1))
	   
	   ;; chook
	   (if (ref.test (ref $procedure)
		  (struct.get $output-port $chook (local.get $op)))
	       (then
		  (drop
		     (call $funcall1
			(ref.cast (ref $procedure)
			   (struct.get $output-port $chook (local.get $op)))
			(local.get $op)))))

	   ;; and close it for good
	   (if (ref.test (ref $sysclose_t)
		  (struct.get $output-port $sysclose (local.get $op)))
	       (then
		  (call_ref $sysclose_t
		     (local.get $op)
		     (struct.get $output-port $sysclose
			(local.get $op)))
		  (return (local.get $res)))
	       (else
		(return (local.get $res)))))))

   ;; -----------------------------------------------------------------
   ;; Output flush functions 
   ;; -----------------------------------------------------------------

   ;; strseek
   (func $strseek
      (param $p (ref eq))
      (param $offset i32)
      (param $whence i32)

      (local $op (ref $output-port))
      (local $buf (ref $bstring))
      (local $len i32)
      (local $cnt i32)
      (local.set $op (ref.cast (ref $output-port) (local.get $p)))
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $len (array.len (local.get $buf)))
      (local.set $cnt (call $BGL_OUTPUT_PORT_CNT (local.get $op)))

      (if (i32.eq (local.get $whence) (global.get $WHENCE_SEEK_CUR))
	  (then
	     (local.set $offset
		(i32.add (local.get $offset)
		   (struct.get $output-port $index (local.get $op)))))
	  (else
	   (if (i32.eq (local.get $whence) (global.get $WHENCE_SEEK_END))
	       (then
		  (local.set $offset
		     (i32.add (local.get $len) (local.get $offset)))))))

      (if (i32.lt_s (local.get $offset) (i32.const 0))
	  (then
	     (return (i32.const -1)))
	  (else
	   (if (i32.gt_s (local.get $offset) (local.get $cnt))
	       (then
		  (return (i32.const -1))))))

      (struct.set $output-port $index (local.get $op)
	 (local.get $offset))
      (return (local.get $offset)))
      
   ;; flush_string
   (func $flush_string
      (param $op (ref $output-port))
      (param $s (ref $bstring))
      (param $start i32)
      (param $l i32)
      (param $err i32)
      
      (local $syswrite (ref null $syswrite_t))
      (local $slen i32)
      (local $n i32)
      (local $buf (ref $bstring))
      
      (local.set $syswrite (struct.get $output-port $syswrite (local.get $op)))
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $slen (local.get $l))
      
      (loop $loop
	 (if (i32.gt_s (local.get $slen) (i32.const 0))
	     (then
		(local.set $n 
		   (call_ref $syswrite_t
		      (local.get $op)
		      (local.get $s)
		      (local.get $start)
		      (local.get $slen)
		      (local.get $syswrite)))
		(if (i32.lt_s (local.get $n) (i32.const 0))
		    (then
		       (if (local.get $err)
			   (then
			      (struct.set $output-port $err (local.get $op)
				 (global.get $BGL_IO_WRITE_ERROR))
			      (throw $fail))
			   (else
			    (br $loop))))
		    (else
		     (local.set $slen
			(i32.sub (local.get $slen) (local.get $n)))
		     (local.set $start
			(i32.add (local.get $start) (local.get $n)))
		     (br $loop)))))))

   ;; invoke_flush_hook
   (func $invoke_flush_hook
      (param $fhook (ref eq))
      (param $op (ref $output-port))
      (param $slen i64)
      (param $err i32)
      
      (local $s (ref eq))
      (local $buf (ref eq))
      (local $cint i64)
      
      (if (ref.test (ref $procedure) (local.get $fhook))
	  (then
	     (local.set $s
		(call $funcall2
		   (ref.cast (ref $procedure)
		      (struct.get $output-port $fhook (local.get $op)))
		   (local.get $op)
		   (call $make_bint (local.get $slen))))
      
	     (if (call $STRINGP (local.get $s))
		 (then
		    (call $flush_string (local.get $op)
		       (ref.cast (ref $bstring) (local.get $s))
		       (i32.const 0)
		       (array.len (ref.cast (ref $bstring) (local.get $s)))
		       (local.get $err)))
		 (else
		  (local.set $buf
		     (call $BGL_OUTPUT_PORT_FLUSHBUF (local.get $op)))
		  
		  (if (i32.eqz (call $INTEGERP (local.get $s)))
		      (then (return)))
		  
		  (if (i32.eqz (call $STRINGP (local.get $buf)))
		      (then (return)))
		  
		  (local.set $cint (call $OBJ_TO_INT (local.get $s)))
		  
		  (if (i64.le_s (local.get $cint)
			 (call $STRING_LENGTH
			    (ref.cast (ref $bstring) (local.get $buf))))
		      (then
			 (if (i64.gt_s (local.get $cint) (i64.const 0))
			     (then
				(call $flush_string (local.get $op)
				   (ref.cast (ref $bstring) (local.get $buf))
				   (i32.const 0)
				   (i32.wrap_i64 (local.get $cint))
				   (local.get $err)))))))))))

   ;; output_flush
   (func $output_flush
      (param $op (ref $output-port))
      (param $str (ref null $bstring))
      (param $start i32)
      (param $slen i32)
      (param $is_read_flush i32)
      (param $err i32)
      (result (ref eq))
      
      (local $buf (ref $bstring))
      (local $len i32)
      (local $cnt i32)
      (local $use i32)
      (local $n i32)
      
      (if (call $BGL_PORT_CLOSED_P (local.get $op))
	  ;; closed output-port
	  (then (throw $fail)))
      
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $len (array.len (local.get $buf)))
      (local.set $cnt (call $BGL_OUTPUT_PORT_CNT (local.get $op)))
      (local.set $use (struct.get $output-port $index (local.get $op)))
	 
      ;; flush out the buffer, if needed
      (if (i32.sub (struct.get $output-port $bufmode (local.get $op))
	     (global.get $BGL_IOEBF))
	  (then
	     (if (ref.eq (local.get $op) (global.get $_stdout))
		 ;; take into account stdout_from
		 (then
		    (local.set $use
		       (i32.sub (local.get $use)
			  (global.get $stdout_from)))))
	     
	     ;; invoke the flush hook
	     (call $invoke_flush_hook
		(struct.get $output-port $fhook (local.get $op))
		(local.get $op)
		(i64.add (i64.extend_i32_u (local.get $use))
		   (i64.extend_i32_u (local.get $slen)))
		(local.get $err))
	     
	     ;; write the buffer
	     (call $flush_string (local.get $op)
		(local.get $buf) (i32.const 0)
		(local.get $use) (local.get $err))
	     
	     ;; write the string that overflowed
	     (if (i32.eqz (ref.eq (local.get $str) (ref.null none)))
		 (then
		    (call $flush_string (local.get $op)
		       (ref.cast (ref $bstring) (local.get $str))
		       (local.get $start)
		       (local.get $slen) (local.get $err))))
	     
	     ;; update the port
	     (if (ref.eq (local.get $op) (global.get $_stdout))
		 (then
		    (if (local.get $is_read_flush)
			(then
			   ;; slen must be zero
			   (global.set $stdout_from
			      (i32.add (global.get $stdout_from)
				 (local.get $use))))
			(else
			 (global.set $stdout_from (i32.const 0))
			 (struct.set $output-port $index
			    (local.get $op) (i32.const 0)))))
		 (else
		  (struct.set $output-port $index
		     (local.get $op) (i32.const 0)))))
	  (else
	   ;; invoke the flush hook
	   (call $invoke_flush_hook
	      (struct.get $output-port $fhook (local.get $op))
	      (local.get $op)
	      (i64.add (i64.extend_i32_u (local.get $use))
		 (i64.extend_i32_u (local.get $slen)))
	      (local.get $err))
	   
	   ;; this is an extensible buffer, that we increase iff it is full
	   (if (i32.le_s (local.get $slen) (i32.const 0))
	       (then
		  (return (local.get $op))))
	   
	   (if (i32.eqz (i32.eq (local.get $cnt) (i32.const 0)))
	       (then
		  (return (local.get $op))))
	   
	   (local.set $n
	      (call_ref $syswrite_t
		 (local.get $op)
		 (ref.cast (ref $bstring) (local.get $str))
		 (local.get $start)
		 (local.get $slen)
		 (ref.cast (ref $syswrite_t)
		    (struct.get $output-port $syswrite (local.get $op)))))
	   
	   (if (i32.lt_s (local.get $n) (i32.const 0))
	       (then
		  (if (local.get $err)
		      (then
			 (throw $fail)))))))
	   
      (return (local.get $op)))

   ;; bgl_output_flush
   (func $bgl_output_flush
      (param $op (ref $output-port))
      (param $str (ref null $bstring))
      (param $start i32)
      (param $size i32)
      (result (ref eq))
      (return_call $output_flush (local.get $op)
	 (local.get $str) (local.get $start) (local.get $size)
	 (i32.const 0) (i32.const 1)))

   ;; $charbuf
   (global $charbuf (ref $bstring)
      (array.new_fixed $bstring 1 (i32.const 0)))
   
   ;; bgl_output_flush_char
   (func $bgl_output_flush_char
      (param $op (ref $output-port))
      (param $c i32)
      (result (ref eq))
      (array.set $bstring (global.get $charbuf) (i32.const 0) (local.get $c))
      (return_call $output_flush (local.get $op)
	 (global.get $charbuf) (i32.const 0)
	 (i32.const 1) (i32.const 0) (i32.const 1)))
   
   ;; bgl_flush_ouput_port
   (func $bgl_flush_output_port (export "bgl_flush_output_port")
      (param $op (ref $output-port))
      (result (ref eq))
      
      (drop
	 (call $bgl_output_flush (local.get $op)
	    (ref.null none) (i32.const 0) (i32.const 0)))
      
      (if (ref.test (ref $sysflush_t)
	     (struct.get $output-port $sysflush (local.get $op)))
	  (then
	     (return_call_ref $sysflush_t
		(local.get $op)
		(struct.get $output-port $sysflush (local.get $op))))
	  (else
	   (return (global.get $BTRUE)))))

   ;; bgl_write
   (func $bgl_write 
      (param $op (ref $output-port))
      (param $str (ref $bstring))
      (param $start i32)
      (param $sz i32)
      (result (ref eq))
      
      (local $buf (ref $bstring))
      (local $index i32)
      (local $c i32)
      
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $index (struct.get $output-port $index (local.get $op)))
      
      (if (i32.gt_s (call $BGL_OUTPUT_PORT_CNT (local.get $op))
	     (local.get $sz))
	  (then
	     (if (i32.eq (struct.get $output-port $bufmode (local.get $op))
		    (global.get $BGL_IOLBF))
		 (then
		    (loop $loop
		       (local.set $sz (i32.sub (local.get $sz) (i32.const 1)))
		       (if (i32.gt_s (local.get $sz) (i32.const 0))
			   (then
			      (local.set $c
				 (array.get $bstring (local.get $str)
				    (local.get $start)))
			      (local.set $start
				 (i32.add (local.get $start) (i32.const 1)))
			      
			      (array.set $bstring (local.get $buf)
				 (local.get $index)
				 (local.get $c))
			      (local.set $index
				 (i32.add (local.get $index) (i32.const 1)))
			      
			      (if (i32.eq (local.get $c) (i32.const 13))
				  (then
				     (drop
					(call $bgl_output_flush
					   (local.get $op)
					   (ref.null none)
					   (i32.const 0)
					   (i32.const 0)))))
			      (br $loop))))
		    (struct.set $output-port $index (local.get $op)
		       (local.get $index))
		    (return (local.get $op)))
		 (else
		  (array.copy $bstring $bstring
		     (local.get $buf)
		     (local.get $index)
		     (local.get $str)
		     (local.get $start)
		     (local.get $sz))
		  (struct.set $output-port $index (local.get $op)
		     (i32.add (local.get $index) (local.get $sz)))
		  (return (local.get $op)))))
	  (else
	   (return_call $bgl_output_flush (local.get $op)
	      (local.get $str) (local.get $start) (local.get $sz)))))

   ;; strwrite
   (func $strwrite
      (param $p (ref eq))
      (param $str (ref $bstring))
      (param $start i32)
      (param $count i32)
      (result i32)
      
      (local $op (ref $output-port))
      (local $buf (ref $bstring))
      (local $used i32)
      (local $nlen i32)
      (local $nbuf (ref $bstring))

      (local.set $op (ref.cast (ref $output-port) (local.get $p)))
      (local.set $buf (struct.get $output-port $buf (local.get $op)))
      (local.set $used (struct.get $output-port $index (local.get $op)))
      (local.set $nlen (i32.mul (i32.const 2)
			  (i32.add (array.len (local.get $buf))
			     (local.get $count))))
      (local.set $nbuf (array.new_default $bstring (local.get $nlen)))
      
      (array.copy $bstring $bstring
	 (local.get $nbuf)
	 (i32.const 0)
	 (local.get $buf)
	 (i32.const 0)
	 (local.get $used))
      (array.copy $bstring $bstring
	 (local.get $nbuf)
	 (local.get $used)
	 (local.get $str)
	 (local.get $start)
	 (local.get $count))
      
      (struct.set $output-port $buf (local.get $op) (local.get $buf))
      (return (local.get $count)))

   ;; -----------------------------------------------------------------
   ;; Opens functions 
   ;; -----------------------------------------------------------------

   ;; bgl_open_input_file
   (func $bgl_open_input_file (export "bgl_open_input_file")
      (param $path (ref $bstring))
      (param $buffer (ref $bstring))
      (result (ref eq))
      
      (local $fd i32)
      (local $rgc (ref $rgc))
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      (local.set $fd
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $path))
	    ;; READ-ONLY flag
	    (i32.const 0)))
      
      (local.set $rgc
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
	    (i32.const 0x0A #;(ASCII NEWLINE '\n'))
	    ;; buf
	    (if (result (ref $bstring))
		(ref.is_null (local.get $buffer))
		(then (array.new_default $bstring (i32.const 4096)))
		(else
		 (if (result (ref $bstring))
		     (i32.eqz (array.len (local.get $buffer)))
		     (then (array.new_default $bstring (i32.const 4096)))
		     (else (ref.cast (ref $bstring) (local.get $buffer))))))))
      
      (struct.new $file-input-port
	 ;; name
	 (local.get $path)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 0)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; useseek
	 (global.get $BUNSPEC)
	 ;; rgc
	 (local.get $rgc)
	 ;; File descriptor
	 (local.get $fd)))

   ;; bgl_open_input_substring_bang
   (func $bgl_open_input_substring_bang (export "bgl_open_input_substring_bang")
      (param $buffer (ref $bstring))
      (param $offset i64)
      (param $end i64)
      (result (ref $string-input-port))

      (local $rgc (ref $rgc))
      (local.set $rgc
	 (struct.new $rgc
	    ;; eof
	    (i32.const 1)
	    ;; filepos
	    (i32.const 0)
	    ;; forward
	    (i32.const 0)
	    ;; bufpos
	    (i32.wrap_i64 (local.get $end))
	    ;; matchstart
	    (i32.wrap_i64 (local.get $offset))
	    ;; matchstop
	    (i32.wrap_i64 (local.get $offset))
	    ;; lastchar
	    (i32.const 13)
	    ;; buffer
	    (local.get $buffer)))
      
      (return
	 (struct.new $string-input-port
	    ;; name
	    (array.new_data $bstring $string-input-port-name
	       (i32.const 0) (i32.const 6))
	    ;; chook
	    (global.get $BUNSPEC)
	    ;; isclosed
	    (i32.const 0)
	    ;; sysclose
	    (ref.null $sysclose_t)
	    ;; sysseek
	    (ref.func $bgl_input_string_seek)
	    ;; userseek
	    (global.get $BUNSPEC)
	    ;; rgc
	    (local.get $rgc)
	    ;; offset
	    (local.get $offset))))
   
   ;; bgl_open_input_substring
   (func $bgl_open_input_substring (export "bgl_open_input_substring")
      (param $str (ref $bstring))
      (param $offset i64)
      (param $end i64)
      (result (ref $string-input-port))

      (local $len i64)
      (local $buf (ref $bstring))
      (local.set $len (i64.sub (local.get $end) (local.get $offset)))
      (local.set $buf (array.new_default $bstring (i32.wrap_i64 (local.get $len))))

      (array.copy $bstring $bstring (local.get $buf)
	 (i32.const 0)
	 (local.get $str)
	 (i32.wrap_i64 (local.get $offset))
	 (i32.wrap_i64 (local.get $len)))
      
      (return_call $bgl_open_input_substring_bang
	 (local.get $buf) (local.get $offset)
	 (i64.extend_i32_u (array.len (local.get $buf)))))
     
   ;; bgl_open_input_string
   (func $bgl_open_input_string (export "bgl_open_input_string")
      (param $str (ref $bstring))
      (param $offset i64)
      (result (ref $string-input-port))
      (return_call $bgl_open_input_substring
	 (local.get $str) (local.get $offset)
	 (i64.extend_i32_u (array.len (local.get $str)))))
     
   ;; bgl_open_output_file
   (func $bgl_open_output_file (export "bgl_open_output_file")
      (param $name (ref $bstring))
      (param $buf (ref $bstring))
      (result (ref eq))
      
      (local $fd i32)
      
      ;; TODO: support buffered output (for now, $buf is ignored)
      (call $store_string
	 (local.get $name)
	 (i32.const 128))
      (local.set $fd
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $name))
	    ;; WRITE-ONLY flag
	    (i32.const 1)))
      
      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then
	     (return (global.get $BFALSE)))
	  (else
	   (return
	      (struct.new $file-output-port
		 ;; name
		 (local.get $name)
		 ;; chook
		 (global.get $BUNSPEC)
		 ;; isclosed
		 (i32.const 0)
		 ;; sysclose
		 (ref.func $_CLOSE)
		 ;; sysseek
		 (ref.func $_LSEEK)
		 ;; buf
		 (local.get $buf)
		 ;; index
		 (i32.const 0)
		 ;; bufmode
		 (global.get $BGL_IONB)
		 ;; syswrite
		 (ref.func $bgl_syswrite)
		 ;; fhook
		 (global.get $BUNSPEC)
		 ;; sysflush
		 (ref.null $sysflush_t)
		 ;; flushbuf
		 (global.get $BUNSPEC)
		 ;; err
		 (i32.const 0)
		 ;; fd
		 (local.get $fd))))))
   
   ;; bgl_open_output_string
   (func $bgl_open_output_string (export "bgl_open_output_string")
      (param $buf (ref $bstring))
      (result (ref $output-port))
      (struct.new $string-output-port
	 ;; name
	 (array.new_data $bstring $string-output-port-name
	    (i32.const 0) (i32.const 6))
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 0)
	 ;; sysclose
	 (ref.null $sysclose_t)
	 ;; sysseek
	 (ref.func $strseek)
	 ;; buf
	 (local.get $buf)
	 ;; index
	 (i32.const 0)
	 ;; bufmode
	 (global.get $BGL_IOEBF)
	 ;; syswrite
	 (ref.func $strwrite)
	 ;; fhook
	 (global.get $BUNSPEC)
	 ;; sysflush
	 (ref.null $sysflush_t)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; err
	 (i32.const 0)))

   ;; -----------------------------------------------------------------
   ;; Misc functions 
   ;; -----------------------------------------------------------------

   ;; get_output_string
   (func $get_output_string (export "get_output_string")
      (param $op (ref $output-port))
      (result (ref $bstring))

      (if (ref.test (ref $string-output-port) (local.get $op))
	  (then
	     (return_call $string_to_bstring_len
		(struct.get $output-port $buf (local.get $op))
		(struct.get $output-port $index (local.get $op))))
	  (else
	   (throw $fail))))
      

   ;; bgl_output_port_buffer_set
   (func $bgl_output_port_buffer_set (export "bgl_output_port_buffer_set")
      (param $op (ref $output-port))
      (param $buf (ref eq))

      (if (i32.eqz (ref.test (ref $bstring) (local.get $buf)))
	  (then (throw $fail))
	  (else
	   (struct.set $output-port $buf
	      (local.get $op) (ref.cast (ref $bstring) (local.get $buf)))
	   (struct.set $output-port $index (local.get $op)
	      (i32.const 0)))))

   ;; bgl_reset_output_string_port
   (func $bgl_reset_output_string_port (export "bgl_reset_output_string_port")
      (param $op (ref $output-port))
      (result (ref eq))

      (local $res (ref $bstring))
      (local.set $res (call $get_output_string (local.get $op)))

      (call $bgl_output_port_buffer_set (local.get $op)
	 (struct.get $output-port $buf (local.get $op)))

      (return (local.get $res)))

   ;; bgl_reset_output_port_error
   (func $bgl_reset_output_port_error (export "bgl_reset_output_port_error")
      (param $op (ref $output-port))
      (result (ref eq))
      (local.get $op))


   ;; bgl_port_isatty
   (func $bgl_port_isatty (export "bgl_port_isatty")
      (param $op (ref $output-port))
      (result i32)

      (if (ref.test (ref $fd-output-port) (local.get $op))
	  (then
	     (return_call $js_isatty
		(struct.get $fd-output-port $fd
		   (ref.cast (ref $fd-output-port) (local.get $op)))))
	  (else
	   (return (i32.const 0)))))
   
   ;; -----------------------------------------------------------------
   ;; Initialization 
   ;; -----------------------------------------------------------------
   (func $bgl_init_io
      (local $denv (ref $dynamic-env))
      (if (call $js_isatty (i32.const 1))
	  (then
	     (global.set $_stdout
		(struct.new $file-output-port
		   ;; name
		   (array.new_data $bstring $stdout-name
		      (i32.const 0) (i32.const 6))
		   ;; chook
		   (global.get $BUNSPEC)
		   ;; isclosed
		   (i32.const 0)
		   ;; sysclose
		   (ref.null $sysclose_t)
		   ;; sysseek
		   (ref.func $_LSEEK)
		   ;; buf
		   (array.new_default $bstring (i32.const 0))
		   ;; index
		   (i32.const 0)
		   ;; bufmode
		   (global.get $BGL_IOLBF)
		   ;; syswrite
		   (ref.func $bgl_syswrite)
		   ;; fhook
		   (global.get $BUNSPEC)
		   ;; sysflush
		   (ref.null $sysflush_t)
		   ;; flushbuf
		   (global.get $BUNSPEC)
		   ;; err
		   (i32.const 0)
		   ;; File descriptor
		   (i32.const 1))))
	  (else
	   (global.set $_stdout
	      (struct.new $file-output-port
		 ;; name
		 (array.new_data $bstring $stdout-name
		    (i32.const 0) (i32.const 6))
		 ;; chook
		 (global.get $BUNSPEC)
		 ;; isclosed
		 (i32.const 0)
		 ;; sysclose
		 (ref.null $sysclose_t)
		 ;; sysseek
		 (ref.func $_LSEEK)
		 ;; buf
		 (array.new_default $bstring (i32.const 8192))
		 ;; index
		 (i32.const 0)
		 ;; bufmode
		 (global.get $BGL_IOFBF)
		 ;; syswrite
		 (ref.func $bgl_syswrite)
		 ;; fhook
		 (global.get $BUNSPEC)
		 ;; sysflush
		 (ref.null $sysflush_t)
		 ;; flushbuf
		 (global.get $BUNSPEC)
		 ;; err
		 (i32.const 0)
		 ;; File descriptor
		 (i32.const 1)))))
      (global.set $_stderr
	 (struct.new $file-output-port
	    ;; name
	    (array.new_data $bstring $stderr-name
	       (i32.const 0) (i32.const 6))
	    ;; chook
	    (global.get $BUNSPEC)
	    ;; isclosed
	    (i32.const 0)
	    ;; sysclose
	    (ref.null $sysclose_t)
	    ;; sysseek
	    (ref.func $_LSEEK)
	    ;; buf
	    (array.new_default $bstring (i32.const 1))
	    ;; index
	    (i32.const 0)
	    ;; bufmode
	    (global.get $BGL_IOFBF)
	    ;; syswrite
	    (ref.func $bgl_syswrite)
	    ;; fhook
	    (global.get $BUNSPEC)
	    ;; sysflush
	    (ref.null $sysflush_t)
	    ;; flushbuf
	    (global.get $BUNSPEC)
	    ;; err
	    (i32.const 0)
	    ;; File descriptor
	    (i32.const 2)))
      (global.set $_stdin
	 (struct.new $console-input-port
	    ;; name
	    (array.new_data $bstring $stdin-name
	       (i32.const 0) (i32.const 5))
	    ;; chook
	    (global.get $BUNSPEC)
	    ;; isclosed
	    (i32.const 0)
	    ;; sysclose
	    (ref.null $sysclose_t)
	    ;; sysseek
	    (ref.null $sysseek_t)
	    ;; userseek
	    (global.get $BUNSPEC)
	    ;; rgc
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
	       (i32.const 0x0A #;(ASCII NEWLINE '\n'))
	       ;; buffer
	       (array.new_default $bstring (i32.const 128)))
	    ;; fd
	    (i32.const 0)))

      ;; update the current dynamic env
      (local.set $denv (call $BGL_CURRENT_DYNAMIC_ENV))
      (struct.set $dynamic-env $current-output-port (local.get $denv)
	 (global.get $_stdout))
      (struct.set $dynamic-env $current-error-port (local.get $denv)
	 (global.get $_stderr))
      (struct.set $dynamic-env $current-input-port (local.get $denv)
	 (global.get $_stdin)))
   )
   

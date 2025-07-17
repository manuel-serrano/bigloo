;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wports.wat         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 27 10:34:00 2024                          */
;*    Last change :  Thu Jul 17 14:46:54 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Input/Output Ports WASM implementation.                          */
;*=====================================================================*/

(module $__bigloo_ports

   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $string-output-port-name "string")
   (data $procedure-output-port-name "procedure")
   (data $string-input-port-name "string")
   (data $stdout-name "stdout")
   (data $stderr-name "stderr")
   (data $stdin-name "stdin")

   (data $SET_INPUT_PORT_POSITION "set-input-port-position!")
   (data $DOES_NOT_SUPPORT "operation not supported")
   (data $INPUT_PROCEDURE_PORT "input-procedure-port")
   (data $BAD_TYPE "wrong type")
   (data $FLUSH "flush")
   (data $PORT_CLOSED "port closed")
   
   (global $BGL_IONB i32 (i32.const 0))  ;; unubuffered
   (global $BGL_IOLBF i32 (i32.const 1)) ;; line buffered
   (global $BGL_IOFBF i32 (i32.const 2)) ;; fully buffered
   (global $BGL_IOEBF i32 (i32.const 3)) ;; extensible buffer

   (global $WHENCE_SEEK_CUR i32 (i32.const 0))
   (global $WHENCE_SEEK_END i32 (i32.const 1))
   (global $WHENCE_SEEK_SET i32 (i32.const 3))

   (global $default_io_bufsize (export "default_io_bufsize") i64 (i64.const 8192))

   (elem declare func $_CLOSE $_LSEEK $strseek $strwrite
      $bgl_input_file_seek $bgl_sysread $bgl_proc_read
      $bgl_input_string_seek $bgl_input_mmap_seek $bgl_mmap_read $bgl_eof_read
      $bgl_syswrite $get_output_string_as_refeq $procclose $procwrite $procflush)
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------

   (import "__js_io" "open_file" (func $js_open_file (param i32 i32 i32) (result i32)))
   (import "__js_io" "open_fd" (func $js_open_fd (param i32 i32 i32) (result i32)))
   (import "__js_io" "close_file" (func $js_close_file (param i32)))
   (import "__js_io" "read_file" (func $js_read_file (param i32 i32 i32 i32) (result i32)))
   (import "__js_io" "path_size" (func $js_path_size (param i32) (param i32) (result i32)))
   (import "__js_io" "last_modification_time" (func $js_last_modification_time (param i32) (param i32) (result f64)))
   (import "__js_io" "file_size" (func $js_file_size (param i32) (result i32)))
   (import "__js_io" "isatty" (func $js_isatty (param i32) (result i32)))
   (import "__js_io" "file_exists" (func $js_file_exists (param i32) (param i32) (result i32)))
   (import "__js_io" "append_file" (func $js_append_file (param i32 i32 i32) (result i32)))
   (import "__js_io" "write_file" (func $js_write_file (param i32 i32 i32 i32) (result i32)))
   (import "__js_io" "append_char" (func $js_append_char (param i32 i32) (result i32)))
   (import "__js_io" "write_char" (func $js_write_char (param i32 i32 i32) (result i32)))
   (import "__js_io" "write_bignum" (func $js_write_bignum (param i32 i32 i32) (result i32)))
   (import "__js_io" "file_delete" (func $js_file_delete (param i32) (param i32) (result i32)))
   (import "__js_io" "make_dir" (func $js_make_dir (param i32) (param i32) (param i32) (result i32)))
   (import "__js_io" "dir_remove" (func $js_dir_remove (param i32) (param i32) (result i32)))
   (import "__js_io" "is_dir" (func $js_directoryp (param i32) (param i32) (result i32)))
   (import "__js_io" "read_dir_init" (func $js_read_dir_init (param i32) (param i32) (result externref)))
   (import "__js_io" "read_dir_size" (func $js_read_dir_size (param externref) (result i32)))
   (import "__js_io" "read_dir_entry" (func $js_read_dir_entry (param externref) (param i32) (param i32) (result i32)))

   (import "__js_io" "file_separator" (global $js_file_separator i32))
   (import "__js_io" "password" (func $js_password (param i32 i32 i32) (result i32)))
   (import "__js_io" "ftruncate" (func $js_ftruncate (param i32 i32) (result i32)))
   (import "__js_io" "truncate" (func $js_truncate (param i32 i32 i32) (result i32)))
   (import "__js_io" "rename" (func $js_rename (param i32 i32 i32 i32) (result i32)))

   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref $bbool)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref $bbool)))
   (import "__bigloo" "BEOF" (global $BEOF (ref $bcnst)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BGL_IO_WRITE_ERROR" (global $BGL_IO_WRITE_ERROR i32))
   (import "__bigloo" "BGL_RGC_DEFAULT_VALUE" (global $rgc-default-value (ref $rgc)))
   (import "__bigloo" "RGC_BUFFER_MATCH_LENGTH" (func $RGC_BUFFER_MATCH_LENGTH (param $port (ref $input-port)) (result i64)))
   (import "__bigloo" "STRINGP" (func $STRINGP (param (ref eq)) (result i32)))
   (import "__bigloo" "STRING_LENGTH" (func $STRING_LENGTH (param (ref $bstring)) (result i64)))
   (import "__bigloo" "string_to_bstring_len" (func $string_to_bstring_len (param (ref $bstring)) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_string_shrink" (func $bgl_string_shrink (param (ref $bstring)) (param i64) (result (ref $bstring))))
   (import "__bigloo" "BGL_MMAP_LENGTH" (func $BGL_MMAP_LENGTH (param (ref $mmap)) (result i64)))
   (import "__bigloo" "BGL_MMAP_REF" (func $BGL_MMAP_REF (param $o (ref $mmap)) (param $i i64) (result i32)))
   (import "__bigloo" "bgl_load_string_in_buffer" (func $load_string_in_buffer (param i32) (param i32) (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_funcall0" (func $funcall0 (param (ref $procedure)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall1" (func $funcall1 (param (ref $procedure)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_funcall2" (func $funcall2 (param (ref $procedure)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "bgl_memcpy" (func $memcpy (param $dest i32) (param (ref $bstring)) (param i32) (param i32)))
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "INTEGERP" (func $INTEGERP (param (ref eq)) (result i32)))
   (import "__bigloo" "OBJ_TO_INT" (func $OBJ_TO_INT (param (ref eq)) (result i64)))
   (import "__bigloo" "BGL_CURRENT_DYNAMIC_ENV" (func $BGL_CURRENT_DYNAMIC_ENV (result (ref $dynamic-env))))
   (import "__bigloo" "RGC_BUFFER_AVAILABLE" (func $RGC_BUFFER_AVAILABLE (param (ref $input-port)) (result i32)))
   
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

   ;; sysread_t
   (type $sysread_t
      (func (param (ref eq))
	 (param (ref $bstring))
	 (param i32)
	 (param i32)
	 (result i32)))
   
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
	    (field $fd i32)
	    (field $position (mut i32)))))

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
	 (i32.const -1)
	 ;; position
	 (i32.const 0)))

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

   ;; procedure-output-port
   (type $procedure-output-port
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
	    (field $err (mut i32))
	    (field $proc (ref $procedure))
	    (field $flush (ref $procedure))
	    (field $close (ref $procedure)))))

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
	    (field $sysread (mut (ref null $sysread_t)))
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
	 ;; sysread
	 (ref.null $sysread_t)
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
	    (field $sysread (mut (ref null $sysread_t)))
	    (field $rgc (ref $rgc))
	    (field $fd (mut i32))
	    (field $position (mut i32)))))

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
	    (field $sysread (mut (ref null $sysread_t)))
	    (field $rgc (ref $rgc))
	    (field $fd (mut i32))
	    (field $position (mut i32)))))

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
	 ;; sysread
	 (ref.null $sysread_t)
	 ;; rgc
	 (global.get $rgc-default-value)
	 ;; fd
	 (i32.const -1)
	 ;; position
	 (i32.const 0)))

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
	    (field $sysread (mut (ref null $sysread_t)))
	    (field $rgc (ref $rgc))
	    (field $fd (mut i32))
	    (field $position (mut i32)))))

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
	    (field $sysread (mut (ref null $sysread_t)))
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
	 ;; sysread
	 (ref.null $sysread_t)
	 ;; rgc
	 (global.get $rgc-default-value)
	 ;; offset
	 (i64.const 0)))

   ;; procedure-input-port
   (type $procedure-input-port
      (sub final $input-port
	 (struct
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $sysread (mut (ref null $sysread_t)))
	    (field $rgc (ref $rgc))
	    (field $proc (ref $procedure))
	    (field $pbuffer (mut (ref eq)))
	    (field $pbufpos (mut i32)))))

   ;; mmap-input-port
   (type $mmap-input-port
      (sub final $input-port
	 (struct 
	    (field $name (mut (ref $bstring)))
	    (field $chook (mut (ref eq)))
	    (field $isclosed (mut i32))
	    (field $sysclose (mut (ref null $sysclose_t)))
	    (field $sysseek (mut (ref null $sysseek_t)))
	    (field $userseek (mut (ref eq)))
	    (field $sysread (mut (ref null $sysread_t)))
	    (field $rgc (ref $rgc))
	    (field $mmap (ref $mmap))
	    (field $offset (mut i64))
	    (field $start i64)
	    (field $end i64))))
   
   ;; -----------------------------------------------------------------
   ;; Predicates
   ;; -----------------------------------------------------------------
   
   (func $INPUT_PORTP (export "INPUT_PORTP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $input-port) (local.get $o)))
   
   (func $INPUT_STRING_PORTP (export "INPUT_STRING_PORTP")
      (param $port (ref eq))
      (result i32)
      (ref.test (ref $string-input-port) (local.get $port)))

   (func $INPUT_MMAP_PORTP (export "INPUT_MMAP_PORTP")
      (param $port (ref eq))
      (result i32)
      (ref.test (ref $mmap-input-port) (local.get $port)))

   (func $INPUT_PROCEDURE_PORTP (export "INPUT_PROCEDURE_PORTP")
      (param $port (ref eq))
      (result i32)
      (ref.test (ref $procedure-input-port) (local.get $port)))

   (func $INPUT_GZIP_PORTP (export "INPUT_GZIP_PORTP")
      (param $port (ref eq))
      (result i32)
      ;;(ref.test (ref $gzip-input-port) (local.get $port))
      (i32.const 0))

   (func $OUTPUT_PORTP (export "OUTPUT_PORTP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $output-port) (local.get $o)))
   
   (func $BGL_OUTPUT_PROCEDURE_PORTP (export "BGL_OUTPUT_PROCEDURE_PORTP")
      (param $port (ref eq))
      (result i32)
      (ref.test (ref $procedure-output-port) (local.get $port)))
   
   (func $BGL_OUTPUT_STRING_PORTP (export "BGL_OUTPUT_STRING_PORTP")
      (param $port (ref eq))
      (result i32)
      (ref.test (ref $string-output-port) (local.get $port)))
   
   ;; -----------------------------------------------------------------
   ;; Common macros
   ;; -----------------------------------------------------------------
   (func $EOF_OBJECTP (export "EOF_OBJECTP")
      (param $v (ref eq))
      (result i32)
      (ref.eq (local.get $v) (global.get $BEOF)))

   (func $BGL_PORT_CLOSED_P (export "BGL_PORT_CLOSED_P")
      (param $op (ref $port))
      (result i32)

      (return (struct.get $port $isclosed (local.get $op))))

   (func $BGL_OUTPUT_PORT_CNT
      (param $op (ref $output-port))
      (result i32)
      
      (return
	 (i32.sub (array.len (struct.get $output-port $buf (local.get $op)))
	    (struct.get $output-port $index (local.get $op)))))

   (func $BGL_OUTPUT_PORT_BUFFER (export "BGL_OUTPUT_PORT_BUFFER")
      (param $op (ref $output-port))
      (result (ref $bstring))

      (return (struct.get $output-port $buf (local.get $op))))
   
   (func $BGL_OUTPUT_PORT_FLUSHBUF
      (param $op (ref $output-port))
      (result (ref eq))

      (return (struct.get $output-port $flushbuf (local.get $op))))

   (func $BGL_OUTPUT_PORT_FILEPOS
      (export "BGL_OUTPUT_PORT_FILEPOS")
      (param $op (ref $output-port))
      (result i64)
      (return
	 (if (result i64) (ref.test (ref $file-output-port) (local.get $op))
	     (then
		(i64.extend_i32_u
		   (struct.get $file-output-port $position
		      (ref.cast (ref $file-output-port) (local.get $op)))))
	     (else
	      (i64.const 0)))))

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
   
   (func $INPUT_PORT_FILLBARRIER (export "INPUT_PORT_FILLBARRIER")
      (param $ip (ref $input-port))
      (result i64)
      (return (i64.extend_i32_u
		 (struct.get $rgc $fillbarrier
		    (struct.get $input-port $rgc (local.get $ip))))))
   
   (func $INPUT_PORT_FILLBARRIER_SET (export "INPUT_PORT_FILLBARRIER_SET")
      (param $ip (ref $input-port))
      (param $v i64)
      (return (struct.set $rgc $fillbarrier
		 (struct.get $input-port $rgc (local.get $ip))
		 (i32.sub (i32.wrap_i64 (local.get $v))
		    (call $RGC_BUFFER_AVAILABLE (local.get $ip))))))
   
   (func $INPUT_PORT_TOKENPOS (export "INPUT_PORT_TOKENPOS")
      (param $ip (ref $input-port))
      (result i64)
      (return (i64.sub (call $INPUT_PORT_FILEPOS (local.get $ip))
		 (call $RGC_BUFFER_MATCH_LENGTH (local.get $ip)))))

   (func $BGL_INPUT_PORT_LENGTH (export "BGL_INPUT_PORT_LENGTH")
      (param $ip (ref $input-port))
      (result i64)
      (if (ref.test (ref $file-input-port) (local.get $ip))
	  (then
	     (return
		(i64.extend_i32_u
		   (call $js_file_size
		      (struct.get $file-input-port $fd
			 (ref.cast (ref $file-input-port) (local.get $ip)))))))
	  (else
	   (if (ref.test (ref $mmap-input-port) (local.get $ip))
	       (then
		  (return
		     (call $BGL_MMAP_LENGTH
			(struct.get $mmap-input-port $mmap
			   (ref.cast (ref $mmap-input-port) (local.get $ip))))))
	       (else
		(return
		   (i64.extend_i32_u
		      (array.len
			 (struct.get $rgc $buf
			    (struct.get $input-port $rgc (local.get $ip))))))))))
      (unreachable))

   (func $INPUT_PORT_ON_FILEP
      (param $port (ref eq))
      (result i32)
      (return (ref.test (ref $file-input-port) (local.get $port))))
   
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
	  (then (call $js_close_file
		   (struct.get $fd-output-port $fd
		      (ref.cast (ref $fd-output-port) (local.get $op)))))
	  (else
	   (if (ref.test (ref $file-input-port) (local.get $op))
	       (then
		  (call $js_close_file
		     (struct.get $file-input-port $fd
			(ref.cast (ref $file-input-port)
			   (local.get $op)))))))))
      
   ;; _LSEEK
   (func $_LSEEK
      (param $port (ref eq))
      (param $offset i32)
      (param $whence i32)
      
      (if (ref.test (ref $file-input-port) (local.get $port))
	  (then
	     (struct.set $file-input-port $position
		(ref.cast (ref $file-input-port) (local.get $port))
		(local.get $offset)))))

   ;; bgl_input_file_seek
   (func $bgl_input_file_seek
      (param $p (ref eq))
      (param $pos i32)
      (param $whence i32)

      (local $ip (ref $file-input-port))
      (local $rgc (ref $rgc))
      
      (local.set $ip (ref.cast (ref $file-input-port) (local.get $p)))
      (local.set $rgc (struct.get $file-input-port $rgc (local.get $ip)))

      (struct.set $rgc $filepos (local.get $rgc) (local.get $pos))
      (struct.set $rgc $eof (local.get $rgc) (i32.const 0))
      (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
      (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
      (struct.set $rgc $forward (local.get $rgc) (i32.const 0))
      (struct.set $rgc $bufpos (local.get $rgc) (i32.const 0))
      (struct.set $rgc $lastchar (local.get $rgc) (i32.const 13))

      (struct.set $file-input-port $position (local.get $ip) (local.get $pos)))
      
   ;; bgl_syswrite
   (func $bgl_syswrite
      (param $p (ref eq))
      (param $buf (ref $bstring))
      (param $start i32)
      (param $count i32)
      (result i32)

      (local $op (ref $fd-output-port))
      (local $fop (ref $file-output-port))
      (local $nbwrite i32)
      (local.set $op (ref.cast (ref $fd-output-port) (local.get $p)))

      (call $memcpy
	 (i32.const 128)
	 (local.get $buf)
	 (local.get $start)
	 (local.get $count))

      (if (ref.test (ref $file-output-port) (local.get $op))
	  (then
	     (local.set $fop (ref.cast (ref $file-output-port) (local.get $op)))
	     (local.set $nbwrite
		(call $js_write_file
		   (struct.get $file-output-port $fd (local.get $fop))
		   (i32.const 128)
		   (local.get $count)
		   (struct.get $file-output-port $position (local.get $fop))))
	     (struct.set $file-output-port $position (local.get $fop)
		(i32.add
		   (local.get $nbwrite
		      (struct.get $file-output-port $position (local.get $fop))))))
	  (else
	   (drop
	      (call $js_append_file
		 (struct.get $fd-output-port $fd (local.get $op))
		 (i32.const 128)
		 (local.get $count)))))

      (return (local.get $count)))

   ;; bgl_eof_read
   (func $bgl_eof_read
      (param $p (ref eq))
      (param $buf (ref $bstring))
      (param $start i32)
      (param $size i32)
      (result i32)

      (return (i32.const 0)))
   
   ;; bgl_sysread
   (func $bgl_sysread
      (param $p (ref eq))
      (param $buf (ref $bstring))
      (param $start i32)
      (param $size i32)
      (result i32)

      (local $nbread i32)
      (local $ip (ref $fd-input-port))
      (local $position i32)
      
      (local.set $ip (ref.cast (ref $fd-input-port) (local.get $p)))
      (local.set $position (struct.get $fd-input-port $position (local.get $ip)))

      (local.set $nbread
	 (call $js_read_file
	    (struct.get $fd-input-port $fd (local.get $ip))
	    (i32.const 128)
	    (local.get $size)
	    (local.get $position)))

      (call $load_string_in_buffer
	 (i32.const 128)
	 (local.get $nbread)
	 (local.get $buf)
	 (local.get $start))

      (struct.set $fd-input-port $position (local.get $ip)
	 (i32.add (local.get $position) (local.get $nbread)))

      (return (local.get $nbread)))

   ;; bgl_proc_read_buf
   (func $bgl_proc_read_buf
      (param $ip (ref $procedure-input-port))
      (param $buf (ref $bstring))
      (param $b (ref $bstring))
      (param $start i32)
      (param $l i32)
      (result i32)
      
      (local $p i32)
      (local $r i32)
      
      (local.set $p (struct.get $procedure-input-port $pbufpos (local.get $ip)))
      (local.set $r (i32.sub (array.len (local.get $buf)) (local.get $p)))
      
      (if (i32.le_s (local.get $r) (local.get $l))
	  (then
	     (array.copy $bstring $bstring
		(local.get $b) (local.get $start)
		(local.get $buf) (local.get $p)
		(local.get $r))
	     (struct.set $procedure-input-port $pbuffer (local.get $ip) (global.get $BFALSE))
	     (struct.set $procedure-input-port $pbufpos (local.get $ip) (i32.const 0))
	     (return (local.get $r)))
	  (else
	   (array.copy $bstring $bstring
	      (local.get $b) (local.get $start)
	      (local.get $buf) (local.get $p)
	      (local.get $l))
	   (struct.set $procedure-input-port $pbufpos (local.get $ip)
	      (i32.add (struct.get $procedure-input-port $pbufpos (local.get $ip))
		 (local.get $l)))
	   (return (local.get $l))))
      (unreachable))
   
   ;; bgl_proc_read
   (func $bgl_proc_read
      (param $port (ref eq))
      (param $b (ref $bstring))
      (param $start i32)
      (param $l i32)
      (result i32)
      
      (local $ip (ref $procedure-input-port))
      (local $buf (ref eq))
      
      (local.set $ip (ref.cast (ref $procedure-input-port) (local.get $port)))
      (local.set $buf (struct.get $procedure-input-port $pbuffer (local.get $ip)))

      (if (ref.test (ref $bstring) (local.get $buf))
	  (then
	     (return_call $bgl_proc_read_buf (local.get $ip) (ref.cast (ref $bstring) (local.get $buf))
		(local.get $b) (local.get $start) (local.get $l)))
	  (else
	   (local.set $buf (call $funcall0 (struct.get $procedure-input-port $proc (local.get $ip))))
	   
	   (if (ref.test (ref $bstring) (local.get $buf))
	       (then
		  (return_call $bgl_proc_read_buf (local.get $ip) (ref.cast (ref $bstring) (local.get $buf))
		     (local.get $b) (local.get $start) (local.get $l)))
	       (else
		(if (ref.eq (local.get $buf) (global.get $BFALSE))
		    (then
		       (struct.set $rgc $eof (struct.get $procedure-input-port $rgc (local.get $ip)) (i32.const 1))
		       (return (i32.const 0)))
		    (else
		     (drop (call $the_failure
			      (array.new_data $bstring $INPUT_PROCEDURE_PORT (i32.const 0) (i32.const 21))
			      (array.new_data $bstring $BAD_TYPE (i32.const 0) (i32.const 10))
			      (local.get $port)))
		     (unreachable)))))))
      (unreachable))

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

   ;; bgl_mmap_read
   (func $bgl_mmap_read
      (param $port (ref eq))
      (param $b (ref $bstring))
      (param $start i32)
      (param $size i32)
      (result i32)
      
      (local $ip (ref $mmap-input-port))
      (local $mm (ref $mmap))
      (local $available i32)
      (local $n i32)
      (local $m i32)
      (local.set $ip (ref.cast (ref $mmap-input-port) (local.get $port)))
      (local.set $mm (struct.get $mmap-input-port $mmap (local.get $ip)))
      
      (local.set $available
	 (i32.wrap_i64
	    (i64.sub (struct.get $mmap-input-port $end (local.get $ip))
	       (struct.get $mmap-input-port $offset (local.get $ip)))))

      (if (i32.gt_s (local.get $available) (i32.const 0))
	  (then
	     (if (i32.gt_s (local.get $available) (local.get $size))
		 (then
		    (local.set $n (local.get $size)))
		 (else
		  (local.set $n (local.get $available))))
	     (loop $while
		(if (i32.lt_s (local.get $m) (local.get $n))
		    (then
		       (array.set $bstring (local.get $b)
			  (i32.add (local.get $start) (local.get $m))
			  (call $BGL_MMAP_REF (local.get $mm)
			     (i64.add
				(struct.get $mmap-input-port $offset (local.get $ip))
				(i64.extend_i32_u (local.get $m)))))
		       (local.set $m (i32.add (local.get $m) (i32.const 1)))
		       (br $while))))
	     (struct.set $mmap-input-port $offset
		(local.get $ip)
		(i64.add (struct.get $mmap-input-port $offset (local.get $ip))
		   (i64.extend_i32_u (local.get $n))))
	     (if (i32.eq (local.get $n) (local.get $available))
		 (then
		    (struct.set $rgc $eof
		       (struct.get $input-port $rgc (local.get $ip))
		       (i32.const 1))))
	     (return (local.get $n)))
	  (else
	   (return (i32.const 0))))
      (unreachable))

      ;; bgl_input_mmap_seek
   (func $bgl_input_mmap_seek
      (param $p (ref eq))
      (param $pos i32)
      (param $whence i32)
      
      (local $mp (ref $mmap-input-port))
      (local $rgc (ref $rgc))
      (local.set $mp (ref.cast (ref $mmap-input-port) (local.get $p)))
      (local.set $rgc (struct.get $input-port $rgc (local.get $mp)))
      
      (if (i32.ge_s (local.get $pos) (i32.const 0))
	  (then
	     (if (i32.lt_s (local.get $pos)
		    (i32.wrap_i64 (call $BGL_INPUT_PORT_LENGTH (local.get $mp))))
		 (then
		    (struct.set $mmap-input-port $offset (local.get $mp)
		       (i64.add (struct.get $mmap-input-port $start (local.get $mp))
			  (i64.extend_i32_u (local.get $pos))))
		    (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
		    (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
		    (struct.set $rgc $forward (local.get $rgc) (i32.const 0))
		    (struct.set $rgc $bufpos (local.get $rgc) (i32.const 0))))))
      
      (if (i32.eq (local.get $pos)
	     (i32.wrap_i64 (call $BGL_INPUT_PORT_LENGTH (local.get $mp))))
	  (then
	     (struct.set $rgc $eof (local.get $rgc) (i32.const 1)))))

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
	   (if (result i32) (ref.test (ref $procedure-input-port) (local.get $ip))
	       (then
		  (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
		  (struct.set $rgc $eof (local.get $rgc) (i32.const 0))
		  (i32.const 1))
	       (else (i32.const 0))))))

   ;; bgl_close_input_port 
   (func $bgl_close_input_port (export "bgl_close_input_port")
      (param $ip (ref $input-port))
      (result (ref eq))

      (local $proc (ref $procedure))
      (local $rgc (ref $rgc))
      
      (if (call $BGL_PORT_CLOSED_P (local.get $ip))
	  ;; the port is already closed
	  (then
	     (return (local.get $ip))))
      
      ;; mark it closed
      (struct.set $input-port $isclosed (local.get $ip) (i32.const 1))
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (struct.set $rgc $forward (local.get $rgc)
	 (struct.get $rgc $bufpos (local.get $rgc)))

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
	   (return (local.get $ip))))
      (unreachable))

   ;; bgl_input_port_seek
   (func $bgl_input_port_seek
      (export "bgl_input_port_seek")
      (param $port (ref $input-port))
      (param $pos i64)
      (result (ref eq))
      
      (local $sysseek (ref null $sysseek_t))
      (local.set $sysseek (struct.get $input-port $sysseek (local.get $port)))

      (if (ref.is_null (local.get $sysseek))
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $SET_INPUT_PORT_POSITION (i32.const 0) (i32.const 24))
		      (array.new_data $bstring $DOES_NOT_SUPPORT (i32.const 0) (i32.const 23))
		      (local.get $port)))
	     (unreachable))
	  (else
	   (call_ref $sysseek_t (local.get $port) (i32.wrap_i64 (local.get $pos))
	      (global.get $WHENCE_SEEK_SET)
	      (local.get $sysseek))
	   (return (local.get $port))))
      (unreachable))
   
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
		(return (local.get $res))))))
      (unreachable))

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
		   (call $BINT (local.get $slen))))
      
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
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $FLUSH (i32.const 0) (i32.const 4))
		      (array.new_data $bstring $PORT_CLOSED (i32.const 0) (i32.const 11))
		      (local.get $op)))
	     (unreachable)))
      
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
	   (if (i32.add ;; actual or operator
		  (i32.gt_s (local.get $slen) (i32.const 0))
		  (i32.eq (local.get $cnt) (i32.const 0)))
	       (then
		  (local.set $n
		     (call_ref $syswrite_t
			(local.get $op)
			(ref.cast (ref $bstring) (local.get $str))
			(local.get $start)
			(local.get $slen)
			(ref.cast (ref $syswrite_t)
			   (struct.get $output-port $syswrite
			      (local.get $op)))))
		  (if (i32.lt_s (local.get $n) (i32.const 0))
		      (then
			 (if (local.get $err)
			     (then
				(throw $fail)))))))))
	   
      (return (local.get $op))
      (unreachable))

   ;; bgl_output_flush
   (func $bgl_output_flush (export "bgl_output_flush")
      (param $op (ref $output-port))
      (param $str (ref null $bstring))
      (param $start i32)
      (param $size i32)
      (result (ref eq))
      (call $output_flush (local.get $op)
	 (local.get $str) (local.get $start) (local.get $size)
	 (i32.const 0) (i32.const 1))
      (if (ref.is_null (struct.get $output-port $sysflush (local.get $op)))
	  (then
	     (return (global.get $BTRUE)))
	  (else
	   (return_call_ref $sysflush_t
	      (local.get $op)
	      (struct.get $output-port $sysflush (local.get $op)))))
      (unreachable))

   ;; $charbuf
   (global $charbuf (ref $bstring)
      (array.new_fixed $bstring 1 (i32.const 0)))
   
   ;; bgl_output_flush_char
   (func $bgl_output_flush_char (export "bgl_output_flush_char")
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
	   (return (global.get $BTRUE))))
      (unreachable))

   ;; bgl_write
   (func $bgl_write (export "bgl_write")
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
				 (array.get_u $bstring (local.get $str)
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
	      (local.get $str) (local.get $start) (local.get $sz))))
      (unreachable))

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

      (struct.set $output-port $index (local.get $op)
	 (i32.add (local.get $used) (local.get $count)))
      (struct.set $output-port $buf (local.get $op)
	 (local.get $nbuf))
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
      
      (call $store_string (local.get $path) (i32.const 128))
      (local.set $fd
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $path))
	    ;; READ-ONLY flag
	    (i32.const 0)))

      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then (return (global.get $BFALSE))))
      
      (local.set $rgc
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
	 (ref.func $_CLOSE)
	 ;; sysseek
	 (ref.func $bgl_input_file_seek)
	 ;; userseek
	 (global.get $BUNSPEC)
	 ;; sysread
	 (ref.func $bgl_sysread)
	 ;; rgc
	 (local.get $rgc)
	 ;; File descriptor
	 (local.get $fd)
	 ;; position
	 (i32.const 0)))

   ;; bgl_open_input_resource
   (func $bgl_open_input_resource (export "bgl_open_input_resource")
      (param $path (ref $bstring))
      (param $buffer (ref $bstring))
      (result (ref eq))
      (return (global.get $BFALSE)))

   ;; bgl_open_input_descriptor
   (func $bgl_open_input_descriptor (export "bgl_open_input_descriptor")
      (param $fd i32)
      (param $buffer (ref $bstring))
      (result (ref eq))
      (local $rgc (ref $rgc))
      (local $name (ref $bstring))
      
      (local.set $name
	 (call $load_string (i32.const 128)
	    (call $js_open_fd
	       (local.get $fd)
	       ;; READ-ONLY flag
	       (i32.const 0)
	       (i32.const 128))))

      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then (return (global.get $BFALSE))))
      
      (local.set $rgc
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
      
      (struct.new $fd-input-port
	 ;; name
	 (local.get $name)
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 0)
	 ;; sysclose
	 (ref.func $_CLOSE)
	 ;; sysseek
	 (ref.func $bgl_input_file_seek)
	 ;; userseek
	 (global.get $BUNSPEC)
	 ;; sysread
	 (ref.func $bgl_sysread)
	 ;; rgc
	 (local.get $rgc)
	 ;; File descriptor
	 (local.get $fd)
	 ;; position
	 (i32.const 0)))

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
	    ;; fillbarrier
	    (i32.const -1)
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
	    ;; sysread
	    (ref.func $bgl_eof_read)
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
	 (local.get $buf) (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $buf)))))
     
   ;; bgl_open_input_string
   (func $bgl_open_input_string (export "bgl_open_input_string")
      (param $str (ref $bstring))
      (param $offset i64)
      (result (ref $string-input-port))
      (return_call $bgl_open_input_substring
	 (local.get $str) (local.get $offset)
	 (i64.extend_i32_u (array.len (local.get $str)))))
     
   ;; bgl_open_input_c_string
   (func $bgl_open_input_c_string (export "bgl_open_input_c_string")
      (param $str (ref $bstring))
      (result (ref $string-input-port))
      (return_call $bgl_open_input_string (local.get $str) (i64.const 0)))

   ;; bgl_reopen_input_c_string
   (func $bgl_reopen_input_c_string (export "bgl_reopen_input_c_string")
      (param $ip (ref $input-port))
      (param $str (ref $bstring))
      (result (ref eq))

      (local $rgc (ref $rgc))
      (local $buf (ref $bstring))
      (local.set $rgc (struct.get $input-port $rgc (local.get $ip)))
      (local.set $buf (struct.get $rgc $buf (local.get $rgc)))

      (if (i32.sub (array.len (local.get $buf)) (array.len (local.get $str)))
	  (then
	     (struct.set $rgc $buf
		(local.get $rgc)
		(array.new_default $bstring
		   (array.len (local.get $str))))))

      (struct.set $rgc $eof (local.get $rgc) (i32.const 1))
      (struct.set $rgc $filepos (local.get $rgc) (i32.const 0))
      (struct.set $rgc $forward (local.get $rgc) (i32.const 0))
      (struct.set $rgc $bufpos (local.get $rgc) (array.len (local.get $str)))
      (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
      (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
      (struct.set $rgc $lastchar (local.get $rgc) (i32.const 13))
      
      (array.copy $bstring $bstring
	 (struct.get $rgc $buf (local.get $rgc)) (i32.const 0)
	 (local.get $str) (i32.const 0)
	 (array.len (local.get $str)))

      (return (local.get $ip)))

   ;; bgl_open_input_procedure
   (func $bgl_open_input_procedure
      (export "bgl_open_input_procedure")
      (param $fun (ref $procedure))
      (param $buffer (ref $bstring))
      (result (ref $input-port))
      
      (local $rgc (ref $rgc))
      (local.set $rgc
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
	    (i32.const 13)
	    ;; buffer
	    (local.get $buffer)))
      
      (return
	 (struct.new $procedure-input-port
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
	    (ref.null $sysseek_t)
	    ;; userseek
	    (global.get $BUNSPEC)
	    ;; sysread
	    (ref.func $bgl_proc_read)
	    ;; rgc
	    (local.get $rgc)
	    ;; proc
	    (local.get $fun)
	    ;; pbuffer
	    (global.get $BUNSPEC)
	    ;; pbufpos
	    (i32.const 0))))

   ;; bgl_open_input_mmap
   (func $bgl_open_input_mmap
      (export "bgl_open_input_mmap")
      (param $mmap (ref $mmap))
      (param $buffer (ref $bstring))
      (param $offset i64)
      (param $end i64)
      (result (ref $input-port))

      (local $rgc (ref $rgc))
      (local.set $rgc
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
	    (i32.const 13)
	    ;; buffer
	    (local.get $buffer)))
      
      (return
	 (struct.new $mmap-input-port
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
	    (ref.func $bgl_input_mmap_seek)
	    ;; userseek
	    (global.get $BUNSPEC)
	    ;; sysread
	    (ref.func $bgl_mmap_read)
	    ;; rgc
	    (local.get $rgc)
	    ;; mmap
	    (local.get $mmap)
	    ;; offset
	    (local.get $offset)
	    ;; start
	    (local.get $offset)
	    ;; end
	    (local.get $end))))
   
   ;; open_output_file
   (func $open_output_file
      (param $name (ref $bstring))
      (param $buf (ref $bstring))
      (param $flags i32)
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
	    (local.get $flags)))
      
      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then (return (global.get $BFALSE))))
      
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
	    (local.get $fd)
	    ;; position
	    (i32.const 0))))

   ;; bgl_open_output_file
   (func $bgl_open_output_file (export "bgl_open_output_file")
      (param $name (ref $bstring))
      (param $buf (ref $bstring))
      (result (ref eq))
      (return_call $open_output_file (local.get $name) (local.get $buf)
	 (i32.const 1)))

   ;; bgl_append_output_file
   (func $bgl_append_output_file (export "bgl_append_output_file")
      (param $name (ref $bstring))
      (param $buf (ref $bstring))
      (result (ref eq))
      (local $op (ref eq))
      (local.set $op
	 (call $open_output_file (local.get $name) (local.get $buf)
	    (i32.const 2)))
      (if (ref.test (ref $file-output-port) (local.get $op))
	  (then
	     (struct.set $file-output-port $position
		(ref.cast (ref $file-output-port) (local.get $op))
		(i32.wrap_i64 (call $bgl_file_size (local.get $name))))))
      (return (local.get $op)))
   
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
	 (ref.func $get_output_string_as_refeq)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; err
	 (i32.const 0)))

  (func $bgl_open_output_procedure
     (export "bgl_open_output_procedure")
     (param $proc (ref $procedure))
     (param $flush (ref $procedure))
     (param $close (ref $procedure))
     (param $buf (ref $bstring))
     (result (ref $output-port))
     
     (struct.new $procedure-output-port
	 ;; name
	 (array.new_data $bstring $procedure-output-port-name
	    (i32.const 0) (i32.const 9))
	 ;; chook
	 (global.get $BUNSPEC)
	 ;; isclosed
	 (i32.const 0)
	 ;; sysclose
	 (ref.func $procclose)
	 ;; sysseek
	 (ref.null $sysseek_t)
	 ;; buf
	 (array.new_fixed $bstring 0)
	 ;; index
	 (i32.const 0)
	 ;; bufmode
	 (global.get $BGL_IONB)
	 ;; syswrite
	 (ref.func $procwrite)
	 ;; fhook
	 (global.get $BUNSPEC)
	 ;; sysflush
	 (ref.func $procflush)
	 ;; flushbuf
	 (global.get $BUNSPEC)
	 ;; err
	 (i32.const 0)
	 ;; proc
	 (local.get $proc)
	 ;; flush
	 (local.get $flush)
	 ;; close
	 (local.get $close)))

   (func $bgl_input_port_reopen (export "bgl_input_port_reopen")
      (param $port (ref $input-port))
      (result (ref eq))
      
      (local $ip (ref $file-input-port))
      (local $rgc (ref $rgc))
      (local $nf i32)
      (local $path (ref $bstring))
      
      (if (i32.eqz (call $INPUT_PORT_ON_FILEP (local.get $port)))
	  (then
	     (if (call $INPUT_STRING_PORTP (local.get $port))
		 (then
		    (return_call $bgl_input_port_seek (local.get $port)
		       (i64.const 0)))
		 (else
		  (if (call $INPUT_MMAP_PORTP (local.get $port))
		      (then
			 (return_call $bgl_input_port_seek (local.get $port)
			    (i64.const 0)))
		      (else
		       (return (global.get $BFALSE))))))))

      (local.set $ip (ref.cast (ref $file-input-port) (local.get $port)))
      (local.set $rgc (struct.get $file-input-port $rgc (local.get $ip)))
      (local.set $path (struct.get $file-input-port $name (local.get $ip)))
      
      (call $js_close_file (struct.get $file-input-port $fd (local.get $ip)))
      
      (call $store_string (local.get $path) (i32.const 128))
      (local.set $nf
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $path))
	    (i32.const 0)))
      
      (if (i32.lt_s (local.get $nf) (i32.const 0))
	  (then
	     (return (global.get $BFALSE))))
      
      (struct.set $rgc $filepos (local.get $rgc) (i32.const 0))
      (struct.set $rgc $eof (local.get $rgc) (i32.const 0))
      (struct.set $rgc $matchstart (local.get $rgc) (i32.const 0))
      (struct.set $rgc $matchstop (local.get $rgc) (i32.const 0))
      (struct.set $rgc $forward (local.get $rgc) (i32.const 0))
      (struct.set $rgc $bufpos (local.get $rgc) (i32.const 0))
      (struct.set $rgc $lastchar (local.get $rgc) (i32.const 13))
      
      (return (global.get $BTRUE)))
 
   ;; -----------------------------------------------------------------
   ;; Misc functions 
   ;; -----------------------------------------------------------------

   ;; FILE_SEPARATOR
   (global $FILE_SEPARATOR (export "FILE_SEPARATOR") i32
      (global.get $js_file_separator))
      
   ;; fexists
   (func $fexists (export "fexists")
      (param $path (ref $bstring))
      (result i32)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      (return_call $js_file_exists
	 (i32.const 128)
	 (array.len (local.get $path))))

   ;; unlink
   (func $unlink (export "unlink")
      (param $path (ref $bstring))
      (result i32)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return_call $js_file_delete
	 (i32.const 128) (array.len (local.get $path))))

   ;; get_output_string
   (func $get_output_string (export "get_output_string")
      (param $op (ref $output-port))
      (result (ref $bstring))

      (return_call $string_to_bstring_len
	 (struct.get $output-port $buf (local.get $op))
	 (struct.get $output-port $index (local.get $op))))

   ;; get_output_string (needed only for function cast)
   (func $get_output_string_as_refeq
      (param $p (ref eq))
      (result (ref eq))
      (return_call $get_output_string
	 (ref.cast (ref $output-port)  (local.get $p))))

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

   ;; bgl_output_port_seek
   (func $bgl_output_port_seek (export "bgl_output_port_seek")
      (param $port (ref $output-port))
      (param $pos i64)
      (result (ref eq))
   
      (local $sysseek (ref null $sysseek_t))
      (local.set $sysseek (struct.get $output-port $sysseek (local.get $port)))

      (if (ref.is_null (local.get $sysseek))
	  (then
	     (return (global.get $BFALSE)))
	  (else
	   (call_ref $sysseek_t (local.get $port)
	      (i32.wrap_i64 (local.get $pos))
	      (global.get $WHENCE_SEEK_SET)
	      (local.get $sysseek))
	   (return (local.get $port))))
      (unreachable))

   ;; bgl_output_port_truncate
   (func $bgl_output_port_truncate (export "bgl_output_port_truncate")
      (param $port (ref $output-port))
      (param $pos i64)
      (result i32)

      (if (ref.test (ref $fd-output-port) (local.get $port))
	  (then
	     (return
		(i32.eq (i32.const 0)
		   (call $js_ftruncate
		      (struct.get $fd-output-port $fd
			 (ref.cast (ref $fd-output-port) (local.get $port)))
		      (i32.wrap_i64 (local.get $pos))))))
	  (else
	   (return (i32.const 0))))
      (unreachable))

   ;; truncate
   (func $truncate (export "truncate")
      (param $path (ref $bstring))
      (param $len i64)
      (result i32)
      
      (call $store_string (local.get $path) (i32.const 128))
      (return_call $js_truncate (i32.const 128)
	 (array.len (local.get $path))
	 (i32.wrap_i64 (local.get $len))))

   ;; rename
   (func $rename (export "rename")
      (param $old (ref $bstring))
      (param $new (ref $bstring))
      (result i32)
      (local $len i32)
      (local $len2 i32)

      (local.set $len (array.len (local.get $old)))
      (local.set $len2 (array.len (local.get $new)))
      (call $store_string (local.get $old) (i32.const 128))
      (call $store_string (local.get $new) (i32.add (local.get $len) (i32.const 128)))
      (return_call $js_rename (i32.const 128) (local.get $len)
	 (i32.add (local.get $len) (i32.const 128)) (local.get $len2)))

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
	   (return (i32.const 0))))
      (unreachable))

   ;; procwrite
   (func $procwrite
      (param $p (ref eq))
      (param $str (ref $bstring))
      (param $start i32)
      (param $sz i32)
      (result i32)
      
      (local $op (ref $procedure-output-port))
      (local $proc (ref eq))
      (local $buf (ref $bstring))
      
      (local.set $op (ref.cast (ref $procedure-output-port) (local.get $p)))
      (local.set $proc (struct.get $procedure-output-port $proc (local.get $op)))
      (local.set $buf (array.new_default $bstring (local.get $sz)))
      
      (array.copy $bstring $bstring
	 (local.get $buf) (i32.const 0)
	 (local.get $str) (local.get $start)
	 (local.get $sz))
      (call $funcall1
	 (struct.get $procedure-output-port $proc (local.get $op))
	 (local.get $buf))
      (return (local.get $sz)))
      
   ;; procflush
   (func $procflush
      (param $p (ref eq))
      (result (ref eq))
      
      (local $op (ref $procedure-output-port))
      (local.set $op (ref.cast (ref $procedure-output-port) (local.get $p)))

      (return_call $funcall0
	 (struct.get $procedure-output-port $flush (local.get $op))))
      
   ;; procclose
   (func $procclose
      (param $p (ref eq))

      (local $op (ref $procedure-output-port))
      (local.set $op (ref.cast (ref $procedure-output-port) (local.get $p)))

      (drop
	 (call $funcall0
	    (struct.get $procedure-output-port $close (local.get $op)))))
      
   ;; -----------------------------------------------------------------
   ;; Initialization 
   ;; -----------------------------------------------------------------
   (func $bgl_init_io (export "bgl_init_io")
      (local $denv (ref $dynamic-env))
      (if (call $js_isatty (i32.const 1))
	  (then
	     (global.set $_stdout
		(struct.new $fd-output-port
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
		   (ref.null $sysseek_t)
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
	      (struct.new $fd-output-port
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
		 (ref.null $sysseek_t)
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
	 (struct.new $fd-output-port
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
	    (ref.null $sysseek_t)
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
	    ;; sysread
	    (ref.func $bgl_sysread)
	    ;; rgc
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
	       (i32.const 0x0A #;(ASCII NEWLINE '\n'))
	       ;; buffer
	       (array.new_default $bstring (i32.const 128)))
	    ;; fd
	    (i32.const 0)
	    ;; position
	    (i32.const 0)))

      ;; update the current dynamic env
      (local.set $denv (call $BGL_CURRENT_DYNAMIC_ENV))
      (struct.set $dynamic-env $current-output-port (local.get $denv)
	 (global.get $_stdout))
      (struct.set $dynamic-env $current-error-port (local.get $denv)
	 (global.get $_stderr))
      (struct.set $dynamic-env $current-input-port (local.get $denv)
	 (global.get $_stdin)))

   ;; -----------------------------------------------------------------
   ;; directories 
   ;; -----------------------------------------------------------------

   (func $bgl_directoryp (export "bgl_directoryp")
      (param $path (ref $bstring))
      (result i32)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return_call $js_directoryp
	 (i32.const 128) (array.len (local.get $path))))

   (func $BGL_MKDIR (export "BGL_MKDIR")
      (param $path (ref $bstring))
      (param $mod i64)
      (result i32)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return_call $js_make_dir
	 (i32.const 128) (array.len (local.get $path))
	 (i32.wrap_i64 (local.get $mod))))
   
   (func $rmdir (export "rmdir")
      (param $path (ref $bstring))
      (result i32)

      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return_call $js_dir_remove
	 (i32.const 128) (array.len (local.get $path))))


   (func $bgl_directory_to_list (export "bgl_directory_to_list")
      (param $name (ref $bstring))
      (result (ref eq))

      (local $arr externref)
      (local $size i32)
      (local $res (ref eq))

      (call $store_string (local.get $name) (i32.const 128))

      (local.set $arr
	 (call $js_read_dir_init (i32.const 128) (array.len (local.get $name))))

      (if (ref.is_null (local.get $arr))
	  (then
	     (return (global.get $BFALSE)))
	  (else
	   (local.set $size (i32.sub (call $js_read_dir_size (local.get $arr))
			       (i32.const 1)))
	   (local.set $res (global.get $BNIL))
	   (loop $while
	      (if (i32.lt_s (local.get $size) (i32.const 0))
		  (then
		     (return (local.get $res)))
		  (else
		   (local.set $res
		      (struct.new $pair
			 (call $load_string
			    (i32.const 128)
			    (call $js_read_dir_entry (local.get $arr)
			       (local.get $size) (i32.const 128)))
			 (local.get $res)))
		   (local.set $size (i32.sub (local.get $size) (i32.const 1)))
		   (br $while))))))
      (unreachable))
   
   ;; -----------------------------------------------------------------
   ;; files 
   ;; -----------------------------------------------------------------
   
   (func $bgl_file_size (export "bgl_file_size")
      (param $path (ref $bstring))
      (result i64)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return
	 (i64.extend_i32_u
	    (call $js_path_size
	       (i32.const 128) (array.len (local.get $path))))))
   
   (func $bgl_last_modification_time (export "bgl_last_modification_time")
      (param $path (ref $bstring))
      (result i64)

      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      
      (return
	 (i64.trunc_f64_s
	    (call $js_last_modification_time
	       (i32.const 128) (array.len (local.get $path))))))
   
   ;; -----------------------------------------------------------------
   ;; password 
   ;; -----------------------------------------------------------------
   
   (func $bgl_password (export "bgl_password")
      (param $prompt (ref $bstring))
      (result (ref $bstring))
      (local $nb i32)
      
      (call $store_string (local.get $prompt) (i32.const 128))
      
      (return_call $load_string (i32.const 128)
	 (call $js_password
	    (i32.const 128)
	    (array.len (local.get $prompt))
	    (i32.const 128))))
   
   )
   

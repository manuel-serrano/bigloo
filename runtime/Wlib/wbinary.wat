;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wbinary.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  8 08:38:25 2025                          */
;*    Last change :  Wed Jan  8 14:29:59 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM Binary IO                                                   */
;*=====================================================================*/

(module $__bigloo_binary

   ;; -----------------------------------------------------------------
   ;; Global constants 
   ;; -----------------------------------------------------------------

   (data $MAGIC_WORD "1966")
   (data $INPUT_OBJ "input_obj")
   (data $BAD_MAGIC "bad magic number")
   
   (global $BGL_BINARY_CLOSED i32 (i32.const 2))

   (global $BGL_BINARY_INPUT i32 (i32.const 0))
   (global $BGL_BINARY_OUTPUT i32 (i32.const 1))

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------

   ;; binary-port
   (type $binary-port
      (struct
	 (field $name (mut (ref $bstring)))
	 (field $fd i32)
	 (field $io (mut i32))))

   (global $binary-port-default-value
      (export "BGL_BINARY_PORT_DEFAULT_VALUE") (ref $binary-port)
      (struct.new $binary-port
	 ;; name
	 (global.get $bstring-default-value)
	 ;; fd
	 (i32.const 0)
	 ;; io
	 (i32.const 0)))
   
   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   ;; open_output_binary_fd
   (func $open_output_binary_fd
      (param $path (ref $bstring))
      (param $flags i32)
      (result (ref eq))
      
      (local $fd i32)
      
      ;; TODO: support buffered output (for now, $buf is ignored)
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      (local.set $fd
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $path))
	    ;; WRITE-ONLY flag
	    (i32.const 1)))
      
      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then (return (global.get $BFALSE))))
      
      (struct.new $binary-port
	 (local.get $path)
	 (local.get $fd)
	 (global.get $BGL_BINARY_OUTPUT)))

      ;; open_output_binary_file
   (func $open_output_binary_file (export "open_output_binary_file")
      (param $path (ref $bstring))
      (result (ref eq))
      (return_call $open_output_binary_fd (local.get $path) (i32.const 1)))

   ;; append_output_binary_file
   (func $append_output_binary_file (export "append_output_binary_file")
      (param $path (ref $bstring))
      (result (ref eq))
      (return_call $open_output_binary_fd (local.get $path) (i32.const 2)))

   ;; open_input_binary_file
   (func $open_input_binary_file (export "open_input_binary_file")
      (param $path (ref $bstring))
      (result (ref eq))
      
      (local $fd i32)
      
      (call $store_string
	 (local.get $path)
	 (i32.const 128))
      (local.set $fd
	 (call $js_open_file
	    (i32.const 128)
	    (array.len (local.get $path))
	    ;; READ-ONLY flag
	    (i32.const 0)))
      
      (if (i32.lt_s (local.get $fd) (i32.const 0))
	  (then (return (global.get $BFALSE))))
      
      (struct.new $binary-port
	 (local.get $path)
	 (local.get $fd)
	 (global.get $BGL_BINARY_INPUT)))

   ;; close_binary_port
   (func $close_binary_port (export "close_binary_port")
      (param $bp (ref $binary-port))
      (result (ref $binary-port))
      (if (i32.ne (struct.get $binary-port $io (local.get $bp))
	     (global.get $BGL_BINARY_CLOSED))
	  (then
	     (call $js_close_file (struct.get $binary-port $fd (local.get $bp)))
	     (struct.set $binary-port $io (local.get $bp)
		(global.get $BGL_BINARY_CLOSED))))
      (local.get $bp))

   ;; bgl_flush_binary_port
   (func $bgl_flush_binary_port
      (export "bgl_flush_binary_port")
      (param $port (ref $binary-port))
      (result (ref eq))
      (return (local.get $port)))
   
   ;; BGL_OUTPUT_CHAR
   (func $BGL_OUTPUT_CHAR
      (export "BGL_OUTPUT_CHAR")
      (param $port (ref $binary-port))
      (param $c i32)
      (result (ref eq))

      (local $fd i32)
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      
      (i32.store8 (i32.const 128) (i32.and (local.get $c) (i32.const 255)))
      (call $js_write_file (local.get $fd) (i32.const 128) (i32.const 1))

      (return (global.get $BUNSPEC)))

   ;; BGL_INPUT_CHAR
   (func $BGL_INPUT_CHAR
      (export "BGL_INPUT_CHAR")
      (param $port (ref $binary-port))
      (result i32)
      
      (local $nbread i32)
      (local $fd i32)
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (i32.const 1) (i32.const -1)))
      
      (if (i32.eq (local.get $nbread) (i32.const 1))
	  (then (return (i32.load8_u (i32.const 128))))
	  (else (return (i32.const -1)))))

   ;; BGL_INT_EOFP
   (func $BGL_INT_EOFP
      (export "BGL_INT_EOFP")
      (param $c i32)
      (result i32)
      (i32.eq (local.get $c) (i32.const -1)))

   ;; bgl_output_string
   (func $bgl_output_string
      (export "bgl_output_string")
      (param $port (ref $binary-port))
      (param $s (ref $bstring))
      (result i32)
      
      (local $fd i32)
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      
      (call $memcpy
	 (i32.const 128) (local.get $s) (i32.const 0) (array.len (local.get $s)))
      (return_call $js_write_file (local.get $fd) (i32.const 128) (array.len (local.get $s))))

   ;; bgl_input_string
   (func $bgl_input_string
      (export "bgl_input_string")
      (param $port (ref $binary-port))
      (param $len i32)
      (result (ref $bstring))
      
      (local $fd i32)
      (local $nbread i32)
      (local $buf (ref $bstring))
      
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      (local.set $buf (array.new_default $bstring (local.get $len)))
      
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (local.get $len) (i32.const -1)))
      
      (local.set $buf (array.new_default $bstring (local.get $nbread)))
      (call $load_string_in_buffer (i32.const 128) (local.get $nbread) (local.get $buf) (i32.const 0))
      (return (local.get $buf)))

   ;; bgl_input_fill_string
   (func $bgl_input_fill_string
      (export "bgl_input_fill_string")
      (param $port (ref $binary-port))
      (param $s (ref $bstring))
      (result i32)

      (local $fd i32)
      (local $nbread i32)
      
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (array.len (local.get $s)) (i32.const -1)))
      (call $load_string_in_buffer (i32.const 128) (local.get $nbread) (local.get $s) (i32.const 0))
      
      (return (local.get $nbread)))
      
   ;; output_obj
   (func $output_obj
      (export "output_obj")
      (param $port (ref $binary-port))
      (param $obj (ref eq))
      (result (ref eq))
      
      (local $fd i32)
      (local $string (ref $bstring))
      (local $len i32)
      (local $magic (ref $bstring))
      
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      (local.set $string (call $obj_to_string (local.get $obj) (global.get $BFALSE)))
      (local.set $len (array.len (local.get $string)))
      (local.set $magic (array.new_data $bstring $MAGIC_WORD (i32.const 0) (i32.const 4)))

      ;; MAGIC_WORD
      (call $memcpy
	 (i32.const 128) (local.get $magic) (i32.const 0)
	 (array.len (local.get $magic)))
      (drop (call $js_write_file (local.get $fd) (i32.const 128) (i32.const 4)))

      ;; obj size
      (i32.store8 (i32.const 128) (i32.and (local.get $len) (i32.const 255)))
      (i32.store8 (i32.const 129) (i32.and (i32.shr_u (local.get $len) (i32.const 8)) (i32.const 255)))
      (i32.store8 (i32.const 130) (i32.and (i32.shr_u (local.get $len) (i32.const 16)) (i32.const 255)))
      (i32.store8 (i32.const 131) (i32.and (i32.shr_u (local.get $len) (i32.const 24)) (i32.const 255)))
      (drop (call $js_write_file (local.get $fd) (i32.const 128) (i32.const 4)))

      ;; obj
      (call $memcpy
	 (i32.const 128) (local.get $string) (i32.const 0)
	 (array.len (local.get $string)))
      (drop
	 (call $js_write_file (local.get $fd) (i32.const 128)
	    (array.len (local.get $string))))
      
      (return (local.get $obj)))

   ;; input_obj
   (func $input_obj
      (export "input_obj")
      (param $port (ref $binary-port))
      (result (ref eq))
      
      (local $nbread i32)
      (local $fd i32)
      (local $magic (ref $bstring))
      (local $size i32)
      (local $buf (ref $bstring))
      (local $err i32)
      
      (local.set $fd (struct.get $binary-port $fd (local.get $port)))
      (local.set $magic (array.new_data $bstring $MAGIC_WORD (i32.const 0) (i32.const 4)))
      
      ;; MAGIC_WORD
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (i32.const 4) (i32.const -1)))
      
      (if (i32.eqz (i32.eq (local.get $nbread) (i32.const 4)))
	  (then (return (global.get $BEOF))))
      
      (if (i32.eqz (i32.eq (i32.load8_u (i32.const 128)) (array.get $bstring (local.get $magic) (i32.const 0))))
	  (then (local.set $err (i32.const 1))))
      (if (i32.eqz (i32.eq (i32.load8_u (i32.const 129)) (array.get $bstring (local.get $magic) (i32.const 1))))
	  (then (local.set $err (i32.const 1))))
      (if (i32.eqz (i32.eq (i32.load8_u (i32.const 130)) (array.get $bstring (local.get $magic) (i32.const 2))))
	  (then (local.set $err (i32.const 1))))
      (if (i32.eqz (i32.eq (i32.load8_u (i32.const 131)) (array.get $bstring (local.get $magic) (i32.const 3))))
	  (then (local.set $err (i32.const 1))))

      (if (i32.gt_s (local.get $err) (i32.const 0))
	  (then
	     (drop (call $the_failure
		      (array.new_data $bstring $INPUT_OBJ (i32.const 0) (i32.const 9))
		      (array.new_data $bstring $BAD_MAGIC (i32.const 0) (i32.const 18))
		      (global.get $BUNSPEC)))
	     (unreachable)))
      
      ;; size
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (i32.const 4) (i32.const -1)))
      
      (local.set $size
	 (i32.add (i32.load8_u (i32.const 128))
	    (i32.add (i32.shl (i32.load8_u (i32.const 129)) (i32.const 8))
	       (i32.add (i32.shl (i32.load8_u (i32.const 130)) (i32.const 16))
		  (i32.shl (i32.load8_u (i32.const 131)) (i32.const 24))))))
      
      ;; payload
      (local.set $nbread
	 (call $js_read_file (local.get $fd) (i32.const 128) (local.get $size) (i32.const -1)))
      
      (if (i32.eqz (i32.eq (local.get $nbread) (local.get $size)))
	  (then (throw $fail)))

      (local.set $buf (array.new_default $bstring (local.get $nbread)))
      (call $load_string_in_buffer (i32.const 128) (local.get $nbread) (local.get $buf) (i32.const 0))

      ;; unmarshall and return
      (return_call $string_to_obj (local.get $buf) (global.get $BFALSE) (global.get $BFALSE)))

   )
   

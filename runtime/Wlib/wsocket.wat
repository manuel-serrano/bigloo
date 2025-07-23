;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsocket.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Wed Jul 23 11:00:15 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM sockets                                                     */
;*=====================================================================*/

(module $__bigloo_socket
   
   ;; -----------------------------------------------------------------
   ;; Data 
   ;; -----------------------------------------------------------------

   (data $socket_input "socket-input")
   (data $socket_output "socket-output")
   (data $socket-servers-no-port "socket servers have no port")

   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $socket
      (sub
	 (struct
	    (field $sock (mut externref))
	    (field $portnum i32)
	    (field $hostname (mut (ref eq)))
	    (field $hostip (mut (ref eq)))
	    (field $family i32)
	    (field $fd i32)
	    (field $input (mut (ref eq)))
	    (field $output (mut (ref eq)))
	    (field $user-data (mut (ref eq))))))

   (type $socket-server
      (sub $socket
	 (struct 
	    (field $sock (mut externref))
	    (field $portnum i32)
	    (field $hostname (mut (ref eq)))
	    (field $hostip (mut (ref eq)))
	    (field $family i32)
	    (field $fd i32)
	    (field $input (mut (ref eq)))
	    (field $output (mut (ref eq)))
	    (field $user-data (mut (ref eq))))))
   
   (type $socket-client
      (sub $socket
	 (struct 
	    (field $sock (mut externref))
	    (field $portnum i32)
	    (field $hostname (mut (ref eq)))
	    (field $hostip (mut (ref eq)))
	    (field $family i32)
	    (field $fd i32)
	    (field $input (mut (ref eq)))
	    (field $output (mut (ref eq)))
	    (field $user-data (mut (ref eq))))))
   
   (type $datagram-socket
      (struct
	 (field $portnum i32)
	 (field $hostname (mut (ref eq)))
	 (field $hostip (mut (ref eq)))
	 (field $fd i32)
	 (field $port (mut (ref eq)))))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__js" "trace" (func $js_trace (param i32)))
   
   (import "__js" "unsupported" (func $js_unsupported (param i32)))
   (import "__js_socket" "nullsocket" (global $nullsocket externref))
   (import "__js_socket" "make_server" (func $js_make_server (param i32 i32 i32 i32 i32) (result externref)))
   (import "__js_socket" "accept" (func $js_accept (param externref) (result i32)))
   
   (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
   (import "__bigloo" "BFALSE" (global $BFALSE (ref eq)))
   (import "__bigloo" "BTRUE" (global $BTRUE (ref eq)))
   (import "__bigloo" "BNIL" (global $BNIL (ref eq)))
   (import "__bigloo" "BEOF" (global $BEOF (ref eq)))
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
   (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
   (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
   (import "__bigloo" "STRING_LENGTH" (func $STRING_LENGTH (param (ref $bstring)) (result i64)))
   (import "__bigloo" "bgl_load_string_in_buffer" (func $load_string_in_buffer (param i32) (param i32) (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "BINT" (func $BINT (param i64) (result (ref eq))))
   (import "__bigloo" "the_failure" (func $the_failure (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "__bigloo" "BGL_INPUT_PORT_DEFAULT_VALUE" (global $input-port-default-value (ref $input-port)))
   (import "__bigloo" "BGL_OUTPUT_PORT_DEFAULT_VALUE" (global $output-port-default-value (ref $output-port)))

   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $socket-default-value
      (export "BGL_SOCKET_DEFAULT_VALUE") (ref $socket)
      (struct.new $socket
	 ;; socket
	 (global.get $nullsocket)
	 ;; portnum
	 (i32.const -1)
	 ;; hostname
	 (global.get $BUNSPEC)
	 ;; hostip
	 (global.get $BUNSPEC)
	 ;; family
	 (i32.const -1)
	 ;; fd
	 (i32.const -1)
	 ;; input
	 (global.get $BUNSPEC)
	 ;; output
	 (global.get $BUNSPEC)
	 ;; use-data
	 (global.get $BUNSPEC)))
	 
   (global $datagram-socket-default-value
      (export "BGL_DATAGRAM_SOCKET_DEFAULT_VALUE") (ref $datagram-socket)
      (struct.new $datagram-socket
	 ;; portnum
	 (i32.const -1)
	 ;; hostname
	 (global.get $BUNSPEC)
	 ;; hostip
	 (global.get $BUNSPEC)
	 ;; fd
	 (i32.const -1)
	 ;; port
	 (global.get $BUNSPEC)))
   
   ;; --------------------------------------------------------
   ;; sockets
   ;; --------------------------------------------------------
   
   (func $SOCKET_INPUT (export "SOCKET_INPUT")
      (param $socket (ref $socket))
      (result (ref $input-port))
      (local $tmp (ref eq))
      (local.set $tmp (struct.get $socket $input (local.get $socket)))
      (if (ref.test (ref $input-port) (local.get $tmp))
	  (then
	     (return (ref.cast (ref $input-port) (local.get $tmp))))
	  (else
	   (call $the_failure
		(array.new_data $bstring $socket_input
		   (i32.const 0)
		   (i32.const 12))
		(array.new_data $bstring $socket-servers-no-port
		   (i32.const 0)
		   (i32.const 27))
		(local.get $socket))
	   (unreachable)))
      (unreachable))

   (func $SOCKET_OUTPUT (export "SOCKET_OUTPUT")
      (param $socket (ref $socket))
      (result (ref $output-port))
      (local $tmp (ref eq))
      (local.set $tmp (struct.get $socket $output (local.get $socket)))
      (if (ref.test (ref $output-port) (local.get $tmp))
	  (then
	     (return (ref.cast (ref $output-port) (local.get $tmp))))
	  (else
	   (call $the_failure
		(array.new_data $bstring $socket_output
		   (i32.const 0)
		   (i32.const 13))
		(array.new_data $bstring $socket-servers-no-port
		   (i32.const 0)
		   (i32.const 27))
		(local.get $socket))
	   (unreachable)))
      (unreachable))

   (func $SOCKET_PORT (export "SOCKET_PORT")
      (param $sock (ref $socket))
      (result i32)
      (return (struct.get $socket $portnum (local.get $sock))))
   
   (func $SOCKET_HOSTNAME (export "SOCKET_HOSTNAME")
      (param $sock (ref $socket))
      (result (ref eq))
      (return (struct.get $socket $hostname (local.get $sock))))
   
   (func $BGL_SOCKET_SERVERP (export "BGL_SOCKET_SERVERP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $socket-server) (local.get $o)))

   (func $BGL_SOCKET_CLIENTP (export "BGL_SOCKET_CLIENTP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $socket-client) (local.get $o)))

   (func $bgl_make_server_socket (export "bgl_make_server_socket")
      (param $hostname (ref eq))
      (param $portnum i32)
      (param $backlog i32)
      (param $family (ref eq))
      (result (ref $socket))
      
      ;;(call $store_string (local.get $path) (i32.const 128))
      
      (struct.new $socket-server
	 ;; sock
	 (call $js_make_server
	    (i32.const 0) (i32.const 0)
	    (local.get $portnum)
	    (local.get $backlog)
	    (i32.const 0))
	 ;; portnum
	 (local.get $portnum)
	 ;; hostname
	 (global.get $BUNSPEC)
	 ;; hostip
	 (global.get $BFALSE)
	 ;; family
	 (i32.const 01)
	 ;; fd
	 (i32.const -1)
	 ;; input
	 (global.get $BFALSE)
	 ;; output
	 (global.get $BFALSE)
	 ;; user-data
	 (global.get $BUNSPEC)))

   (func $bgl_socket_accept (export "bgl_socket_accept")
      (param $serv (ref $socket))
      (param $errp i32)
      (param $inb (ref eq))
      (param $outb (ref eq))
      (result (ref eq))

      (call $js_trace
	 (call $js_accept (struct.get $socket $sock (local.get $serv))))
      (return (global.get $BUNSPEC)))
   )

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wsocket.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 30 10:49:20 2024                          */
;*    Last change :  Tue Jul 22 13:24:38 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM threads                                                     */
;*=====================================================================*/

(module $__bigloo_thread
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $socket (struct))
   (type $datagram-socket (struct))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__js" "unsupported" (func $js_unsupported (param i32)))
   
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
      (struct.new $socket))
   (global $datagram-socket-default-value
      (export "BGL_DATAGRAM_SOCKET_DEFAULT_VALUE") (ref $datagram-socket)
      (struct.new $datagram-socket))
   
   ;; --------------------------------------------------------
   ;; Library functions
   ;; --------------------------------------------------------
   (func $SOCKET_INPUT (export "SOCKET_INPUT")
      (param $socket (ref $socket))
      (result (ref $input-port))
      (global.get $input-port-default-value))
   
   (func $SOCKET_OUTPUT (export "SOCKET_OUTPUT")
      (param $socket (ref $socket))
      (result (ref $output-port))
      (global.get $output-port-default-value))
   
   )

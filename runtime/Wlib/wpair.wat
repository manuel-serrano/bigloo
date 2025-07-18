;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wpair.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep 28 06:41:16 2024                          */
;*    Last change :  Fri Jul 18 15:57:43 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM pairs                                                       */
;*=====================================================================*/

(module $__bigloo_pair
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   (type $pair (sub (struct
		       (field $car (mut (ref eq)))
		       (field $cdr (mut (ref eq))))))
   (type $epair (sub $pair (struct 
			      (field $car (mut (ref eq))) 
			      (field $cdr (mut (ref eq))) 
			      (field $cer (mut (ref eq))))))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
   (import "__bigloo" "BNIL" (global $BNIL (ref $bnil)))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref $bunspecified)))

   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------

   (global $pair-default-value
      (export "BGL_PAIR_DEFAULT_VALUE") (ref $pair)
      (struct.new $pair (global.get $BUNSPEC) (global.get $BNIL)))
   (global $epair-default-value
      (export "BGL_EPAIR_DEFAULT_VALUE") (ref $epair)
      (struct.new $epair (global.get $BUNSPEC) (global.get $BNIL) (global.get $BUNSPEC)))

   ;; -----------------------------------------------------------------
   ;; Macros
   ;; -----------------------------------------------------------------
   
   (func $PAIRP (export "PAIRP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $pair) (local.get $o)))

   (func $EPAIRP (export "EPAIRP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $epair) (local.get $o)))

   (func $NULLP (export "NULLP")
      (param $o (ref eq))
      (result i32)
      (ref.eq (global.get $BNIL) (local.get $o)))

   (func $MAKE_YOUNG_PAIR (export "MAKE_YOUNG_PAIR")
      (param $a (ref eq))
      (param $d (ref eq))
      (result (ref $pair))
      (struct.new $pair (local.get $a) (local.get $d)))

   (func $MAKE_STACK_PAIR (export "MAKE_STACK_PAIR")
      (param $a (ref eq))
      (param $d (ref eq))
      (result (ref $pair))
      (struct.new $pair (local.get $a) (local.get $d)))

   (func $MAKE_YOUNG_EPAIR (export "MAKE_YOUNG_EPAIR")
      (param $a (ref eq))
      (param $d (ref eq))
      (param $e (ref eq))
      (result (ref $epair))
      (struct.new $epair (local.get $a) (local.get $d) (local.get $e)))

   )
   

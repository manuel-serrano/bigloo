;; -*- eval: (bee-mode) -*-
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/runtime.wat        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 13 10:34:00 2024                          */
;*    Last change :  Tue Dec 17 09:56:10 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Bigloo WASM builtin runtime                                      */
;*=====================================================================*/

(module $__bigloo

  ;; General bigloo memory
  (memory 1)
  (export "memory" (memory 0))

  (func $bgl_internal_error (export "bgl_internal_error")
     (param $errno i32)
     (param $val i32)
     (result (ref eq))
     (call $js_internal_error (local.get $errno) (local.get $val))
     (return (global.get $BUNSPEC)))

  (func $CNST_TABLE_SET (export "CNST_TABLE_SET")
     (param $offset i32)
     (param $val (ref eq))
     (result (ref eq))
     (global.get $BUNSPEC))

  (func $CNST_TABLE_REF (export "CNST_TABLE_REF")
     (param $offset i32)
     (result (ref eq))
     (global.get $BUNSPEC))
  
  ;; Bigloo types default value
  (global $funptr-default-value
     (export "BGL_FUNPTR_DEFAULT_VALUE") (ref func)
     (ref.func $BOOLEANP))

  (global $real-default-value
     (export "BGL_REAL_DEFAULT_VALUE") (ref $real)
     (struct.new $real (f64.const 0)))
  
  (global $bucs2-default-value
     (export "BGL_BUCS2_DEFAULT_VALUE") (ref $bucs2)
     (struct.new $bucs2 (i32.const 0) (i32.const 0)))
  
  (global $pair-default-value
     (export "BGL_PAIR_DEFAULT_VALUE") (ref $pair)
     (struct.new $pair (global.get $BUNSPEC) (global.get $BNIL)))
  (global $epair-default-value
     (export "BGL_EPAIR_DEFAULT_VALUE") (ref $epair)
     (struct.new $epair (global.get $BUNSPEC) (global.get $BNIL) (global.get $BUNSPEC)))
  
  (global $cell-default-value
     (export "BGL_CELL_DEFAULT_VALUE") (ref $cell)
     (struct.new $cell (global.get $BUNSPEC)))
  
  (global $ucs2string-default-value
     (export "BGL_UCS2STRING_DEFAULT_VALUE") (ref $ucs2string)
     (array.new_fixed $ucs2string 0))
  
  (global $regexp-default-value
     (export "BGL_REGEXP_DEFAULT_VALUE") (ref $regexp)
     (struct.new $regexp))
  
  (global $vector-default-value
     (export "BGL_VECTOR_DEFAULT_VALUE") (ref $vector)
     (array.new_fixed $vector 0))
  (global $u8vector-default-value
     (export "BGL_U8VECTOR_DEFAULT_VALUE") (ref $u8vector)
     (array.new_fixed $u8vector 0))
  (global $s8vector-default-value
     (export "BGL_S8VECTOR_DEFAULT_VALUE") (ref $s8vector)
     (array.new_fixed $s8vector 0))
  (global $u16vector-default-value
     (export "BGL_U16VECTOR_DEFAULT_VALUE") (ref $u16vector)
     (array.new_fixed $u16vector 0))
  (global $s16vector-default-value
     (export "BGL_S16VECTOR_DEFAULT_VALUE") (ref $s16vector)
     (array.new_fixed $s16vector 0))
  (global $u32vector-default-value
     (export "BGL_U32VECTOR_DEFAULT_VALUE") (ref $u32vector)
     (array.new_fixed $u32vector 0))
  (global $s32vector-default-value
     (export "BGL_S32VECTOR_DEFAULT_VALUE") (ref $s32vector)
     (array.new_fixed $s32vector 0))
  (global $u64vector-default-value
     (export "BGL_U64VECTOR_DEFAULT_VALUE") (ref $u64vector)
     (array.new_fixed $u64vector 0))
  (global $s64vector-default-value
     (export "BGL_S64VECTOR_DEFAULT_VALUE") (ref $s64vector)
     (array.new_fixed $s64vector 0))
  (global $f32vector-default-value
     (export "BGL_F32VECTOR_DEFAULT_VALUE") (ref $f32vector)
     (array.new_fixed $f32vector 0))
  (global $f64vector-default-value
     (export "BGL_F64VECTOR_DEFAULT_VALUE") (ref $f64vector)
     (array.new_fixed $f64vector 0))

  (global $struct-default-value
     (export "BGL_STRUCT_DEFAULT_VALUE") (ref $struct)
     (struct.new $struct (global.get $BUNSPEC) (global.get $vector-default-value)))

  (global $mutex-default-value
     (export "BGL_MUTEX_DEFAULT_VALUE") (ref $mutex)
     (struct.new $mutex
	;; name
	(global.get $BUNSPEC)
	;; backend
	(global.get $BUNSPEC)
	;; state
	(global.get $BUNSPEC)))
  (global $condvar-default-value
     (export "BGL_CONDVAR_DEFAULT_VALUE") (ref $condvar)
     (struct.new $condvar))
  
  (global $procedure-default-value
     (export "BGL_PROCEDURE_DEFAULT_VALUE") (ref $procedure)
     (struct.new $procedure
	;; entry
	(ref.func $BOOLEANP)
	;; attr
	(global.get $BUNSPEC)
	;; arity
	(i32.const 0)
	;; env
	(global.get $vector-default-value)))
  (global $procedure-l-default-value
     (export "BGL_PROCEDURE_L_DEFAULT_VALUE") (ref $procedure-l)
     (struct.new $procedure-l
	;; entry
	(ref.func $BOOLEANP)
	;; env
	(global.get $vector-default-value)))
  (global $procedure-el-default-value
     (export "BGL_PROCEDURE_EL_DEFAULT_VALUE") (ref $procedure-el)
     (array.new_fixed $procedure-el 0))
  
  (global $binary-port-default-value
     (export "BGL_BINARY_PORT_DEFAULT_VALUE") (ref $binary-port)
     (struct.new $binary-port))

  (global $socket-default-value
     (export "BGL_SOCKET_DEFAULT_VALUE") (ref $socket)
     (struct.new $socket))
  (global $datagram-socket-default-value
     (export "BGL_DATAGRAM_SOCKET_DEFAULT_VALUE") (ref $datagram-socket)
     (struct.new $datagram-socket))
  
  (global $mmap-default-value
     (export "BGL_MMAP_DEFAULT_VALUE") (ref $mmap)
     (struct.new $mmap))
  
  (global $process-default-value
     (export "BGL_PROCESS_DEFAULT_VALUE") (ref $process)
     (struct.new $process))
  
  (global $exit-default-value
     (export "BGL_EXIT_DEFAULT_VALUE") (ref $exit)
     (struct.new $exit
	;; userp
	(i64.const 0)
	;; stamp
	(i64.const 0)
	;; protect
	(global.get $BUNSPEC)
	;; prev
	(ref.null none)))

  (global $dynamic-env-default-value
     (export "BGL_DYNAMIC_ENV_DEFAULT_VALUE") (ref $dynamic-env)
     (struct.new $dynamic-env
	;; exitd_top
	(global.get $exit-default-value)
	;; exitd_val
	(global.get $BUNSPEC)
	;; uncaught-exception-handler
	(global.get $BUNSPEC)
	;; error-handler
	(global.get $BUNSPEC)
	;; current-output-port
	(ref.null none)
	;; current-error-port
	(ref.null none)
	;; current-input-port
	(ref.null none)
	;; module
	(global.get $BUNSPEC)
	;; abase
	(global.get $BUNSPEC)
	))

  (global $object-default-value
     (export "BGL_OBJECT_DEFAULT_VALUE") (ref $BgL_objectz00_bglt)
     (struct.new $BgL_objectz00_bglt
	(i64.const 0)
	(global.get $BUNSPEC)))

  (global $opaque-default-value
     (export "BGL_OPAQUE_DEFAULT_VALUE") (ref $opaque)
     (struct.new $opaque))

    

   ;; -----------------------------------------------------------------
   ;; Utilities
   ;; -----------------------------------------------------------------

   ;; memcpy
   (func $memcpy
     (param $dest i32)
     (param $src (ref $bstring))
     (param $i i32)
     (param $len i32)
     (loop $loop
	(if (i32.lt_u (local.get $i) (local.get $len))
	    (then
	       (i32.store8 (i32.add (local.get $dest) (local.get $i))
		  (array.get $bstring (local.get $src) (local.get $i)))
	       (local.set $i (i32.add (local.get $i) (i32.const 1)))
	       (br $loop)))))
  
  ;; --------------------------------------------------------
  ;; Struct functions
  ;; --------------------------------------------------------

  (func $STRUCT_SET (export "STRUCT_SET")
    (param $struct (ref $struct))
    (param $index i32)
    (param $value (ref eq))
    (result (ref eq))
    (array.set $vector 
      (struct.get $struct $values (local.get $struct)) 
      (local.get $index) 
      (local.get $value))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Multiple values functions
  ;; --------------------------------------------------------
  (global $mvalues_count (export "mvalues_count")
     (mut i32) (i32.const 0))

  (global $mvalues (export "mvalues")
     (mut (ref $vector))
     (array.new $vector (global.get $BUNSPEC) (i32.const 16)))

  (func $BGL_MVALUES_VAL (export "BGL_MVALUES_VAL")
     (param $i i32)
     (result (ref eq))
     (array.get $vector (global.get $mvalues) (local.get $i)))

  (func $BGL_MVALUES_VAL_SET (export "BGL_MVALUES_VAL_SET")
     (param $i i32)
     (param $val (ref eq))
     (result (ref eq))
     (array.set $vector (global.get $mvalues) (local.get $i) (local.get $val))
     (local.get $val))

  (func $BGL_MVALUES_NUMBER (export "BGL_MVALUES_NUMBER")
     (result i32)
     (global.get $mvalues_count))

  (func $BGL_MVALUES_NUMBER_SET (export "BGL_MVALUES_NUMBER_SET")
     (param $n i32)
     (result i32)
     (global.set $mvalues_count (local.get $n))
     ;; Reallocate mvalues if not large enough.
     (if (i32.gt_u (local.get $n) (array.len (global.get $mvalues)))
	 (then
	    (if (i32.gt_s (local.get $n) (i32.const 0))
		(then
		   (global.set $mvalues
		      (array.new $vector (global.get $BUNSPEC) (local.get $n)))))))
     (local.get $n))

  ;; --------------------------------------------------------
  ;; Cell functions
  ;; --------------------------------------------------------

  (func $CELL_SET (export "CELL_SET") (param $c (ref $cell)) (param $v (ref eq)) (result (ref eq))
    (struct.set $cell $val (local.get $c) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Pair functions
  ;; --------------------------------------------------------

  (func $SET_CAR (export "SET_CAR") (param $p (ref $pair)) (param $v (ref eq)) (result (ref eq))
    (struct.set $pair $car (local.get $p) (local.get $v))
    (global.get $BUNSPEC))
  
  (func $SET_CDR (export "SET_CDR") (param $p (ref $pair)) (param $v (ref eq)) (result (ref eq))
    (struct.set $pair $cdr (local.get $p) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Procedure functions
  ;; --------------------------------------------------------

  (func $MAKE_FX_PROCEDURE (export "MAKE_FX_PROCEDURE")
    (param $entry (ref func))
    (param $arity i32)
    (param $size i32)
    (result (ref $procedure))
    (struct.new $procedure
      (local.get $entry)
      (global.get $BUNSPEC)
      (local.get $arity)
      (array.new $vector (global.get $BUNSPEC) (local.get $size))))

  (func $MAKE_L_PROCEDURE (export "MAKE_L_PROCEDURE")
    (param $entry (ref func))
    (param $size i32)
    (result (ref $procedure-l))
    (struct.new $procedure-l
      (local.get $entry)
      (array.new $vector (global.get $BUNSPEC) (local.get $size))))

  (func $MAKE_EL_PROCEDURE (export "MAKE_EL_PROCEDURE")
     (param $size i32)
     (result (ref $procedure-el))
     (array.new $procedure-el (global.get $BUNSPEC) (local.get $size)))

  (global $procedure-el-empty (export "BGL_PROCEDURE_EL_EMPTY")
     (ref $procedure-el)
     (array.new_fixed $procedure-el 0))
  
  (func $PROCEDURE_CORRECT_ARITYP (export "PROCEDURE_CORRECT_ARITYP")
    (param $p (ref $procedure)) 
    (param $i i32) 
    (result i32)
    (local $arity i32)
    (local.set $arity (struct.get $procedure $arity (local.get $p)))
    ;; (arity == i) || ((arity < 0) && (-i - 1 <= arity))
    (i32.or 
      (i32.eq (local.get $arity) (local.get $i))
      (i32.and 
        (i32.lt_s (local.get $arity) (i32.const 0))
        (i32.le_s (i32.sub (i32.const -1) (local.get $i)) (local.get $arity)))))

  (func $PROCEDURE_SET (export "PROCEDURE_SET") 
    (param $p (ref $procedure)) 
    (param $i i32) 
    (param $v (ref eq)) 
    (result (ref eq))
    (array.set $vector (struct.get $procedure $env (local.get $p)) (local.get $i) (local.get $v))
    (global.get $BUNSPEC))

  (func $PROCEDURE_L_SET (export "PROCEDURE_L_SET") 
    (param $p (ref $procedure-l)) 
    (param $i i32)
    (param $v (ref eq)) 
    (result (ref eq))
    (array.set $vector (struct.get $procedure-l $env (local.get $p)) (local.get $i) (local.get $v))
    (global.get $BUNSPEC))

  (func $PROCEDURE_L_REF (export "PROCEDURE_L_REF") 
    (param $p (ref $procedure-l)) 
    (param $i i32) 
    (param $v (ref eq)) 
    (result (ref eq))
    (array.get $vector (struct.get $procedure-l $env (local.get $p)) (local.get $i)))

  (func $PROCEDURE_EL_SET (export "PROCEDURE_EL_SET") 
    (param $p (ref $procedure-el)) 
    (param $i i32) 
    (param $v (ref eq)) 
    (result (ref eq))
    (array.set $procedure-el (local.get $p) (local.get $i) (local.get $v))
    (global.get $BUNSPEC))

  (func $PROCEDURE_EL_REF (export "PROCEDURE_EL_REF") 
    (param $p (ref $procedure-el)) 
    (param $i i32) 
    (result (ref eq))
    (array.get $procedure-el (local.get $p) (local.get $i)))

  (func $PROCEDURE_ATTR_SET (export "PROCEDURE_ATTR_SET") 
    (param $p (ref $procedure)) 
    (param $v (ref eq)) 
    (result (ref eq))
    (struct.set $procedure $attr (local.get $p) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Vector functions
  ;; --------------------------------------------------------

  (func $bgl_fill_vector (export "bgl_fill_vector")
    (param $v (ref $vector))
    (param $start i64)
    (param $end i64)
    (param $o (ref eq))
    (result (ref eq))
    (array.fill $vector 
      (ref.cast (ref $vector) (local.get $v)) ;; FIXME: remove the cast
      (i32.wrap_i64 (local.get $start)) 
      (local.get $o)
      (i32.wrap_i64 (i64.sub (local.get $end) (local.get $start))))
    (global.get $BUNSPEC))
  
  ;; --------------------------------------------------------
  ;; Typed vector functions
  ;; --------------------------------------------------------

  ;; TODO: better implementation of tvector descr

  (global $tvector_descr_i8 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_i16 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_i32 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_i64 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_f32 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_f64 (mut (ref eq)) (global.get $BUNSPEC))
  (global $tvector_descr_eqref (mut (ref eq)) (global.get $BUNSPEC))

  (func $TVECTOR_DESCR (export "TVECTOR_DESCR")
    (param $v arrayref)
    (result (ref eq))
    (if (ref.test (ref $u32vector) (local.get $v))
      (then (return (global.get $tvector_descr_i32))))
    (if (ref.test (ref $u64vector) (local.get $v))
      (then (return (global.get $tvector_descr_i64))))
    (if (ref.test (ref $f32vector) (local.get $v))
      (then (return (global.get $tvector_descr_f32))))
    (if (ref.test (ref $f64vector) (local.get $v))
      (then (return (global.get $tvector_descr_f64))))
    (if (ref.test (ref $vector) (local.get $v))
      (then (return (global.get $tvector_descr_eqref))))
    (global.get $BUNSPEC))

  (func $TVECTOR_DESCR_SET (export "TVECTOR_DESCR_SET")
    (param $v arrayref)
    (param $desc (ref eq))
    (result (ref eq))
    (if (ref.test (ref $u32vector) (local.get $v))
      (then (global.set $tvector_descr_i32 (local.get $desc)) (return (global.get $BUNSPEC))))
    (if (ref.test (ref $u64vector) (local.get $v))
      (then (global.set $tvector_descr_i64 (local.get $desc)) (return (global.get $BUNSPEC))))
    (if (ref.test (ref $f32vector) (local.get $v))
      (then (global.set $tvector_descr_f32 (local.get $desc)) (return (global.get $BUNSPEC))))
    (if (ref.test (ref $f64vector) (local.get $v))
      (then (global.set $tvector_descr_f64 (local.get $desc)) (return (global.get $BUNSPEC))))
    (if (ref.test (ref $vector) (local.get $v))
      (then (global.set $tvector_descr_eqref (local.get $desc)) (return (global.get $BUNSPEC))))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Flonum builtin functions
  ;; --------------------------------------------------------

  (func $BGL_SIGNBIT (export "BGL_SIGNBIT") (param $v f64) (result i64)
    (i64.shr_u (i64.reinterpret_f64 (local.get $v)) (i64.const 63)))

  (func $BGL_IS_FINITE (export "BGL_IS_FINITE") (param $v f64) (result i32)
    ;; This is the code generated by Clang for __builtin_isfinite().
    ;; See https://godbolt.org/z/WPoW3djYK
    (i64.lt_s
      (i64.and
        (i64.reinterpret_f64 (local.get $v))
        (i64.const 0x7FFFFFFFFFFFFFFF #;NaN))
      (i64.const 0x7FF0000000000000 #;Inf)))

  (func $BGL_IS_INF (export "BGL_IS_INF") (param $v f64) (result i32)
    ;; The abs is required to take care of -INF and +INF.
    (f64.eq (f64.abs (local.get $v)) (f64.const inf)))

  (func $BGL_IS_NAN (export "BGL_IS_NAN") (param $v f64) (result i32)
    ;; NaN are the only floating point values that are never
    ;; equal to themself (this is the trick used by Clang).
    (f64.ne (local.get $v) (local.get $v)))

  ;; --------------------------------------------------------
  ;; Exit builtin functions
  ;; --------------------------------------------------------

  (func $bgl_make_exit (export "bgl_make_exit")
     (result (ref $exit))
     (struct.new $exit
	(i64.const 0)
	(i64.const 0)
	(global.get $BNIL)
	(ref.null none)))
  
  (func $PUSH_ENV_EXIT (export "PUSH_ENV_EXIT") 
    (param $env (ref $dynamic-env)) 
    (param $v (ref $exit)) 
    (param $protect i64) 
    (result (ref eq))
    (struct.set $exit $userp (local.get $v) (local.get $protect))
    (struct.set $exit $prev (local.get $v) (struct.get $dynamic-env $exitd_top (local.get $env)))
    (struct.set $dynamic-env $exitd_top (local.get $env) (local.get $v))
    (global.get $BUNSPEC))

  (func $PUSH_EXIT (export "PUSH_EXIT") 
    (param $v (ref $exit)) 
    (param $protect i64) 
    (result (ref eq))
    (call $PUSH_ENV_EXIT 
        (global.get $current-dynamic-env)
        (local.get $v) 
        (local.get $protect)))

  (func $POP_ENV_EXIT (export "POP_ENV_EXIT")
    (param $env (ref $dynamic-env)) 
    (result (ref eq))
    (struct.set $dynamic-env $exitd_top
        (local.get $env)
	(ref.cast (ref $exit)
	   (struct.get $exit $prev
	      (struct.get $dynamic-env $exitd_top (local.get $env)))))
    (global.get $BUNSPEC))

  (func $POP_EXIT (export "POP_EXIT") (result (ref eq))
    (call $POP_ENV_EXIT (global.get $current-dynamic-env)))

  (func $EXITD_STAMP (export "EXITD_STAMP") (param $o (ref eq))
     (result (ref eq))
     (call $make_bint
	(struct.get $exit $stamp (ref.cast (ref $exit) (local.get $o)))))

  (func $EXITD_CALLCCP (export "EXITD_CALLCCP") (param $o (ref eq)) (result i32)
    (i32.const 0))

  (func $EXITD_TO_EXIT (export "EXITD_TO_EXIT") (param $o (ref eq)) (result (ref $exit))
    (ref.cast (ref $exit) (local.get $o)))

  (func $BGL_EXITD_PROTECT (export "BGL_EXITD_PROTECT") 
    (param (ref $exit)) 
    (result (ref eq))
    (struct.get $exit $protect (local.get 0)))

  (func $BGL_EXITD_PROTECT_SET (export "BGL_EXITD_PROTECT_SET") 
    (param $e (ref $exit)) 
    (param $p (ref eq)) 
    (struct.set $exit $protect (local.get $e) (local.get $p)))

  (func $BGL_EXITD_PUSH_PROTECT (export "BGL_EXITD_PUSH_PROTECT") 
    (param $e (ref $exit)) 
    (param $p (ref eq))
    (call $BGL_EXITD_PROTECT_SET (local.get $e)
      (struct.new $pair 
        (local.get $p)
        (struct.get $exit $protect (local.get $e)))))
  
  (func $BGL_ERROR_HANDLER_GET (export "BGL_ERROR_HANDLER_GET")
    (result (ref eq))
    (struct.get $dynamic-env $error-handler (global.get $current-dynamic-env)))

  (func $BGL_ENV_ERROR_HANDLER_GET (export "BGL_ENV_ERROR_HANDLER_GET")
    (param $env (ref $dynamic-env))
    (result (ref eq))
    (struct.get $dynamic-env $error-handler (local.get $env)))

  (func $BGL_ERROR_HANDLER_SET (export "BGL_ERROR_HANDLER_SET") 
    (param $hdl (ref eq)) 
    (struct.set $dynamic-env $error-handler (global.get $current-dynamic-env) (local.get $hdl)))

  (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_GET (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET") (result (ref eq))
    (struct.get $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env)))

  (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_SET (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET") (param $hdl (ref eq))
    (struct.set $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env) (local.get $hdl)))

  (func $BGL_ENV_EXITD_TOP_AS_OBJ (export "BGL_ENV_EXITD_TOP_AS_OBJ") 
    (param $env (ref $dynamic-env)) 
    (result (ref eq))
    (ref.cast (ref $exit) (struct.get $dynamic-env $exitd_top (local.get $env))))

  (func $BGL_EXITD_TOP_AS_OBJ (export "BGL_EXITD_TOP_AS_OBJ") (result (ref eq))
    (call $BGL_ENV_EXITD_TOP_AS_OBJ (global.get $current-dynamic-env)))

  (func $BGL_EXITD_BOTTOMP (export "BGL_EXITD_BOTTOMP") (param $o (ref eq)) (result i32)
    (ref.is_null (struct.get $exit $prev (ref.cast (ref $exit) (local.get $o)))))

  (func $BGL_ENV_EXITD_VAL_SET (export "BGL_ENV_EXITD_VAL_SET") 
    (param $env (ref $dynamic-env)) 
    (param $v (ref eq)) 
    (result (ref eq))
    (struct.set $dynamic-env $exitd_val (local.get $env) (local.get $v))
    (global.get $BUNSPEC))

  (func $BGL_EXITD_VAL_SET (export "BGL_EXITD_VAL_SET") (param $v (ref eq)) (result (ref eq))
    (call $BGL_ENV_EXITD_VAL_SET (global.get $current-dynamic-env) (local.get $v)))

  ;; --------------------------------------------------------
  ;; funcall
  ;; --------------------------------------------------------
  (func $funcall0
     (param $proc (ref $procedure))
     (result (ref eq))
     (return_call_ref $func0
	(local.get $proc)
	(ref.cast (ref $func0)
	   (struct.get $procedure $entry (local.get $proc)))))
  
  (func $funcall1
     (param $proc (ref $procedure))
     (param $arg0 (ref eq))
     (result (ref eq))
     (return_call_ref $func1
	(local.get $proc)
	(local.get $arg0)
	(ref.cast (ref $func1)
	   (struct.get $procedure $entry (local.get $proc)))))


  (func $funcall2
     (param $proc (ref $procedure))
     (param $arg0 (ref eq))
     (param $arg1 (ref eq))
     (result (ref eq))
     (return_call_ref $func2
	(local.get $proc)
	(local.get $arg0)
	(local.get $arg1)
	(ref.cast (ref $func2)
	   (struct.get $procedure $entry (local.get $proc)))))

  (func $funcall3
     (param $proc (ref $procedure))
     (param $arg0 (ref eq))
     (param $arg1 (ref eq))
     (param $arg2 (ref eq))
     (result (ref eq))
     (return_call_ref $func3
	(local.get $proc)
	(local.get $arg0)
	(local.get $arg1)
	(local.get $arg2)
	(ref.cast (ref $func3)
	   (struct.get $procedure $entry (local.get $proc)))))

  (func $funcall4
     (param $proc (ref $procedure))
     (param $arg0 (ref eq))
     (param $arg1 (ref eq))
     (param $arg2 (ref eq))
     (param $arg3 (ref eq))
     (result (ref eq))
     (return_call_ref $func4
	(local.get $proc)
	(local.get $arg0)
	(local.get $arg1)
	(local.get $arg2)
	(local.get $arg3)
	(ref.cast (ref $func4)
	   (struct.get $procedure $entry (local.get $proc)))))
  
  ;; --------------------------------------------------------
  ;; Generic variadic call builtin functions
  ;; --------------------------------------------------------

  (func $make_list_params
     (param $params (ref $vector))
     (param $i i32)
     (result (ref eq))
     (local $len i32)
     (local $j i32)
     (local $list (ref eq))
     (local.set $list (global.get $BNIL))
     (local.set $len (array.len (local.get $params)))
     (local.set $j (i32.sub (local.get $len) (i32.const 1)))
     
     (block $break
	(loop $continue
	   (if (i32.lt_s (local.get $j) (local.get $i))
	       (then (br $break)))
	   (local.set $list
	      (struct.new $pair
		 (array.get $vector (local.get $params) (local.get $j))
		 (local.get $list)))
	   (local.set $j (i32.sub (local.get $j) (i32.const 1)))
	   (br $continue)))
     
     (local.get $list))

  ;; apply17
  (func $apply17
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local $arg12 (ref eq))
     (local $arg13 (ref eq))
     (local $arg14 (ref eq))
     (local $arg15 (ref eq))
     (local $arg16 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg12 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg13 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg14 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg15 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg16 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func17
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (local.get $arg12)
		  (local.get $arg13)
		  (local.get $arg14)
		  (local.get $arg15)
		  (local.get $arg16)
		  (ref.cast (ref $func17) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply16
  (func $apply16
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local $arg12 (ref eq))
     (local $arg13 (ref eq))
     (local $arg14 (ref eq))
     (local $arg15 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg12 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg13 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg14 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg15 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func16
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (local.get $arg12)
		  (local.get $arg13)
		  (local.get $arg14)
		  (local.get $arg15)
		  (ref.cast (ref $func16) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply15
  (func $apply15
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local $arg12 (ref eq))
     (local $arg13 (ref eq))
     (local $arg14 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg12 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg13 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg14 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func15
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (local.get $arg12)
		  (local.get $arg13)
		  (local.get $arg14)
		  (ref.cast (ref $func15) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply14
  (func $apply14
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local $arg12 (ref eq))
     (local $arg13 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg12 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg13 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func14
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (local.get $arg12)
		  (local.get $arg13)
		  (ref.cast (ref $func14) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply13
  (func $apply13
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local $arg12 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg12 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func13
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (local.get $arg12)
		  (ref.cast (ref $func13) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply12
  (func $apply12
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local $arg11 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg11 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func12
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (local.get $arg11)
		  (ref.cast (ref $func12) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply11
  (func $apply11
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local $arg10 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg10 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func11
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (local.get $arg10)
		  (ref.cast (ref $func11) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply10
  (func $apply10
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local $arg9 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg9 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func10
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (local.get $arg9)
		  (ref.cast (ref $func10) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply9
  (func $apply9
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local $arg8 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg8 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func9
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (local.get $arg8)
		  (ref.cast (ref $func9) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply8
  (func $apply8
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local $arg7 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg7 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func8
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (local.get $arg7)
		  (ref.cast (ref $func8) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply7
  (func $apply7
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local $arg6 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg6 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func7
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (local.get $arg6)
		  (ref.cast (ref $func7) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply6
  (func $apply6
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg5 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func6
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (local.get $arg5)
		  (ref.cast (ref $func6) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))

  ;; apply5
  (func $apply5
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg4 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func5
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (local.get $arg4)
		  (ref.cast (ref $func5) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply4
  (func $apply4
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg3 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func4
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (local.get $arg3)
		  (ref.cast (ref $func4) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply3
  (func $apply3
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg2 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func3
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (local.get $arg2)
		  (ref.cast (ref $func3) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply2
  (func $apply2
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func2
		  (local.get $proc)
		  (local.get $arg0)
		  (local.get $arg1)
		  (ref.cast (ref $func2) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply1
  (func $apply1
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func1
		  (local.get $proc)
		  (local.get $arg0)
		  (ref.cast (ref $func1) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))
  
  ;; apply0
  (func $apply0
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (if (ref.eq (local.get $args) (global.get $BNIL))
	 (then (return_call_ref $func0
		  (local.get $proc)
		  (ref.cast (ref $func0) (struct.get $procedure $entry (local.get $proc)))))
	 (else (throw $fail))))

  ;; apply-1
  (func $apply_1 
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (return_call_ref $func1
	(local.get $proc)
	(local.get $args)
	(ref.cast (ref $func1) (struct.get $procedure $entry (local.get $proc)))))
  
  ;; apply-2
  (func $apply_2
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (return_call_ref $func2
	(local.get $proc)
	(local.get $arg0)
	(local.get $args)
	(ref.cast (ref $func2) (struct.get $procedure $entry (local.get $proc)))))
  
  ;; apply-3
  (func $apply_3
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local.set $arg0 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (local.set $arg1 (struct.get $pair $car (ref.cast (ref $pair) (local.get $args))))
     (local.set $args (struct.get $pair $cdr (ref.cast (ref $pair) (local.get $args))))
     (return_call_ref $func3
	(local.get $proc)
	(local.get $arg0)
	(local.get $arg1)
	(local.get $args)
	(ref.cast (ref $func3) (struct.get $procedure $entry (local.get $proc)))))
  
  ;; apply
  (func $apply (export "apply")
     (param $proc (ref $procedure))
     (param $args (ref eq))
     (result (ref eq))
     (local $arity i32)
     (local $arg0 (ref eq))
     (local $arg1 (ref eq))
     (local $arg2 (ref eq))
     (local $arg3 (ref eq))
     (local $arg4 (ref eq))
     (local $arg5 (ref eq))
     (local.set $arity (struct.get $procedure $arity (local.get $proc)))
     (block $error
	(block $-3
	   (block $-2
	      (block $-1
		 (block $0
		    (block $1
		       (block $2
			  (block $3
			     (block $4
				(block $5
				   (block $6
				      (block $7
					 (block $8
					    (block $9
					       (block $10
						  (block $11
						     (block $12
							(block $13
							   (block $14
							      (block $15
								 (block $16
								    (block $17
								       (br_table $-3 $-2 $-1 $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $16 $17 $error
									  (i32.add (local.get $arity (i32.const 3)))))
								    (return_call $apply17 (local.get $proc) (local.get $args)))
								 (return_call $apply16 (local.get $proc) (local.get $args)))
							      (return_call $apply15 (local.get $proc) (local.get $args)))
							   (return_call $apply14 (local.get $proc) (local.get $args)))
							(return_call $apply13 (local.get $proc) (local.get $args)))
						     (return_call $apply12 (local.get $proc) (local.get $args)))
						  (return_call $apply11 (local.get $proc) (local.get $args)))
					       (return_call $apply10 (local.get $proc) (local.get $args)))
					    (return_call $apply9 (local.get $proc) (local.get $args)))
					 (return_call $apply8 (local.get $proc) (local.get $args)))
				      (return_call $apply7 (local.get $proc) (local.get $args)))
				   (return_call $apply6 (local.get $proc) (local.get $args)))
				(return_call $apply5 (local.get $proc) (local.get $args)))
			     (return_call $apply4 (local.get $proc) (local.get $args)))
			  (return_call $apply3 (local.get $proc) (local.get $args)))
		       (return_call $apply2 (local.get $proc) (local.get $args)))
		    (return_call $apply1 (local.get $proc) (local.get $args)))
		 (return_call $apply0 (local.get $proc) (local.get $args)))
	      (return_call $apply_1 (local.get $proc) (local.get $args)))
	   (return_call $apply_2 (local.get $proc) (local.get $args)))
	(return_call $apply_3 (local.get $proc) (local.get $args)))
     (call $js_internal_error (i32.const 0) (local.get $arity))
     (throw $fail)
     (unreachable))
			     
  ;; generic_va_call
  (func $generic_va_call (export "generic_va_call")
     (param $proc (ref $procedure))
     (param $params (ref $vector))
     (result (ref eq))
     (local $entry (ref func))
     (local.set $entry (struct.get $procedure $entry (local.get $proc)))
     (block $error
	(block $0
	   (block $1
	      (block $2
		 (block $3
		    (block $4
		       (block $5
			  (br_table $0 $1 $2 $3 $4 $5 $error (i32.sub (i32.const -1) (struct.get $procedure $arity (local.get $proc)))))
		       ;; 5 mandatory arguments
		       (return_call_ref $func6
			  (local.get $proc)
			  (array.get $vector (local.get $params) (i32.const 0))
			  (array.get $vector (local.get $params) (i32.const 1))
			  (array.get $vector (local.get $params) (i32.const 2))
			  (array.get $vector (local.get $params) (i32.const 3))
			  (array.get $vector (local.get $params) (i32.const 4))
			  (call $make_list_params (local.get $params) (i32.const 5))
			  (ref.cast (ref $func6) (local.get $entry))))
		    ;; 4 mandatory arguments
		    (return_call_ref $func5
		       (local.get $proc)
		       (array.get $vector (local.get $params) (i32.const 0))
		       (array.get $vector (local.get $params) (i32.const 1))
		       (array.get $vector (local.get $params) (i32.const 2))
		       (array.get $vector (local.get $params) (i32.const 3))
		       (call $make_list_params (local.get $params) (i32.const 4))
		       (ref.cast (ref $func5) (local.get $entry))))
		 ;; 3 mandatory arguments
		 (return_call_ref $func4
		    (local.get $proc)
		    (array.get $vector (local.get $params) (i32.const 0))
		    (array.get $vector (local.get $params) (i32.const 1))
		    (array.get $vector (local.get $params) (i32.const 2))
		    (call $make_list_params (local.get $params) (i32.const 3))
		    (ref.cast (ref $func4) (local.get $entry))))
	      ;; 2 mandatory arguments
	      (return_call_ref $func3
		 (local.get $proc)
		 (array.get $vector (local.get $params) (i32.const 0))
		 (array.get $vector (local.get $params) (i32.const 1))
		 (call $make_list_params (local.get $params) (i32.const 2))
		 (ref.cast (ref $func3) (local.get $entry))))
	   ;; 1 mandatory argument
	   (return_call_ref $func2
	      (local.get $proc)
	      (array.get $vector (local.get $params) (i32.const 0))
	      (call $make_list_params (local.get $params) (i32.const 1))
	      (ref.cast (ref $func2) (local.get $entry))))
	;; 0 mandatory argument
	(return_call_ref $func1
	   (local.get $proc)
	   (call $make_list_params (local.get $params) (i32.const 0))
	   (ref.cast (ref $func1) (local.get $entry))))
     (unreachable))
  
  ;; --------------------------------------------------------
  ;; OS functions
  ;; --------------------------------------------------------

  (global $OS_CLASS (export "OS_CLASS") (ref $bstring)
    ;; ASCII for 'wasm'
    (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))

  (global $OS_NAME (export "OS_NAME") (ref $bstring)
    ;; ASCII for 'wasm'
    (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))
    
  (global $OS_ARCH (export "OS_ARCH") (ref $bstring)
    ;; ASCII for 'wasm'
    (array.new_fixed $bstring 4 (i32.const 0x77) (i32.const 0x61) (i32.const 0x73) (i32.const 0x6D)))
    
  (global $OS_TMP (export "OS_TMP") (ref $bstring)
    ;; ASCII for '/tmp'
    (array.new_fixed $bstring 4 (i32.const 0x2F) (i32.const 0x74) (i32.const 0x6D) (i32.const 0x70)))

  ;; --------------------------------------------------------
  ;; Object functions
  ;; --------------------------------------------------------
  
  (func $BGL_OBJECT_CLASS_NUM_SET (export "BGL_OBJECT_CLASS_NUM_SET")
    (param $o (ref $BgL_objectz00_bglt))
    (param $num i64)
    (result (ref eq))
    (struct.set $BgL_objectz00_bglt $header (local.get $o) (local.get $num))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Mutext functions
  ;; --------------------------------------------------------

  (func $bgl_make_nil_mutex (export "bgl_make_nil_mutex")
    (result (ref $mutex))
    (global.get $mutex-default-value))

  (func $bgl_make_nil_condvar (export "bgl_make_nil_condvar")
    (result (ref $condvar))
    (global.get $condvar-default-value))

  ;; --------------------------------------------------------
  ;; RGC functions
  ;; --------------------------------------------------------


;*   ;; close_file_input_port                                          */
;*   (func $close_file_input_port                                      */
;*     (param $port (ref $file-input-port))                            */
;*     (result (ref eq))                                               */
;*                                                                     */
;*     (call $js_close_file (struct.get $file-input-port $fd (local.get $port))) */
;*     (local.get $port))                                              */
;*                                                                     */
;*   ;; bgl_close_input_port                                           */
;*   (func $bgl_close_input_port (export "bgl_close_input_port")       */
;*      (param $port (ref $input-port))                                */
;*      (result (ref eq))                                              */
;*                                                                     */
;*      (local $rgc (ref $rgc))                                        */
;*      (local.set $rgc (struct.get $input-port $rgc (local.get $port))) */
;*                                                                     */
;*      (struct.set $rgc $eof (local.get $rgc) (i32.const 1 #;TRUE))   */
;*      ;; TODO: call chook                                            */
;*                                                                     */
;*      ;; is closed                                                   */
;*      (struct.set $input-port $isclosed (local.get $port) (i32.const 1)) */
;*                                                                     */
;*      (if (ref.test (ref $file-input-port) (local.get $port))        */
;* 	 (then (return (call $close_file_input_port (ref.cast (ref $file-input-port) (local.get $port)))))) */
;*                                                                     */
;*      ;; Default implementation                                      */
;*      (local.get $port))                                             */

  ;; --------------------------------------------------------
  ;; Hash functions
  ;; --------------------------------------------------------

  (func $bgl_string_hash (export "bgl_string_hash")
    (param $str (ref $bstring))
    (param $start i32)
    (param $len i32)
    (result i64)
    (local $r i64)
    (local $i i32)
    ;; We use the same algorithm as for the C and Java implementation of Bigloo runtime.
    (local.set $r (i64.const 5381))
    (local.set $i (local.get $start))
    (loop $for-loop
      (if (i32.lt_u (local.get $i) (local.get $len))
        (then
          ;; r <- r + (r << 5) + s[i]
          (local.set $r
            (i64.add
              (local.get $r)
              (i64.add
                (i64.shl
                  (local.get $r)
                  (i64.const 5))
                (i64.extend_i32_u (array.get $bstring (local.get $str) (local.get $i))))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $for-loop))))
    (i64.and
      (local.get $r)
      (i64.const 536870911 #;((1 << 29) - 1))))

  (func $bgl_string_hash_persistent (export "bgl_string_hash_persistent")
    (param $str (ref $bstring))
    (param $start i32)
    (param $len i32)
    (result i64)
    (call $bgl_string_hash (local.get $str) (local.get $start) (local.get $len)))

  (func $bgl_symbol_hash_number (export "bgl_symbol_hash_number")
    (param $sym (ref $symbol))
    (result i64)
    (i64.add
      (call $bgl_string_hash 
        (struct.get $symbol $str (local.get $sym)) 
        (i32.const 0) 
        (array.len (struct.get $symbol $str (local.get $sym))))
      (i64.const 1)))

  (func $bgl_symbol_hash_number_persistent (export "bgl_symbol_hash_number_persistent")
    (param $sym (ref $symbol))
    (result i64)
    (call $bgl_symbol_hash_number (local.get $sym)))

  (func $bgl_keyword_hash_number (export "bgl_keyword_hash_number")
    (param $key (ref $keyword))
    (result i64)
    (i64.add
      (call $bgl_string_hash 
        (struct.get $keyword $str (local.get $key)) 
        (i32.const 0) 
        (array.len (struct.get $keyword $str (local.get $key))))
      (i64.const 2)))

  (func $bgl_keyword_hash_number_persistent (export "bgl_keyword_hash_number_persistent")
    (param $key (ref $keyword))
    (result i64)
    (call $bgl_keyword_hash_number (local.get $key)))

  (func $bgl_pointer_hash_number (export "bgl_pointer_hash_number")
    (param $obj (ref eq))
    (param $power i64)
    (result i64)
    (i64.rem_u
      (call $bgl_obj_hash_number (local.get $obj))
      (local.get $power)))

  (func $bgl_foreign_hash_number (export "bgl_foreign_hash_number")
    (param $obj (ref $foreign))
    (result i64)
    (i64.extend_i32_u (struct.get $foreign $ptr (local.get $obj))))

  ;; --------------------------------------------------------
  ;; lockf function and constants
  ;; --------------------------------------------------------

  (global $F_LOCK (export "F_LOCK") i32 (i32.const 0))
  (global $F_TLOCK (export "F_TLOCK") i32 (i32.const 0))
  (global $F_ULOCK (export "F_ULOCK") i32 (i32.const 0))
  (global $F_TEST (export "F_TEST") i32 (i32.const 0))
  (func $bgl_lockf (export "bgl_lockf")
    (param $port (ref $output-port))
    (param i32)
    (param i64)
    (result i32)
    ;; Not implemented (not supported by NodeJS fs API).
    (i32.const 0 #;FALSE))

  ;; --------------------------------------------------------
  ;; Math functions
  ;; --------------------------------------------------------

  (export "fmod" (func $fmod))
  (export "exp" (func $exp))
  (export "log" (func $log))
  (export "log2" (func $log2))
  (export "log10" (func $log10))
  (export "sin" (func $sin))
  (export "cos" (func $cos))
  (export "tan" (func $tan))
  (export "asin" (func $asin))
  (export "acos" (func $acos))
  (export "atan" (func $atan))
  (export "atan2" (func $atan2))
  (export "pow" (func $pow))
  (export "RANDOMFL" (func $RANDOMFL))

  ;; flonums
  (export "bgl_double_to_ieee_string" (func $bgl_double_to_ieee_string))
  (export "bgl_float_to_ieee_string" (func $bgl_float_to_ieee_string))
  
  (func $bgl_double_to_ieee_string
     (param $x f64)
     (result (ref $bstring))
     ;; CARE MS 16sep2024: TODO
     (global.get $bstring-default-value))
  
  (func $bgl_float_to_ieee_string
     (param $x f32)
     (result (ref $bstring))
     ;; CARE MS 16sep2024: TODO
     (global.get $bstring-default-value))
  
  ;; --------------------------------------------------------
  ;; Date functions
  ;; --------------------------------------------------------

  (type $stringarray (array (ref $bstring)))

  (global $day_names (mut (ref null $stringarray)) (ref.null none))
  (global $day_anames (mut (ref null $stringarray)) (ref.null none))

  (func $make_day_name 
    (param $day i32) 
    (param $longFormat i32) 
    (result (ref $bstring))
    
    (call $load_string
      (i32.const 128)
      (call $js_date_day_name 
        (local.get $day) 
        (local.get $longFormat) 
        (i32.const 128))))

  (func $make_day_names (param $longFormat i32) (result (ref $stringarray))
    (array.new_fixed $stringarray 7
      (call $make_day_name (i32.const 0) (local.get $longFormat))
      (call $make_day_name (i32.const 1) (local.get $longFormat))
      (call $make_day_name (i32.const 2) (local.get $longFormat))
      (call $make_day_name (i32.const 3) (local.get $longFormat))
      (call $make_day_name (i32.const 4) (local.get $longFormat))
      (call $make_day_name (i32.const 5) (local.get $longFormat))
      (call $make_day_name (i32.const 6) (local.get $longFormat))))

  (func $bgl_day_name (export "bgl_day_name") 
     (param $day i32) 
     (result (ref $bstring))
     (if (ref.is_null (global.get $day_names))
	 (then
	    (global.set $day_names (call $make_day_names (i32.const 1 #;(Long format))))))
     (array.get $stringarray (global.get $day_names) (i32.sub (local.get $day) (i32.const 1))))

  (func $bgl_day_aname (export "bgl_day_aname") 
    (param $day i32) 
    (result (ref $bstring))
    (if (ref.is_null (global.get $day_anames))
      (then (global.set $day_anames (call $make_day_names (i32.const 0 #;(Short format))))))
    (array.get $stringarray (global.get $day_anames) (i32.sub (local.get $day) (i32.const 1))))

  (global $month_names (mut (ref null $stringarray)) (ref.null none))
  (global $month_anames (mut (ref null $stringarray)) (ref.null none))

  (func $make_month_name 
    (param $month i32) 
    (param $longFormat i32) 
    (result (ref $bstring))
    
    (call $load_string
      (i32.const 128)
      (call $js_date_month_name 
        (local.get $month) 
        (local.get $longFormat) 
        (i32.const 128))))

  (func $make_month_names (param $longFormat i32) (result (ref $stringarray))
    (array.new_fixed $stringarray 12
      (call $make_month_name (i32.const 0) (local.get $longFormat))
      (call $make_month_name (i32.const 1) (local.get $longFormat))
      (call $make_month_name (i32.const 2) (local.get $longFormat))
      (call $make_month_name (i32.const 3) (local.get $longFormat))
      (call $make_month_name (i32.const 4) (local.get $longFormat))
      (call $make_month_name (i32.const 5) (local.get $longFormat))
      (call $make_month_name (i32.const 6) (local.get $longFormat))
      (call $make_month_name (i32.const 7) (local.get $longFormat))
      (call $make_month_name (i32.const 8) (local.get $longFormat))
      (call $make_month_name (i32.const 9) (local.get $longFormat))
      (call $make_month_name (i32.const 10) (local.get $longFormat))
      (call $make_month_name (i32.const 11) (local.get $longFormat))))

  (func $bgl_month_name (export "bgl_month_name") 
    (param $month i32) 
    (result (ref $bstring))
    (if (ref.is_null (global.get $month_names))
      (then (global.set $month_names (call $make_month_names (i32.const 1 #;(Long format))))))
    (array.get $stringarray (global.get $month_names) (i32.sub (local.get $month) (i32.const 1))))

  (func $bgl_month_aname (export "bgl_month_aname") 
    (param $month i32) 
    (result (ref $bstring))
    (if (ref.is_null (global.get $month_anames))
      (then (global.set $month_anames (call $make_month_names (i32.const 0 #;(Short format))))))
    (array.get $stringarray (global.get $month_anames) (i32.sub (local.get $month) (i32.const 1))))

  (func $bgl_make_date (export "bgl_make_date")
    (param $ns i64)
    (param $s i32)
    (param $m i32)
    (param $h i32)
    (param $mday i32)
    (param $mon i32)
    (param $year i32)
    (param $tz i64)
    (param $istz i32)
    (param $isdst i32)
    (result (ref $date))
    (call $bgl_update_date
      (struct.new_default $date)
      (local.get $ns)
      (local.get $s)
      (local.get $m)
      (local.get $h)
      (local.get $mday)
      (local.get $mon)
      (local.get $year)
      (local.get $tz)
      (local.get $istz)
      (local.get $isdst)))

  (func $bgl_update_date (export "bgl_update_date")
    (param $date (ref $date))
    (param $ns i64)
    (param $s i32)
    (param $m i32)
    (param $h i32)
    (param $mday i32)
    (param $mon i32)
    (param $year i32)
    (param $tz i64)
    (param $istz i32)
    (param $isdst i32)
    (result (ref $date))
    (struct.set $date $nanosecond (local.get $date) (i64.rem_u (local.get $ns) (i64.const 1000000000)))
    (struct.set $date $second (local.get $date) (i32.add (local.get $s) (i32.wrap_i64 (i64.div_u (local.get $ns) (i64.const 1000000000)))))
    (struct.set $date $minute (local.get $date) (local.get $m))
    (struct.set $date $hour (local.get $date) (local.get $h))
    (struct.set $date $day (local.get $date) (local.get $mday))
    (struct.set $date $month (local.get $date) (local.get $mon))
    (struct.set $date $year (local.get $date) (local.get $year))
    (struct.set $date $is-dst (local.get $date) (local.get $isdst))

    (if (local.get $istz)
      (then
        (struct.set $date $time 
          (local.get $date)
          (call $js_date_mktimegm
            (local.get $year)
            (local.get $mon)
            (local.get $mday)
            (local.get $h)
            (local.get $m)
            (local.get $s)
            (i64.div_u (local.get $ns) (i64.const 1000000)))))
      (else
        (struct.set $date $time  
          (local.get $date)
          (call $js_date_mktime
            (local.get $year)
            (local.get $mon)
            (local.get $mday)
            (local.get $h)
            (local.get $m)
            (local.get $s)
            (i64.div_u (local.get $ns) (i64.const 1000000))))))

    ;; FIXME: handle timezone correctly! This code is completly buggy.

    (local.get $date))

  (func $BGL_DATE_UPDATE_MILLISECOND (export "BGL_DATE_UPDATE_MILLISECOND")
    (param $date (ref $date))
    (param $ms i64)
    (result i64)
    (struct.set $date $nanosecond (local.get $date) (i64.mul (local.get $ms) (i64.const 1000000)))
    (struct.get $date $nanosecond (local.get $date)))

  (func $BGL_DATE_UPDATE_SECOND (export "BGL_DATE_UPDATE_SECOND")
    (param $date (ref $date))
    (param $s i32)
    (result i32)
    (struct.set $date $second (local.get $date) (local.get $s))
    (local.get $s))

  (func $BGL_DATE_UPDATE_MINUTE (export "BGL_DATE_UPDATE_MINUTE")
    (param $date (ref $date))
    (param $m i32)
    (result i32)
    (struct.set $date $minute (local.get $date) (local.get $m))
    (local.get $m))

  (func $BGL_DATE_UPDATE_TIME (export "BGL_DATE_UPDATE_TIME")
    (param $date (ref $date))
    (param $s i64)
    (result i64)
    (struct.set $date $time (local.get $date) (i64.mul (local.get $s) (i64.const 1000)))
    (local.get $s))

  ;; --------------------------------------------------------
  ;; Sockets
  ;; --------------------------------------------------------
  (func $SOCKET_INPUT (export "SOCKET_INPUT")
     (param $socket (ref $socket))
     (result (ref $input-port))
     (global.get $input-port-default-value))
  
  (func $SOCKET_OUTPUT (export "SOCKET_OUTPUT")
     (param $socket (ref $socket))
     (result (ref $output-port))
     (global.get $output-port-default-value))

  ;; --------------------------------------------------------
  ;; OS
  ;; --------------------------------------------------------
  (func $bgl_signal (export "bgl_signal")
     (param $sig i32)
     (param $hdl (ref eq))
     (result (ref eq))
     (call $js_signal (local.get $sig) (local.get $hdl))
     (return (global.get $BUNSPEC)))
		       
  ;; --------------------------------------------------------
  ;; Dynamic env functions
  ;; --------------------------------------------------------

  (global $current-dynamic-env (ref $dynamic-env) 
    (struct.new $dynamic-env
      ;; $exitd_top
       (struct.new $exit
	  (i64.const 0)
	  (i64.const 0)
	  (global.get $BNIL)
	  (ref.null none))
      ;; $exitd_val
      (struct.new $pair
	 (struct.new $pair
	    (global.get $BUNSPEC)
	    (global.get $BUNSPEC))
	 (global.get $BUNSPEC))
      ;; $uncaught-exception-handler
      (global.get $BNIL)
      ;; $error-handler
      (struct.new $pair
	 (global.get $BUNSPEC)
	 (global.get $BFALSE))
      ;; $current-output-port
      (ref.null none)
      ;; $current-error-port
      (ref.null none)
      ;; $current-input-port
      (ref.null none)
      ;; $module
      (global.get $BUNSPEC)
      ;; $abase
      (global.get $BUNSPEC)
      ))

  (func $BGL_CURRENT_DYNAMIC_ENV (export "BGL_CURRENT_DYNAMIC_ENV")
    (result (ref $dynamic-env))
    (global.get $current-dynamic-env))

  (func $BGL_ENV_CURRENT_OUTPUT_PORT (export "BGL_ENV_CURRENT_OUTPUT_PORT")
    (param $env (ref $dynamic-env))
    (result (ref $output-port))
    (ref.cast (ref $output-port)
       (struct.get $dynamic-env $current-output-port (local.get $env))))

  (func $BGL_ENV_CURRENT_ERROR_PORT (export "BGL_ENV_CURRENT_ERROR_PORT")
    (param $env (ref $dynamic-env))
    (result (ref $output-port))
    (ref.cast (ref $output-port)
       (struct.get $dynamic-env $current-error-port (local.get $env))))

  (func $BGL_ENV_CURRENT_INPUT_PORT (export "BGL_ENV_CURRENT_INPUT_PORT")
    (param $env (ref $dynamic-env))
    (result (ref $input-port))
    (ref.cast (ref $input-port)
       (struct.get $dynamic-env $current-input-port (local.get $env))))

  (func $BGL_ENV_CURRENT_OUTPUT_PORT_SET (export "BGL_ENV_CURRENT_OUTPUT_PORT_SET")
    (param $env (ref $dynamic-env))
    (param $port (ref $output-port))
    (struct.set $dynamic-env $current-output-port (local.get $env) (local.get $port)))

  (func $BGL_ENV_CURRENT_ERROR_PORT_SET (export "BGL_ENV_CURRENT_ERROR_PORT_SET")
    (param $env (ref $dynamic-env))
    (param $port (ref $output-port))
    (struct.set $dynamic-env $current-error-port (local.get $env) (local.get $port)))

  (func $BGL_ENV_CURRENT_INPUT_PORT_SET (export "BGL_ENV_CURRENT_INPUT_PORT_SET")
    (param $env (ref $dynamic-env))
    (param $port (ref $input-port))
    (struct.set $dynamic-env $current-input-port (local.get $env) (local.get $port)))

  (func $BGL_MODULE (export "BGL_MODULE")
     (result (ref eq))
     (struct.get $dynamic-env $module (global.get $current-dynamic-env)))
  
  (func $BGL_MODULE_SET (export "BGL_MODULE_SET")
     (param $mod (ref eq))
     (result (ref eq))
     (struct.set $dynamic-env $module (global.get $current-dynamic-env) (local.get $mod))
     (local.get $mod))
  
  (func $BGL_ABASE (export "BGL_ABASE")
     (result (ref eq))
     (struct.get $dynamic-env $abase (global.get $current-dynamic-env)))
  
  (func $BGL_ABASE_SET (export "BGL_ABASE_SET")
     (param $abase (ref eq))
     (result (ref eq))
     (struct.set $dynamic-env $abase (global.get $current-dynamic-env) (local.get $abase))
     (local.get $abase))
  
  ;; --------------------------------------------------------
  ;; Main function
  ;; --------------------------------------------------------

  (func $main (export "__js_bigloo_main")
    (local $i i32)
    (local $argv (ref eq))
    (local.set $i (i32.sub (global.get $js_argc) (i32.const 1)))
    (local.set $argv (global.get $BNIL))
    (loop $loop
      (if (i32.ge_s (local.get $i) (i32.const 0))
        (then
          (local.set $argv
            (struct.new $pair
              (call $load_string (i32.const 128)
                (call $js_get_arg (local.get $i) (i32.const 128)))
              (local.get $argv)))
          (local.set $i (i32.sub (local.get $i) (i32.const 1)))
          (br $loop))))

    (call $bgl_init_io)
    
    (drop
       (call $bigloo_main (ref.cast (ref $pair) (local.get $argv)))))

  (func $BIGLOO_EXIT
     (export "BIGLOO_EXIT")
     (param (ref eq))
     (result (ref eq))
     (drop
	(call $bgl_flush_output_port
	   (call $BGL_ENV_CURRENT_OUTPUT_PORT
	      (call $BGL_CURRENT_DYNAMIC_ENV))))
     (drop
	(call $bgl_flush_output_port
	   (call $BGL_ENV_CURRENT_ERROR_PORT
	      (call $BGL_CURRENT_DYNAMIC_ENV))))
     (call $js_exit
	(if (result i32)
	    (call $INTEGERP (local.get 0))
	    (then (i32.wrap_i64 (call $OBJ_TO_INT (local.get 0))))
	    (else (i32.const 0))))
     (global.get $BUNSPEC))
)

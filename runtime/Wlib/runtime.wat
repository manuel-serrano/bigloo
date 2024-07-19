(module $__runtime
  ;; WASI
  (import "wasi_snapshot_preview1" "fd_write" (func $wasi_fd_write (param i32 i32 i32 i32) (result i32)))

  ;; General bigloo memory
  (memory 1)
  (export "memory" (memory 0))

  ;; /!\ DO NOT MODIFY THE FOLLOWING LINE. 
  ;; It is used to include the content of 'runtime.types'.
  (;TYPES;)

  (global $BUNSPEC (export "BUNSPEC") (ref $bunspec) (struct.new $bunspec))
  (global $BOPTIONAL (export "BOPTIONAL") (ref $boptional) (struct.new $boptional))
  (global $BKEY (export "BKEY") (ref $bkey) (struct.new $bkey))
  (global $BREST (export "BREST") (ref $brest) (struct.new $brest))
  (global $BEOA (export "BEOA") (ref $beoa) (struct.new $beoa))
  (global $BFALSE (export "BFALSE") (ref $bbool) (struct.new $bbool (i32.const 0)))
  (global $BTRUE (export "BTRUE") (ref $bbool) (struct.new $bbool (i32.const 1)))

  ;; --------------------------------------------------------
  ;; Dynamic env functions
  ;; --------------------------------------------------------

  (global $current-dynamic-env (ref $dynamic-env) (struct.new_default $dynamic-env))

  (func $BGL_CURRENT_DYNAMIC_ENV (export "BGL_CURRENT_DYNAMIC_ENV")
    (result (ref null $dynamic-env))
    (global.get $current-dynamic-env))

  (func $BGL_ENV_CURRENT_OUTPUT_PORT (export "BGL_ENV_CURRENT_OUTPUT_PORT")
    (param $env (ref null $dynamic-env))
    (result (ref null $output-port))
    (struct.get $dynamic-env $current-out-port (local.get $env)))

  (func $BGL_ENV_CURRENT_ERROR_PORT (export "BGL_ENV_CURRENT_ERROR_PORT")
    (param $env (ref null $dynamic-env))
    (result (ref null $output-port))
    (struct.get $dynamic-env $current-err-port (local.get $env)))

  (func $BGL_ENV_CURRENT_INPUT_PORT (export "BGL_ENV_CURRENT_INPUT_PORT")
    (param $env (ref null $dynamic-env))
    (result (ref null $input-port))
    (struct.get $dynamic-env $current-in-port (local.get $env)))

  (func $BGL_ENV_CURRENT_OUTPUT_PORT_SET (export "BGL_ENV_CURRENT_OUTPUT_PORT_SET")
    (param $env (ref null $dynamic-env))
    (param $port (ref null $output-port))
    (struct.set $dynamic-env $current-out-port (local.get $env) (local.get $port)))

  (func $BGL_ENV_CURRENT_ERROR_PORT_SET (export "BGL_ENV_CURRENT_ERROR_PORT_SET")
    (param $env (ref null $dynamic-env))
    (param $port (ref null $output-port))
    (struct.set $dynamic-env $current-err-port (local.get $env) (local.get $port)))

  (func $BGL_ENV_CURRENT_INPUT_PORT_SET (export "BGL_ENV_CURRENT_INPUT_PORT_SET")
    (param $env (ref null $dynamic-env))
    (param $port (ref null $input-port))
    (struct.set $dynamic-env $current-in-port (local.get $env) (local.get $port)))

  ;; --------------------------------------------------------
  ;; Boolean functions
  ;; --------------------------------------------------------

  (func $BBOOL (export "BBOOL") (param i32) (result (ref null $bbool))
    (if (result (ref $bbool)) (local.get 0)
        (then (global.get $BTRUE))
        (else (global.get $BFALSE))))

  (func $CBOOL (export "CBOOL") (param eqref) (result i32)
      (if (result i32)
        (ref.test (ref $bbool) (local.get 0))
        (then (struct.get $bbool 0 (ref.cast (ref $bbool) (local.get 0))))
        (else (i32.const 1))))

  ;; --------------------------------------------------------
  ;; Custom functions
  ;; --------------------------------------------------------

  (func $CUSTOM_IDENTIFIER_SET (export "CUSTOM_IDENTIFIER_SET")
    (param $custom (ref null $custom))
    (param $ident (ref null $bstring))
    (result eqref)
    (struct.set $custom $ident (local.get $custom) (local.get $ident))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Multiple values functions
  ;; --------------------------------------------------------

  (global $mvalues_count (export "mvalues_count") (mut i32) (i32.const 0))
  (global $mvalues (export "mvalues") (mut (ref $vector)) (array.new_default $vector (i32.const 8)))
  (func $BGL_MVALUES_VAL (export "BGL_MVALUES_VAL") (param $i i32) (result eqref) (array.get $vector (global.get $mvalues) (local.get $i)))
  (func $BGL_MVALUES_VAL_SET (export "BGL_MVALUES_VAL_SET") (param $i i32) (param $val eqref) (result eqref) (array.set $vector (global.get $mvalues) (local.get $i) (local.get $val)) (local.get $val))
  (func $BGL_MVALUES_NUMBER (export "BGL_MVALUES_NUMBER") (result i32) (global.get $mvalues_count))
  (func $BGL_MVALUES_NUMBER_SET (export "BGL_MVALUES_NUMBER_SET") (param $n i32) (result i32)
    (global.set $mvalues_count (local.get $n))
    ;; Reallocate mvalues if not large enough.
    (if (i32.gt_u (local.get $n) (array.len (global.get $mvalues)))
      (then (global.set $mvalues (array.new_default $vector (local.get $n)))))
    (local.get $n))

  ;; --------------------------------------------------------
  ;; Class functions
  ;; --------------------------------------------------------

  (func $BGL_CLASS_SUBCLASSES_SET (export "BGL_CLASS_SUBCLASSES_SET")
    (param $class (ref null $class))
    (param $subclasses eqref)
    (result eqref)
    (struct.set $class $subclasses (local.get $class) (local.get $subclasses))
    (global.get $BUNSPEC))

  (func $BGL_CLASS_DIRECT_FIELDS_SET (export "BGL_CLASS_DIRECT_FIELDS_SET")
    (param $class (ref null $class))
    (param $direct_fields (ref null $vector))
    (result eqref)
    (struct.set $class $direct_fields (local.get $class) (local.get $direct_fields))
    (global.get $BUNSPEC))

  (func $BGL_CLASS_ALL_FIELDS_SET (export "BGL_CLASS_ALL_FIELDS_SET")
    (param $class (ref null $class))
    (param $all_fields (ref null $vector))
    (result eqref)
    (struct.set $class $all_fields (local.get $class) (local.get $all_fields))
    (global.get $BUNSPEC))

  (func $BGL_CLASS_EVDATA_SET (export "BGL_CLASS_EVDATA_SET")
    (param $class (ref null $class))
    (param $evdata eqref)
    (result eqref)
    (struct.set $class $evdata (local.get $class) (local.get $evdata))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Cell functions
  ;; --------------------------------------------------------

  (func $CELL_SET (export "CELL_SET") (param $c (ref null $cell)) (param $v eqref) (result eqref)
    (struct.set $cell $car (local.get $c) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Pair functions
  ;; --------------------------------------------------------

  (func $SET_CAR (export "SET_CAR") (param $p (ref null $pair)) (param $v eqref) (result eqref)
    (struct.set $pair $car (local.get $p) (local.get $v))
    (global.get $BUNSPEC))
  
  (func $SET_CDR (export "SET_CDR") (param $p (ref null $pair)) (param $v eqref) (result eqref)
    (struct.set $pair $cdr (local.get $p) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Procedure functions
  ;; --------------------------------------------------------

  (func $MAKE_FX_PROCEDURE (export "MAKE_FX_PROCEDURE")
    (param $entry eqref)
    (param $arity i32)
    (param $size i32)
    (result (ref null $procedure))
    (struct.new $procedure
      (struct.get $tmpfun 0 (ref.cast (ref $tmpfun) (local.get $entry)))
      (ref.null none)
      (local.get $arity)
      (array.new_default $vector (local.get $size))))

  (func $PROCEDURE_CORRECT_ARITYP (export "PROCEDURE_CORRECT_ARITYP")
    (param $p (ref null $procedure)) 
    (param $i i32) 
    (result i32)
    (local $arity i32)
    (local.set $arity (struct.get $procedure $arity (local.get $p)))
    ;; (arity == i) || ((arity < 0) && (-i - 1 <= arity))
    (i32.or 
      (i32.eq (local.get $arity) (local.get $i))
      (i32.and 
        (i32.lt_s (local.get $arity) (i32.const 0))
        (i32.lt_s (i32.sub (i32.const -1) (local.get $i)) (local.get $arity)))))

  (func $PROCEDURE_SET (export "PROCEDURE_SET") 
    (param $p (ref null $procedure)) 
    (param $i i32) 
    (param $v eqref) 
    (result eqref)
    (array.set $vector (struct.get $procedure $env (local.get $p)) (local.get $i) (local.get $v))
    (global.get $BUNSPEC))

  (func $PROCEDURE_ATTR_SET (export "PROCEDURE_ATTR_SET") 
    (param $p (ref null $procedure)) 
    (param $v eqref) 
    (result eqref)
    (struct.set $procedure $attr (local.get $p) (local.get $v))
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; Vector functions
  ;; --------------------------------------------------------

  (func $bgl_fill_vector (export "bgl_fill_vector")
    (param $v (ref null $vector))
    (param $start i64)
    (param $end i64)
    (param $o eqref)
    (result eqref)
    (array.fill $vector 
      (ref.cast (ref $vector) (local.get $v)) ;; FIXME: remove the cast
      (i32.wrap_i64 (local.get $start)) 
      (local.get $o)
      (i32.wrap_i64 (i64.sub (local.get $end) (local.get $start))))
    (global.get $BUNSPEC))
  
  ;; --------------------------------------------------------
  ;; Typed vector functions
  ;; --------------------------------------------------------

  ;; TODO: implement tvector descr

  (func $TVECTOR_DESCR (export "TVECTOR_DESCR")
    (param $v arrayref)
    (result eqref)
    (global.get $BUNSPEC))

  (func $TVECTOR_DESCR_SET (export "TVECTOR_DESCR_SET")
    (param $v arrayref)
    (param $desc eqref)
    (result eqref)
    (global.get $BUNSPEC))

  ;; --------------------------------------------------------
  ;; String functions
  ;; --------------------------------------------------------

  ;; TODO: maybe implement this as a generic function in scheme
  (func $string_append 
    (export "string_append") 
    (param $a (ref null $bstring)) 
    (param $b (ref null $bstring))
    (result (ref null $bstring))
    (local $r (ref $bstring))
    (local.set $r
      (array.new_default $bstring 
        (i32.add 
          (array.len (local.get $a))
          (array.len (local.get $b)))))
    (array.copy $bstring $bstring (local.get $r) (i32.const 0) (local.get $a) (i32.const 0) (array.len (local.get $a)))
    (array.copy $bstring $bstring (local.get $r) (array.len (local.get $a)) (local.get $b) (i32.const 0) (array.len (local.get $b)))
    (local.get $r))

  (func $string_append_3
    (export "string_append_3")
    (param $a (ref null $bstring)) 
    (param $b (ref null $bstring))
    (param $c (ref null $bstring))
    (result (ref null $bstring))
    (local $r (ref $bstring))
    (local $l1 i32)
    (local $l2 i32)
    (local $l3 i32)
    (local.set $l1 (array.len (local.get $a)))
    (local.set $l2 (array.len (local.get $b)))
    (local.set $l3 (array.len (local.get $c)))
    (local.set $r
      (array.new_default $bstring 
        (i32.add 
          (i32.add (local.get $l1) (local.get $l2))
          (local.get $l3))))
    (array.copy $bstring $bstring (local.get $r) (i32.const 0) (local.get $a) (i32.const 0) (local.get $l1))
    (array.copy $bstring $bstring (local.get $r) (local.get $l1) (local.get $b) (i32.const 0) (local.get $l2))
    (array.copy $bstring $bstring (local.get $r) (i32.add (local.get $l1) (local.get $l2)) (local.get $c) (i32.const 0) (local.get $l3))
    (local.get $r))

  (func $c_substring
    (export "c_substring")
    (param $str (ref null $bstring))
    (param $min i64)
    (param $max i64)
    (result (ref null $bstring))
    (local $len i32)
    (local $r (ref $bstring))
    (local.set $len (i32.wrap_i64 (i64.sub (local.get $max) (local.get $min))))
    (local.set $r (array.new_default $bstring (local.get $len)))
    (array.copy $bstring $bstring
      (local.get $r)
      (i32.const 0)
      (local.get $str)
      (i32.wrap_i64 (local.get $min))
      (local.get $len))
    (local.get $r))

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
        (i64.const 0x7FFFFFFFFFFFFFFF (; NaN ;)))
      (i64.const 0x7FF0000000000000 (; Inf ;))))

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

  (func $PUSH_ENV_EXIT (export "PUSH_ENV_EXIT") 
    (param $env (ref null $dynamic-env)) 
    (param $v (ref null $exit)) 
    (param $protect i64) 
    (result eqref)
    (struct.set $exit $userp (local.get $v) (local.get $protect))
    (struct.set $exit $prev (local.get $v) (struct.get $dynamic-env 0 (local.get $env)))
    (struct.set $dynamic-env 0 (local.get $env) (local.get $v))
    (global.get $BUNSPEC))

  (func $PUSH_EXIT (export "PUSH_EXIT") 
    (param $v (ref null $exit)) 
    (param $protect i64) 
    (result eqref)
    (call $PUSH_ENV_EXIT 
        (global.get $current-dynamic-env)
        (local.get $v) 
        (local.get $protect)))

  (func $POP_ENV_EXIT (export "POP_ENV_EXIT")
    (param $env (ref null $dynamic-env)) 
    (result eqref)
    (struct.set $dynamic-env 0
        (local.get $env)
        (struct.get $exit $prev
            (struct.get $dynamic-env 0 (local.get $env))))
    (global.get $BUNSPEC))

  (func $POP_EXIT (export "POP_EXIT") (result eqref)
    (call $POP_ENV_EXIT (global.get $current-dynamic-env)))

  (func $EXITD_STAMP (export "EXITD_STAMP") (param $o eqref) (result (ref null $bint))
    (struct.new $bint (struct.get $exit $stamp (ref.cast (ref $exit) (local.get $o)))))

  (func $EXITD_CALLCCP (export "EXITD_CALLCCP") (param $o eqref) (result i32)
    (i32.const 0))

  (func $EXITD_TO_EXIT (export "EXITD_TO_EXIT") (param $o eqref) (result (ref null $exit))
    (ref.cast (ref $exit) (local.get $o)))

  (func $BGL_EXITD_PROTECT (export "BGL_EXITD_PROTECT") 
    (param (ref null $exit)) 
    (result eqref)
    (struct.get $exit $protect (local.get 0)))

  (func $BGL_EXITD_PROTECT_SET (export "BGL_EXITD_PROTECT_SET") 
    (param $e (ref null $exit)) 
    (param $p eqref) 
    (struct.set $exit $protect (local.get $e) (local.get $p)))

  (func $BGL_EXITD_PUSH_PROTECT (export "BGL_EXITD_PUSH_PROTECT") 
    (param $e (ref null $exit)) 
    (param $p eqref)
    (call $BGL_EXITD_PROTECT_SET (local.get $e)
      (struct.new $pair 
        (local.get $p)
        (struct.get $exit $protect (local.get $e)))))

  (func $BGL_ERROR_HANDLER_SET (export "BGL_ERROR_HANDLER_SET") 
    (param $hdl eqref) 
    (struct.set $dynamic-env $error-handler (global.get $current-dynamic-env) (local.get $hdl)))

  (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_GET (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_GET") (result eqref)
    (struct.get $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env)))

  (func $BGL_UNCAUGHT_EXCEPTION_HANDLER_SET (export "BGL_UNCAUGHT_EXCEPTION_HANDLER_SET") (param $hdl eqref)
    (struct.set $dynamic-env $uncaught-exception-handler (global.get $current-dynamic-env) (local.get $hdl)))

  (func $BGL_ENV_EXITD_TOP_AS_OBJ (export "BGL_ENV_EXITD_TOP_AS_OBJ") 
    (param $env (ref null $dynamic-env)) 
    (result eqref)
    (ref.cast (ref $exit) (struct.get $dynamic-env $top (local.get $env))))

  (func $BGL_EXITD_TOP_AS_OBJ (export "BGL_EXITD_TOP_AS_OBJ") (result eqref)
    (call $BGL_ENV_EXITD_TOP_AS_OBJ (global.get $current-dynamic-env)))

  (func $BGL_EXITD_BOTTOMP (export "BGL_EXITD_BOTTOMP") (param $o eqref) (result i32)
    (ref.is_null (struct.get $exit $prev (ref.cast (ref $exit) (local.get $o)))))

  (func $BGL_ENV_EXITD_VAL_SET (export "BGL_ENV_EXITD_VAL_SET") 
    (param $env (ref null $dynamic-env)) 
    (param $v eqref) 
    (result eqref)
    (struct.set $dynamic-env $exitd_val (local.get $env) (local.get $v))
    (global.get $BUNSPEC))

  (func $BGL_EXITD_VAL_SET (export "BGL_EXITD_VAL_SET") (param $v eqref) (result eqref)
    (call $BGL_ENV_EXITD_VAL_SET (global.get $current-dynamic-env) (local.get $v)))

  ;; --------------------------------------------------------
  ;; Generic variadic call builtin functions
  ;; --------------------------------------------------------

  (func $make_list_params (param $params (ref $vector)) (param $i i32) (result eqref)
    (local $len i32)
    (local $j i32)
    (local $list (ref null $pair))
    (local.set $len (array.len (local.get $params)))
    (local.set $j (i32.sub (local.get $len) (i32.const 1)))

    (block $break (loop $continue
      (if (i32.lt_s (local.get $j) (local.get $i)) (then (br $break)))

      (local.set $list (struct.new $pair (array.get $vector (local.get $params) (local.get $j)) (local.get $list)))
      (local.set $j (i32.sub (local.get $j) (i32.const 1)))

      (br $continue)))
    (local.get $list))

  (func $generic_va_call (export "generic_va_call") (param $proc (ref $procedure)) (param $params (ref $vector)) (result eqref)
    (local $entry funcref)
    (local.set $entry (struct.get $procedure $entry (local.get $proc)))
    (block $error
    (block $0
    (block $1
    (block $2
    (block $3
    (block $4
    (block $5
      (br_table $0 $1 $2 $3 $4 $5 $error (i32.sub (i32.const -1) (struct.get $procedure $arity (local.get $proc)))))

      ;; 5 mandatory argument
      (return (call_ref $func7
        (local.get $proc)
        (array.get $vector (local.get $params) (i32.const 0))
        (array.get $vector (local.get $params) (i32.const 1))
        (array.get $vector (local.get $params) (i32.const 2))
        (array.get $vector (local.get $params) (i32.const 3))
        (array.get $vector (local.get $params) (i32.const 4))
        (call $make_list_params (local.get $params) (i32.const 5))
        (ref.cast (ref $func7) (local.get $entry)))))
      ;; 4 mandatory argument
      (return (call_ref $func6
        (local.get $proc)
        (array.get $vector (local.get $params) (i32.const 0))
        (array.get $vector (local.get $params) (i32.const 1))
        (array.get $vector (local.get $params) (i32.const 2))
        (array.get $vector (local.get $params) (i32.const 3))
        (call $make_list_params (local.get $params) (i32.const 4))
        (ref.cast (ref $func6) (local.get $entry)))))
      ;; 3 mandatory argument
      (return (call_ref $func5
        (local.get $proc)
        (array.get $vector (local.get $params) (i32.const 0))
        (array.get $vector (local.get $params) (i32.const 1))
        (array.get $vector (local.get $params) (i32.const 2))
        (call $make_list_params (local.get $params) (i32.const 3))
        (ref.cast (ref $func5) (local.get $entry)))))
      ;; 2 mandatory argument
      (return (call_ref $func4
        (local.get $proc)
        (array.get $vector (local.get $params) (i32.const 0))
        (array.get $vector (local.get $params) (i32.const 1))
        (call $make_list_params (local.get $params) (i32.const 2))
        (ref.cast (ref $func4) (local.get $entry)))))
      ;; 1 mandatory argument
      (return (call_ref $func3
        (local.get $proc)
        (array.get $vector (local.get $params) (i32.const 0))
        (call $make_list_params (local.get $params) (i32.const 1))
        (ref.cast (ref $func3) (local.get $entry)))))
      ;; 0 mandatory argument
      (return (call_ref $func2
        (local.get $proc)
        (call $make_list_params (local.get $params) (i32.const 0))
        (ref.cast (ref $func2) (local.get $entry)))))
    (unreachable))

  ;; --------------------------------------------------------
  ;; Output port functions
  ;; --------------------------------------------------------
  
  (func $store_string
    (param $text (ref null $bstring))
    (param $addr i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (loop $loop
      (if (i32.lt_u (local.get $i) (array.len (local.get $text)))
        (then
          (i32.store8 (local.get $addr) (array.get $bstring (local.get $text) (local.get $i)))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (local.set $addr (i32.add (local.get $addr) (i32.const 1)))
          (br $loop)))))

  (func $bgl_display_char (export "bgl_display_char")
    (param $c i32)
    (param $port (ref null $output-port))
    (result eqref)
    (local.get $port))

  (func $bgl_display_substring (export "bgl_display_substring")
    (param $text (ref null $bstring))
    (param $start i64)
    (param $end i64)
    (param $port (ref null $output-port))
    (result eqref)

    ;; We write the ciovec_array at the memory address 128 (of the primary memory).
    (i32.store (i32.const 128) (i32.const 136))
    (i32.store (i32.const 132) (array.len (local.get $text)))
    (call $store_string
      (local.get $text)
      (i32.const 136))
    
    ;; FIXME: add thread synchronisation code
    (drop (call $wasi_fd_write
      (i32.const 1) ;; STDOUT, FIXME: should depend on output-port
      (i32.const 128) ;; address of iovs
      (i32.const 1) ;; length of iovs
      (i32.const 128) ;; address where to store the wrote bytes count
    ))

    (local.get $port))

  (func $bgl_display_string (export "bgl_display_string")
    (param $text (ref null $bstring))
    (param $port (ref null $output-port))
    (result eqref)
    (call $bgl_display_substring
      (local.get $text)
      (i64.const 0)
      (i64.extend_i32_u (array.len (local.get $text)))
      (local.get $port)))
)
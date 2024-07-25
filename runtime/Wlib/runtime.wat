(module $__runtime
  ;; WASI
  (import "wasi_snapshot_preview1" "fd_write" (func $wasi_fd_write (param i32 i32 i32 i32) (result i32)))
  (import "__js" "open_file" (func $js_open_file (param i32 i32 i32) (result i32)))
  (import "__js" "close_file" (func $js_close_file (param i32)))
  (import "__js" "read_file" (func $js_read_file (param i32 i32) (result i32)))
  (import "__js" "write_file" (func $js_write_file (param i32 i32 i32)))

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

  (global $current-dynamic-env (ref $dynamic-env) 
    (struct.new $dynamic-env
      ;; $exitd_top
      (struct.new_default $exit)
      ;; $exitd_val
      (struct.new $pair (struct.new $pair (global.get $BUNSPEC) (global.get $BUNSPEC)) (global.get $BUNSPEC))
      ;; $uncaught-exception-handler
      (ref.null none)
      ;; $error-handler
      (struct.new $pair (global.get $BUNSPEC) (global.get $BFALSE))
      
      ;; $current-out-port
      (struct.new $file-output-port 
        ;; Name
        (array.new_fixed $bstring 6 
          (i32.const 0x73) ;; s
          (i32.const 0x74) ;; t
          (i32.const 0x64) ;; d
          (i32.const 0x6F) ;; o
          (i32.const 0x75) ;; u
          (i32.const 0x74) ;; t
          )
        ;; CHook
        (global.get $BUNSPEC)
        ;; FHook
        (global.get $BUNSPEC)
        ;; Flushbuf
        (global.get $BUNSPEC)
        ;; Is closed
        (i32.const 0)
        ;; File descriptor
        (i32.const 1))
      ;; $current-err-port
      (struct.new $file-output-port 
        ;; Name
        (array.new_fixed $bstring 6 
          (i32.const 0x73) ;; s
          (i32.const 0x74) ;; t
          (i32.const 0x64) ;; d
          (i32.const 0x65) ;; e
          (i32.const 0x72) ;; r
          (i32.const 0x72) ;; r
          )
        ;; CHook
        (global.get $BUNSPEC)
        ;; FHook
        (global.get $BUNSPEC)
        ;; Flushbuf
        (global.get $BUNSPEC)
        ;; Is closed
        (i32.const 0)
        ;; File descriptor
        (i32.const 2))
      ;; $current-in-port
      (struct.new $input-port
        ;; Name
        (array.new_fixed $bstring 5
          (i32.const 0x73) ;; s
          (i32.const 0x74) ;; t
          (i32.const 0x64) ;; d
          (i32.const 0x69) ;; i
          (i32.const 0x6E) ;; n
          )
        ;; CHook
        (global.get $BUNSPEC)
        ;; RGC
        (ref.null extern))))

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

  (func $bgl_make_class (export "bgl_make_class")
    (param $name (ref null $symbol))
    (param $module (ref null $symbol))
    (param $num i64)
    (param $inheritance-num i64)
    (param $super eqref)
    (param $sub eqref)
    (param $alloc (ref null $procedure))
    (param $hash i64)
    (param $direct-fields (ref null $vector))
    (param $all-fields (ref null $vector))
    (param $constructor eqref)
    (param $virtual-fields (ref null $vector))
    (param $new eqref)
    (param $nil (ref null $procedure))
    (param $shrink eqref)
    (param $depth i64)
    (param $evdata eqref)
    (result (ref null $class))

    (local $self (ref $class))
    (local $ancestors (ref $vector))
    (local.set $ancestors (array.new_default $vector (i32.add (i32.wrap_i64 (local.get $depth)) (i32.const 1))))
    (if (i64.lt_u (local.get $depth) (i64.const 0))
      (then 
        (array.copy 
          $vector $vector
          (local.get $ancestors)
          (i32.const 0)
          (struct.get $class $ancestors (ref.cast (ref $class) (local.get $super)))
          (i32.const 0)
          (i32.wrap_i64 (local.get $depth)))))

    (local.set $self 
      (struct.new $class
        (local.get $name)
        (local.get $module)
        (local.get $new)
        (local.get $alloc)
        (local.get $nil)
        (global.get $BUNSPEC) (; NIL ;)
        (local.get $constructor)
        (local.get $super)
        (local.get $sub)
        (local.get $shrink)
        (local.get $evdata)
        (local.get $ancestors)
        (local.get $virtual-fields)
        (local.get $direct-fields)
        (local.get $all-fields)
        (local.get $hash)
        (local.get $num)
        (local.get $depth)))
    
    (array.set $vector (local.get $ancestors) (i32.wrap_i64 (local.get $depth)) (local.get $self))
    (local.get $self)
    )

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
    (struct.set $exit $prev (local.get $v) (struct.get $dynamic-env $exitd_top (local.get $env)))
    (struct.set $dynamic-env $exitd_top (local.get $env) (local.get $v))
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
    (struct.set $dynamic-env $exitd_top
        (local.get $env)
        (struct.get $exit $prev
            (struct.get $dynamic-env $exitd_top (local.get $env))))
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
  
  (func $BGL_ERROR_HANDLER_GET (export "BGL_ERROR_HANDLER_GET")
    (result eqref)
    (struct.get $dynamic-env $error-handler (global.get $current-dynamic-env)))

  (func $BGL_ENV_ERROR_HANDLER_GET (export "BGL_ENV_ERROR_HANDLER_GET")
    (param $env (ref null $dynamic-env))
    (result eqref)
    (struct.get $dynamic-env $error-handler (local.get $env)))

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
    (ref.cast (ref $exit) (struct.get $dynamic-env $exitd_top (local.get $env))))

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
  ;; IO functions
  ;; --------------------------------------------------------
  

  ;; --------------------------------------------------------
  ;; Output port functions
  ;; --------------------------------------------------------

  (func $store_substring
    (param $text (ref null $bstring))
    (param $start i64)
    (param $end i64)
    (param $addr i32)
    (local $i i32)
    (local.set $i (i32.wrap_i64 (local.get $start)))
    (loop $loop
      (if (i32.lt_u (local.get $i) (i32.wrap_i64 (local.get $end)))
        (then
          (i32.store8 (local.get $addr) (array.get $bstring (local.get $text) (local.get $i)))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (local.set $addr (i32.add (local.get $addr) (i32.const 1)))
          (br $loop)))))
  
  (func $store_string
    (param $text (ref null $bstring))
    (param $addr i32)
    (call $store_substring
      (local.get $text)
      (i64.const 0)
      (i64.extend_i32_u (array.len (local.get $text)))
      (local.get $addr)))

  (func $bgl_display_char (export "bgl_display_char")
    (param $c i32)
    (param $port (ref null $output-port))
    (result eqref)
    
    (i32.store8 (i32.const 128) (local.get $c))
    ;; FIXME: support other ports
    (call $js_write_file (i32.const 1) (i32.const 128) (i32.const 1))
    (local.get $port))

  (func $display_substring_file_port
    (param $text (ref $bstring))
    (param $start i64)
    (param $end i64)
    (param $port (ref $file-output-port))

    (call $store_substring
      (local.get $text)
      (local.get $start)
      (local.get $end)
      (i32.const 128))
    (call $js_write_file 
      (struct.get $file-output-port $fd (ref.cast (ref $file-output-port) (local.get $port))) 
      (i32.const 128) 
      (i32.wrap_i64 
        (i64.sub 
          (local.get $end) 
          (local.get $start)))))

  (func $display_substring_string_port
    (param $text (ref $bstring))
    (param $start i64)
    (param $end i64)
    (param $port (ref $string-output-port))

    (local $length i32)
    (local $new_buffer (ref $bstring))

    ;; Allocate space for new buffer.
    (local.set $length (i32.wrap_i64 (i64.sub (local.get $end) (local.get $start))))
    (local.set $new_buffer
      (array.new_default $bstring
        (i32.add 
          (array.len (struct.get $string-output-port $buffer (local.get $port)))
          (local.get $length))))
    
    ;; Copy data to new buffer.
    (array.copy $bstring $bstring 
      (local.get $new_buffer) (i32.const 0) 
      (struct.get $string-output-port $buffer (local.get $port)) (i32.const 0)
      (local.get $length))
    (array.copy $bstring $bstring
      (local.get $new_buffer) (local.get $length)
      (local.get $text) (i32.wrap_i64 (local.get $start))
      (local.get $length))

    (struct.set $string-output-port $buffer
      (local.get $port)
      (local.get $new_buffer)))

  (func $bgl_display_substring (export "bgl_display_substring")
    (param $text (ref null $bstring))
    (param $start i64)
    (param $end i64)
    (param $port (ref null $output-port))
    (result eqref)

    (if (ref.test (ref $file-output-port) (local.get $port))
      (then 
        (call $display_substring_file_port 
          (ref.cast (ref $bstring) (local.get $text))
          (local.get $start)
          (local.get $end)
          (ref.cast (ref $file-output-port) (local.get $port))))
      (else
        (call $display_substring_string_port 
          (ref.cast (ref $bstring) (local.get $text))
          (local.get $start)
          (local.get $end)
          (ref.cast (ref $string-output-port) (local.get $port)))))

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

  (func $flush_string_output_port
    (param $port (ref $string-output-port))
    (result eqref)
    (struct.get $string-output-port $buffer (local.get $port)))

  (func $flush_file_output_port
    (param $port (ref $file-output-port))
    (result eqref)
    ;; TODO: implement flush file output port
    (global.get $BTRUE))

  (func $bgl_flush_output_port (export "bgl_flush_output_port")
    (param $port (ref null $output-port))
    (result eqref)
    
    (if (ref.test (ref $string-output-port) (local.get $port))
      (then (return (call $flush_string_output_port (ref.cast (ref $string-output-port) (local.get $port))))))
    (if (ref.test (ref $file-output-port) (local.get $port))
      (then (return (call $flush_file_output_port (ref.cast (ref $file-output-port) (local.get $port))))))
    (unreachable))

  (func $bgl_reset_output_string_port (export "bgl_reset_output_string_port")
    (param $port (ref null $output-port))
    (result eqref)
    (local $str-port (ref $string-output-port))
    (local $buffer (ref $bstring))
    (local.set $str-port (ref.cast (ref $string-output-port) (local.get $port)))
    (local.set $buffer (struct.get $string-output-port $buffer (local.get $str-port)))
    (struct.set $string-output-port $buffer (local.get $str-port) (array.new_fixed $bstring 0))
    (local.get $buffer))

  (func $bgl_reset_output_port_error (export "bgl_reset_output_port_error")
    (param $port (ref null $output-port))
    (result eqref)
    (local.get $port))

  (data $string-output-port-name "string")
  (func $bgl_open_output_string (export "bgl_open_output_string")
    (param $buffer (ref null $bstring))
    (result (ref null $output-port))
    (struct.new $string-output-port
      ;; Name
      (array.new_data $bstring $string-output-port-name (i32.const 0) (i32.const 6))
      ;; CHook
      (global.get $BUNSPEC)
      ;; FHook
      (global.get $BUNSPEC)
      ;; Flushbuf
      (global.get $BUNSPEC)
      ;; Is closed
      (i32.const 0)
      ;; Buffer
      (array.new_fixed $bstring 0)))

  (func $bgl_open_output_file (export "bgl_open_output_file")
    (param $path (ref null $bstring))
    (param $buffer (ref null $bstring))
    (result eqref)
    (local $fd i32)
    ;; TODO: support buffered output (for now, $buffer is ignored)
    (call $store_string
      (local.get $path)
      (i32.const 128))
    (local.set $fd
      (call $js_open_file
        (i32.const 128)
        (array.len (local.get $path))
        ;; WRITE-ONLY flag
        (i32.const 1)))
    (struct.new $file-output-port
      ;; Name
      (local.get $path)
      ;; CHook
      (global.get $BUNSPEC)
      ;; FHook
      (global.get $BUNSPEC)
      ;; Flushbuf
      (global.get $BUNSPEC)
      ;; Is closed
      (i32.const 0)
      ;; File descriptor
      (local.get $fd)))

  (func $close_string_output_port
    (param $port (ref $string-output-port))
    (result eqref)
    (local $buffer (ref $bstring))
    (local.set $buffer (struct.get $string-output-port $buffer (local.get $port)))
    (struct.set $string-output-port $buffer (local.get $port) (array.new_fixed $bstring 0))
    (local.get $buffer))

  (func $close_file_output_port
    (param $port (ref $file-output-port))
    (result eqref)
    (call $js_close_file (struct.get $file-output-port $fd (local.get $port)))
    (global.get $BUNSPEC))

  (func $bgl_close_output_port (export "bgl_close_output_port")
    (param $port (ref null $output-port))
    (result eqref)
  
    (struct.set $output-port $isclosed (local.get $port) (i32.const 1))
    ;; TODO: call chook

    (if (ref.test (ref $string-output-port) (local.get $port))
      (then (return (call $close_string_output_port (ref.cast (ref $string-output-port) (local.get $port))))))
    (if (ref.test (ref $file-output-port) (local.get $port))
      (then (return (call $close_file_output_port (ref.cast (ref $file-output-port) (local.get $port))))))
    (unreachable)
  )

  ;; --------------------------------------------------------
  ;; RGC functions
  ;; --------------------------------------------------------

  (func $load_string
    (param $addr i32)
    (param $length i32)
    (result (ref $bstring))
    (local $str (ref $bstring))
    (local $i i32)
    (local.set $i (i32.const 0))
    (local.set $str (array.new_default $bstring (local.get $length)))
    (loop $loop
      (if (i32.lt_u (local.get $i) (local.get $length))
        (then 
          (array.set $bstring 
            (local.get $str)
            (local.get $i)
            (i32.load8_u (i32.add (local.get $addr) (local.get $i))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $loop))))
    (local.get $str))
)
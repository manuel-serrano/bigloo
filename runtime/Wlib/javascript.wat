;*=====================================================================*/
;*    /priv/serrano2/bigloo/wasm/runtime/Wlib/javascript.wat           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 25 11:07:11 2024                          */
;*    Last change :  Wed Sep 25 12:51:01 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    JavaScript imports                                               */
;*    -------------------------------------------------------------    */
;*    This file is concatanated to runtime.wat to build bigloo_x.wat   */
;*=====================================================================*/

(module $__runtime_javascript
  (import "__js" "not_implemented" (func $not_implemented (param i32)))
  (import "__js" "trace" (func $js_trace (param i32))) ;; FIXME: remove
  (import "__js" "internalError" (func $js_internal_error (param i32)))

  (import "__js" "argc" (global $js_argc i32))
  (import "__js" "get_arg" (func $js_get_arg (param i32 i32) (result i32)))

  (import "__js" "open_file" (func $js_open_file (param i32 i32 i32) (result i32)))
  (import "__js" "close_file" (func $js_close_file (param i32)))
  (import "__js" "read_file" (func $js_read_file (param i32 i32 i32) (result i32)))
  (import "__js" "write_file" (func $js_write_file (param i32 i32 i32)))
  (import "__js" "write_char" (func $js_write_char (param i32 i32)))
  (import "__js" "write_bignum" (func $js_write_bignum (param i32 externref)))

  (import "__js_math" "fmod" (func $fmod (param f64 f64) (result f64)))
  (import "__js_math" "exp" (func $exp (param f64) (result f64)))
  (import "__js_math" "log" (func $log (param f64) (result f64)))
  (import "__js_math" "log2" (func $log2 (param f64) (result f64)))
  (import "__js_math" "log10" (func $log10 (param f64) (result f64)))
  (import "__js_math" "sin" (func $sin (param f64) (result f64)))
  (import "__js_math" "cos" (func $cos (param f64) (result f64)))
  (import "__js_math" "tan" (func $tan (param f64) (result f64)))
  (import "__js_math" "asin" (func $asin (param f64) (result f64)))
  (import "__js_math" "acos" (func $acos (param f64) (result f64)))
  (import "__js_math" "atan" (func $atan (param f64) (result f64)))
  (import "__js_math" "atan2" (func $atan2 (param f64 f64) (result f64)))
  (import "__js_math" "pow" (func $pow (param f64 f64) (result f64)))
  (import "__js_math" "randomf" (func $RANDOMFL (result f64)))

  (import "__js_date" "current_seconds" (func $bgl_current_seconds (result i64)))
  (import "__js_date" "current_milliseconds" (func $bgl_current_milliseconds (result i64)))
  (import "__js_date" "current_microseconds" (func $bgl_current_microseconds (result i64)))
  (import "__js_date" "current_nanoseconds" (func $bgl_current_nanoseconds (result i64)))
  (import "__js_date" "mktime" (func $js_date_mktime (param i32 i32 i32 i32 i32 i32 i64) (result i64)))
  (import "__js_date" "mktimegm" (func $js_date_mktimegm (param i32 i32 i32 i32 i32 i32 i64) (result i64)))
  (import "__js_date" "day_name" (func $js_date_day_name (param i32 i32 i32) (result i32)))
  (import "__js_date" "month_name" (func $js_date_month_name (param i32 i32 i32) (result i32)))

  (import "__js" "exit" (func $js_exit (param i32)))
  (import "__js" "signal" (func $js_signal (param i32) (param (ref eq))))

  (import "$__object" "BGl_classzd2nilzd2initz12z12zz__objectz00" (func $BGl_z62classzd2nilzd2initz12z70zz__objectz00 (param (ref $class)) (result (ref eq)))))



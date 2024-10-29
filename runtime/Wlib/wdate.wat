;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wdate.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Mon Oct 21 17:43:39 2024                          */
;*    Last change :  Mon Oct 21 17:47:15 2024 (serrano)                */
;*    Copyright   :  2024 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM dates                                                       */
;*=====================================================================*/

(module $__bigloo_date

   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $date (struct
		  (field $timezone (mut i64))
		  (field $year (mut i32))
		  (field $month (mut i32))
		  (field $yday (mut i32))
		  (field $wday (mut i32))
		  (field $day (mut i32))
		  (field $hour (mut i32))
		  (field $minute (mut i32))
		  (field $second (mut i32))
		  (field $nanosecond (mut i64))
		  (field $is-dst (mut i32))
		  (field $is-gmt (mut i32))
		  (field $time (mut i64))))

   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $date-default-value
     (export "BGL_DATE_DEFAULT_VALUE") (ref $date)
     (struct.new $date
	;; timezone
	(i64.const 0)
	;; year
	(i32.const 0)
	;; month
	(i32.const 0)
	;; yday
	(i32.const 0)
	;; wday
	(i32.const 0)
	;; day
	(i32.const 0)
	;; hour
	(i32.const 0)
	;; minute
	(i32.const 0)
	;; second
	(i32.const 0)
	;; nanosecond
	(i64.const 0)
	;; is-dst
	(i32.const 0)
	;; is-gmt
	(i32.const 0)
	;; time
	(i64.const 0)))

   ;; -----------------------------------------------------------------
   ;; Library functions
   ;; -----------------------------------------------------------------
   
   (export "bgl_current_seconds" (func $bgl_current_seconds))
   (export "bgl_current_milliseconds" (func $bgl_current_milliseconds))
   (export "bgl_current_microseconds" (func $bgl_current_microseconds))
   (export "bgl_current_nanoseconds" (func $bgl_current_nanoseconds))

   )

   
   

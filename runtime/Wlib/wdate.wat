;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wdate.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Mon Oct 21 17:43:39 2024                          */
;*    Last change :  Wed Feb  5 07:54:59 2025 (serrano)                */
;*    Copyright   :  2024-25 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    WASM dates                                                       */
;*=====================================================================*/

(module $__bigloo_date
   
   ;; -----------------------------------------------------------------
   ;; Javascript imports 
   ;; -----------------------------------------------------------------
   
   (import "__js_date" "epoch" (global $epoch externref))
   (import "__js_date" "current_milliseconds" (func $js_current_milliseconds (result f64)))
   (import "__js_date" "mkDate" (func $js_mkdate (param f64) (result externref)))
   (import "__js_date" "mktime" (func $js_mktime (param i32 i32 i32 i32 i32 i32 f64 i32) (result externref)))
   (import "__js_date" "getMilliseconds" (func $js_date_milliseconds (param externref) (result f64)))
   (import "__js_date" "setMilliseconds" (func $js_date_set_milliseconds (param externref) (param f64)))
   (import "__js_date" "getSeconds" (func $js_date_seconds (param externref) (result i32)))
   (import "__js_date" "setSeconds" (func $js_date_set_seconds (param externref) (param i32)))
   (import "__js_date" "getMinutes" (func $js_date_minutes (param externref) (result i32)))
   (import "__js_date" "setMinutes" (func $js_date_set_minutes (param externref) (param i32)))
   (import "__js_date" "getHours" (func $js_date_hours (param externref) (result i32)))
   (import "__js_date" "setHours" (func $js_date_set_hours (param externref) (param i32)))
   (import "__js_date" "getDay" (func $js_date_day (param externref) (result i32)))
   (import "__js_date" "setDay" (func $js_date_set_day (param externref) (param i32)))
   (import "__js_date" "getWday" (func $js_date_wday (param externref) (result i32)))
   (import "__js_date" "getYday" (func $js_date_yday (param externref) (result i32)))
   (import "__js_date" "getMonth" (func $js_date_month (param externref) (result i32)))
   (import "__js_date" "setMonth" (func $js_date_set_month (param externref) (param i32)))
   (import "__js_date" "getYear" (func $js_date_year (param externref) (result i32)))
   (import "__js_date" "setYear" (func $js_date_set_year (param externref) (param i32)))
   (import "__js_date" "getTimezone" (func $js_date_timezone (param externref) (result f64)))
   (import "__js_date" "isDst" (func $js_date_isdst (param externref) (result i32)))
   (import "__js_date" "getTime" (func $js_date_time (param externref) (result f64)))
   (import "__js_date" "secondsToString" (func $js_seconds_to_string (param i64) (param i32) (result i32)))
   (import "__js_date" "secondsToUTCString" (func $js_seconds_to_utc_string (param i64) (param i32) (result i32)))
   (import "__js_date" "day_name" (func $js_date_day_name (param i32 i32 i32) (result i32)))
   (import "__js_date" "month_name" (func $js_date_month_name (param i32 i32 i32) (result i32)))
   
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))


   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (type $date
      (struct
	 (field $dt externref)
	 (field $nsec (mut i64))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $date-default-value
      (export "BGL_DATE_DEFAULT_VALUE") (ref $date)
      (struct.new $date (global.get $epoch) (i64.const 0)))
   
   ;; -----------------------------------------------------------------
   ;; Constructors and predicates
   ;; -----------------------------------------------------------------
   
   (func $BGL_DATEP (export "BGL_DATEP")
      (param $o (ref eq))
      (result i32)
      (ref.test (ref $date) (local.get $o)))
   
   (func $bgl_current_nanoseconds (export "bgl_current_nanoseconds")
      (result i64)
      (i64.mul (call $bgl_current_milliseconds) (i64.const 1000000)))
   
   (func $bgl_current_microseconds (export "bgl_current_microseconds")
      (result i64)
      (i64.mul (call $bgl_current_milliseconds) (i64.const 1000)))

   (func $bgl_current_milliseconds (export "bgl_current_milliseconds")
      (result i64)
      (i64.trunc_f64_s (call $js_current_milliseconds)))

   (func $bgl_current_seconds (export "bgl_current_seconds")
      (result i64)
      (i64.div_s (call $bgl_current_milliseconds) (i64.const 1000)))
   
   (func $bgl_nanoseconds_to_date (export "bgl_nanoseconds_to_date")
      (param $nsec i64)
      (result (ref $date))
      (local $tmp (ref $date))
      (local.set $tmp
	 (call $bgl_milliseconds_to_date
	    (i64.div_s (local.get $nsec) (i64.const 1000000))))
      (struct.set $date $nsec (local.get $tmp)
	 (i64.rem_s (local.get $nsec)
	    (i64.const 1000000)))
      (return (local.get $tmp)))

   (func $bgl_milliseconds_to_date (export "bgl_milliseconds_to_date")
      (param $ms i64)
      (result (ref $date))
      (struct.new $date
	 (call $js_mkdate (f64.convert_i64_s (local.get $ms)))
	 (i64.const 0)))

   (func $bgl_seconds_to_date (export "bgl_seconds_to_date")
      (param $s i64)
      (result (ref $date))
      (return_call $bgl_milliseconds_to_date
	 (i64.mul (local.get $s) (i64.const 1000))))

   (func $bgl_make_date (export "bgl_make_date")
      (param $nsec i64)
      (param $sec i32)
      (param $min i32)
      (param $hour i32)
      (param $day i32)
      (param $month i32)
      (param $year i32)
      (param $timezone i64)
      (param $isgmt i32)
      (param $dst i32)
      (result (ref $date))
      (struct.new $date
	 (call $js_mktime
	    (local.get $year)
	    (local.get $month)
	    (local.get $day)
	    (local.get $hour)
	    (local.get $min)
	    (local.get $sec)
	    (f64.convert_i64_s (i64.div_s (local.get $nsec) (i64.const 1000000)))
	    (local.get $isgmt))
	 (i64.rem_s (local.get $nsec) (i64.const 1000000000))))

   (func $bgl_update_date (export "bgl_update_date")
      (param $dt (ref $date))
      (param $nsec i64)
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
      (call $js_date_set_milliseconds (struct.get $date $dt (local.get $dt))
	 (f64.convert_i64_s (i64.div_s (local.get $nsec) (i64.const 1000000))))
      (call $js_date_set_seconds (struct.get $date $dt (local.get $dt))
	 (local.get $s))
      (call $js_date_set_minutes (struct.get $date $dt (local.get $dt))
	 (local.get $m))
      (call $js_date_set_hours (struct.get $date $dt (local.get $dt))
	 (local.get $h))
      (call $js_date_set_day (struct.get $date $dt (local.get $dt))
	 (local.get $mday))
      (call $js_date_set_month (struct.get $date $dt (local.get $dt))
	 (local.get $mon))
      (call $js_date_set_year (struct.get $date $dt (local.get $dt))
	 (local.get $year))
      (return (local.get $dt)))

   ;; -----------------------------------------------------------------
   ;; Getters
   ;; -----------------------------------------------------------------

   (func $BGL_DATE_NANOSECOND (export "BGL_DATE_NANOSECOND")
      (param $dt (ref $date))
      (result i64)
      (return (struct.get $date $nsec (local.get $dt))))
   
   (func $BGL_DATE_MILLISECOND (export "BGL_DATE_MILLISECOND")
      (param $dt (ref $date))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_date_milliseconds
	    (struct.get $date $dt (local.get $dt)))))
   
   (func $BGL_DATE_SECOND (export "BGL_DATE_SECOND")
      (param $dt (ref $date))
      (result i32)
      (call $js_date_seconds
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_MINUTE (export "BGL_DATE_MINUTE")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_minutes
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_HOUR (export "BGL_DATE_HOUR")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_hours
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_DAY (export "BGL_DATE_DAY")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_day
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_WDAY (export "BGL_DATE_WDAY")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_wday
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_YDAY (export "BGL_DATE_YDAY")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_yday
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_MONTH (export "BGL_DATE_MONTH")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_month
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_YEAR (export "BGL_DATE_YEAR")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_year
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_TIMEZONE (export "BGL_DATE_TIMEZONE")
      (param $dt (ref $date))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_date_timezone
	    (struct.get $date $dt (local.get $dt)))))

   (func $BGL_DATE_ISDST (export "BGL_DATE_ISDST")
      (param $dt (ref $date))
      (result i32)
      (return_call $js_date_isdst
	 (struct.get $date $dt (local.get $dt))))

   (func $BGL_DATE_ISGMT (export "BGL_DATE_ISGMT")
      (param $dt (ref $date))
      (result i32)
      (return (i64.eqz (call $BGL_DATE_TIMEZONE (local.get $dt)))))

   (func $BGL_DATE_TIME (export "BGL_DATE_TIME")
      (param $dt (ref $date))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_date_time
	    (struct.get $date $dt (local.get $dt)))))

  (func $BGL_DATE_UPDATE_MILLISECOND (export "BGL_DATE_UPDATE_MILLISECOND")
    (param $dt (ref $date))
    (param $ms i64)
    (result i64)
    (call $js_date_set_milliseconds (struct.get $date $dt (local.get $dt))
       (f64.convert_i64_s (i64.div_s (local.get $ms) (i64.const 1000000))))
    (local.get $ms))

  (func $BGL_DATE_UPDATE_SECOND (export "BGL_DATE_UPDATE_SECOND")
    (param $dt (ref $date))
    (param $s i64)
    (result i64)
    (call $js_date_set_seconds (struct.get $date $dt (local.get $dt))
       (i32.wrap_i64 (local.get $s)))
    (local.get $s))

  (func $BGL_DATE_UPDATE_MINUTE (export "BGL_DATE_UPDATE_MINUTE")
    (param $dt (ref $date))
    (param $m i64)
    (result i64)
    (call $js_date_set_minutes (struct.get $date $dt (local.get $dt))
       (i32.wrap_i64 (local.get $m)))
    (local.get $m))

  (func $BGL_DATE_UPDATE_TIME (export "BGL_DATE_UPDATE_TIME")
     (param $dt (ref $date))
     (param $h i64)
     (result i64)
     (call $js_date_set_hours (struct.get $date $dt (local.get $dt))
	(i32.wrap_i64 (local.get $h)))
     (local.get $h))

   ;; -----------------------------------------------------------------
   ;; Misc
   ;; -----------------------------------------------------------------
   
   (func $bgl_seconds_to_gmtdate (export "bgl_seconds_to_gmtdate")
      (param $sec i64)
      (result (ref $date))
      (return (struct.new $date (call $js_mkdate (f64.convert_i64_s (i64.mul (local.get $sec) (i64.const 1000)))) (i64.const 0))))

   (func $bgl_milliseconds_to_gmtdate (export "bgl_milliseconds_to_gmtdate")
      (param $ms i64)
      (result (ref $date))
      (return (struct.new $date (call $js_mkdate (f64.convert_i64_s (local.get $ms))) (i64.const 0))))

   (func $bgl_date_to_nanoseconds (export "bgl_date_to_nanoseconds")
      (param $dt (ref $date))
      (result i64)
      (i64.add
	 (i64.mul
	    (i64.trunc_f64_s (call $js_date_time (struct.get $date $dt (local.get $dt))))
	    (i64.const 1000000))
	 (struct.get $date $nsec (local.get $dt))))

   (func $bgl_date_to_milliseconds (export "bgl_date_to_milliseconds")
      (param $dt (ref $date))
      (result i64)
      (i64.trunc_f64_s
	 (call $js_date_time (struct.get $date $dt (local.get $dt)))))
   
   (func $bgl_date_to_seconds (export "bgl_date_to_seconds")
      (param $dt (ref $date))
      (result i64)
      (i64.div_s (i64.trunc_f64_s
		    (call $js_date_time (struct.get $date $dt (local.get $dt))))
	 (i64.const 1000)))

   (func $bgl_seconds_to_string (export "bgl_seconds_to_string")
      (param $sec i64)
      (result (ref $bstring))
      (return_call $load_string (i32.const 128)
	 (call $js_seconds_to_string (local.get $sec) (i32.const 128))))
   
   (func $bgl_seconds_to_utc_string (export "bgl_seconds_to_utc_string")
      (param $sec i64)
      (result (ref $bstring))
      (return_call $load_string (i32.const 128)
	 (call $js_seconds_to_utc_string (local.get $sec) (i32.const 128))))

   ;; -----------------------------------------------------------------
   ;; Names
   ;; -----------------------------------------------------------------
   
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
	 (then (global.set $day_anames
		  (call $make_day_names (i32.const 0 #;(Short format))))))
     (array.get $stringarray
	(global.get $day_anames) (i32.sub (local.get $day) (i32.const 1))))

  (func $bgl_month_name (export "bgl_month_name") 
     (param $month i32) 
     (result (ref $bstring))
     (if (ref.is_null (global.get $month_names))
	 (then (global.set $month_names
		  (call $make_month_names (i32.const 1 #;(Long format))))))
     (array.get $stringarray
	(global.get $month_names) (i32.sub (local.get $month) (i32.const 1))))

  (func $bgl_month_aname (export "bgl_month_aname") 
     (param $month i32) 
     (result (ref $bstring))
     (if (ref.is_null (global.get $month_anames))
	 (then (global.set $month_anames
		  (call $make_month_names (i32.const 0 #;(Short format))))))
     (array.get $stringarray
	(global.get $month_anames) (i32.sub (local.get $month) (i32.const 1))))

   )

   
   

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/date.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  4 10:35:59 2003                          */
;*    Last change :  Tue Sep 29 11:31:08 2009 (serrano)                */
;*    Copyright   :  2003-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The operations on time and date.                                 */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/date.texi@                                */
;*       @node Date@                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __date
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __param
	    __rgc
	    __r5_control_features_6_4
	    	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __foreign
	    __evenv
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_input_6_10_2)
   
   (extern  (macro c-date?::bool (::obj) "BGL_DATEP")
	    (c-date-new::date (::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_make_date")
	    
	    (macro c-date-integer->second::elong (::long) "(long)")
	    (macro c-date-second::int (::date) "BGL_DATE_SECOND")
	    (macro c-date-minute::int (::date) "BGL_DATE_MINUTE")
	    (macro c-date-hour::int (::date) "BGL_DATE_HOUR")
	    (macro c-date-day::int (::date) "BGL_DATE_DAY")
	    (macro c-date-wday::int (::date) "BGL_DATE_WDAY")
	    (macro c-date-yday::int (::date) "BGL_DATE_YDAY")
	    (macro c-date-month::int (::date) "BGL_DATE_MONTH")
	    (macro c-date-year::int (::date) "BGL_DATE_YEAR")
	    (macro c-date-timezone::long (::date) "BGL_DATE_TIMEZONE")
	    (macro c-date-is-dst::int (::date) "BGL_DATE_ISDST")
	    
	    (c-date-day-name::bstring (::int) "bgl_day_name")
	    (c-date-day-aname::bstring (::int) "bgl_day_aname")
	    (c-date-month-name::bstring (::int) "bgl_month_name")
	    (c-date-month-aname::bstring (::int) "bgl_month_aname")
	    
	    (c-date-current-seconds::elong () "bgl_current_seconds")
	    (c-date-current-microseconds::llong () "bgl_current_microseconds")
	    (c-date-from-seconds::date (::elong) "bgl_seconds_to_date")
	    (c-date-to-seconds::elong (::date) "bgl_date_to_seconds")
	    (c-date-seconds-to-string::bstring (::elong) "bgl_seconds_to_string")
	    (c-date-seconds-to-utc-string::bstring (::elong) "bgl_seconds_to_utc_string"))
   
   (java    (class foreign
	       (method static c-date?::bool (::obj) "DATEP")
	       (method static c-date-new::date (::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_make_date")
	       (method static c-date-from-seconds::date (::elong) "bgl_seconds_to_date")
	       (method static c-date-current-seconds::elong () "bgl_current_seconds")
	       (method static c-date-current-microseconds::llong () "bgl_current_microseconds")
	       (method static c-date-to-seconds::elong (::date) "bgl_date_to_seconds")
	       (method static c-date-seconds-to-string::bstring (::elong) "bgl_seconds_to_string")
	       (method static c-date-seconds-to-utc-string::bstring (::elong) "bgl_seconds_to_utc_string")
	       
	       (method static c-date-integer->second::elong (::long) "bgl_integer_to_seconds")
	       (method static c-date-second::int (::date) "BGL_DATE_SECOND")
	       (method static c-date-minute::int (::date) "BGL_DATE_MINUTE")
	       (method static c-date-hour::int (::date) "BGL_DATE_HOUR")
	       (method static c-date-day::int (::date) "BGL_DATE_DAY")
	       (method static c-date-wday::int (::date) "BGL_DATE_WDAY")
	       (method static c-date-yday::int (::date) "BGL_DATE_YDAY")
	       (method static c-date-month::int (::date) "BGL_DATE_MONTH")
	       (method static c-date-year::int (::date) "BGL_DATE_YEAR")
	       (method static c-date-timezone::long (::date) "BGL_DATE_TIMEZONE")
	       (method static c-date-is-dst::int (::date) "BGL_DATE_ISDST")
	       
	       (method static c-date-day-name::bstring (::int) "bgl_day_name")
	       (method static c-date-day-aname::bstring (::int) "bgl_day_aname")
	       (method static c-date-month-name::bstring (::int) "bgl_month_name")
	       (method static c-date-month-aname::bstring (::int) "bgl_month_aname")))
   
   (export  (inline date?::bool ::obj)
	    (make-date #!key
		       (nsec 0) (sec 1) (min 1) (hour 1)
		       (day 1) (month 1) (year 1970)
		       timezone (dst -1))
	    (date-copy date #!key sec min hour day month year)
	    
	    (inline integer->second::elong ::long)
	    
	    (inline date-nanosecond::elong ::date)
	    (inline date-second::int ::date)
	    (inline date-minute::int ::date)
	    (inline date-hour::int ::date)
	    (inline date-day::int ::date)
	    (inline date-wday::int ::date)
	    (inline date-week-day::int ::date)
	    (inline date-yday::int ::date)
	    (inline date-year-day::int ::date)
	    (inline date-month::int ::date)
	    (inline date-year::int ::date)
	    (inline date-timezone::long ::date)
	    (inline date-zone-offset::long ::date)
	    (inline date-is-dst::int ::date)
	    
	    (inline current-seconds::elong)
	    (inline current-microseconds::llong)
	    (inline current-date::date)
	    (inline seconds->date::date ::elong)
	    (inline date->seconds::elong ::date)
	    (inline date->string::bstring ::date)
	    (inline date->utc-string::bstring ::date)
	    (inline seconds->string::bstring ::elong)
	    (inline seconds->utc-string::bstring ::elong)
	    
	    (inline day-seconds::elong)
	    (day-name::bstring ::int)
	    (day-aname::bstring ::int)
	    (month-name::bstring ::int)
	    (month-aname::bstring ::int)
	    (date-month-length::int ::date)
	    (inline leap-year?::bool ::int)
	    
	    (rfc2822-date->date::date ::bstring)
	    (rfc2822-parse-date::date ::input-port)
	    
	    (date->rfc2822-date::bstring ::date))

   (pragma  (date? (predicate-of date) no-cfa-top nesting)
	    (c-date? (predicate-of date) no-cfa-top nesting)

	    (c-date-integer->second args-safe)
	    (c-date-second args-safe)
	    (c-date-minute args-safe)
	    (c-date-hour args-safe)
	    (c-date-day args-safe)
	    (c-date-wday args-safe)
	    (c-date-yday args-safe)
	    (c-date-month args-safe)
	    (c-date-year args-safe)
	    (c-date-timezone args-safe)))

;*---------------------------------------------------------------------*/
;*    date? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (date? obj)
   (c-date? obj))

;*---------------------------------------------------------------------*/
;*    make-date ...                                                    */
;*    -------------------------------------------------------------    */
;*    Warning: for SRFI-19 compatibility, nanoseconds are an           */
;*    argument, but its value is currently discarded!                  */
;*---------------------------------------------------------------------*/
(define (make-date #!key
                   (nsec 0)
		   (sec 1) (min 1) (hour 1)
		   (day 1) (month 1) (year 1970)
		   timezone (dst -1))
   (if (integer? timezone)
       (c-date-new sec min hour day month year timezone #t dst)
       (c-date-new sec min hour day month year 0 #f dst)))

;*---------------------------------------------------------------------*/
;*    date-copy ...                                                    */
;*---------------------------------------------------------------------*/
(define (date-copy date #!key sec min hour day month year)
   (c-date-new (or sec (date-second date))
	       (or min (date-minute date))
	       (or hour (date-hour date))
	       (or day (date-day date))
	       (or month (date-month date))
	       (or year (date-year date))
	       0
	       #f
	       (date-is-dst date)))
      
;*---------------------------------------------------------------------*/
;*    integer->second ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (integer->second i)
   (c-date-integer->second i))

;*---------------------------------------------------------------------*/
;*    date-nanosecond ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (date-nanosecond d::date)
   #e0)

;*---------------------------------------------------------------------*/
;*    date-second ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-second d::date)
   (c-date-second d))

;*---------------------------------------------------------------------*/
;*    date-minute ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-minute d::date)
   (c-date-minute d))

;*---------------------------------------------------------------------*/
;*    date-hour ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-hour d::date)
   (c-date-hour d))

;*---------------------------------------------------------------------*/
;*    date-day ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (date-day d::date)
   (c-date-day d))

;*---------------------------------------------------------------------*/
;*    date-week-day ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-week-day d::date)
   (c-date-wday d))

;*---------------------------------------------------------------------*/
;*    date-wday ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-wday d::date)
   (c-date-wday d))

;*---------------------------------------------------------------------*/
;*    date-year-day ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-year-day d::date)
   (c-date-yday d))

;*---------------------------------------------------------------------*/
;*    date-yday ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-yday d::date)
   (c-date-yday d))

;*---------------------------------------------------------------------*/
;*    date-month ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (date-month d::date)
   (c-date-month d))

;*---------------------------------------------------------------------*/
;*    date-year ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-year d::date)
   (c-date-year d))

;*---------------------------------------------------------------------*/
;*    date-timezone ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-timezone d::date)
   (c-date-timezone d))

;*---------------------------------------------------------------------*/
;*    date-zone-offset ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (date-zone-offset d::date)
   (*fx 3600 (c-date-timezone d)))

;*---------------------------------------------------------------------*/
;*    date-is-dst ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-is-dst d::date)
   (c-date-is-dst d))

;*---------------------------------------------------------------------*/
;*    current-seconds ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-seconds)
   (c-date-current-seconds))

;*---------------------------------------------------------------------*/
;*    current-microseconds ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (current-microseconds)
   (c-date-current-microseconds))

;*---------------------------------------------------------------------*/
;*    current-date ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (current-date)
   (c-date-from-seconds (c-date-current-seconds)))

;*---------------------------------------------------------------------*/
;*    seconds->date ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (seconds->date elong)
   (c-date-from-seconds elong))

;*---------------------------------------------------------------------*/
;*    date->seconds ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date->seconds date)
   (c-date-to-seconds date))

;*---------------------------------------------------------------------*/
;*    date->string ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (date->string date)
   (seconds->string (date->seconds date)))

;*---------------------------------------------------------------------*/
;*    date->utc-string ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (date->utc-string date)
   (seconds->utc-string (date->seconds date)))

;*---------------------------------------------------------------------*/
;*    seconds->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (seconds->string sec)
   (c-date-seconds-to-string sec))

;*---------------------------------------------------------------------*/
;*    seconds->utc-string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (seconds->utc-string sec)
   (c-date-seconds-to-utc-string sec))

;*---------------------------------------------------------------------*/
;*    day-seconds ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (day-seconds)
   #e86400)

;*---------------------------------------------------------------------*/
;*    day-name ...                                                     */
;*---------------------------------------------------------------------*/
(define (day-name day)
   (cond
      ((<fx day 1)
       (error 'day-name "Illegal day number" day))
      ((>fx day 7)
       (c-date-day-name (+fx 1 (remainderfx day 7))))
      (else
       (c-date-day-name day))))

;*---------------------------------------------------------------------*/
;*    day-aname ...                                                    */
;*---------------------------------------------------------------------*/
(define (day-aname day)
   (cond
      ((<fx day 1)
       (error 'day-aname "Illegal day number" day))
      ((>fx day 7)
       (c-date-day-aname (+fx 1 (remainderfx day 7))))
      (else
       (c-date-day-aname day))))

;*---------------------------------------------------------------------*/
;*    month-name ...                                                   */
;*---------------------------------------------------------------------*/
(define (month-name month)
   (cond
      ((<fx month 1)
       (error 'month-aname "Illegal month number" month))
      ((>fx month 12)
       (c-date-month-name (+fx 1 (remainderfx month 12))))
      (else
       (c-date-month-name month))))

;*---------------------------------------------------------------------*/
;*    month-aname ...                                                  */
;*---------------------------------------------------------------------*/
(define (month-aname month)
   (cond
      ((<fx month 1)
       (error 'month-aname "Illegal month number" month))
      ((>fx month 12)
       (c-date-month-aname (+fx 1 (remainderfx month 12))))
      (else
       (c-date-month-aname month))))

;*---------------------------------------------------------------------*/
;*    *month-lengths* ...                                              */
;*---------------------------------------------------------------------*/
(define *month-lengths* `#(31 28 31 30 31 30 31 31 30 31 30 31))

;*---------------------------------------------------------------------*/
;*    date-month-length ...                                            */
;*---------------------------------------------------------------------*/
(define (date-month-length d)
   (let ((m (date-month d)))
      (if (=fx m 2)
	  (if (leap-year? (date-year d)) 29 28)
	  (vector-ref *month-lengths* (-fx m 1)))))

;*---------------------------------------------------------------------*/
;*    leap-year? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (leap-year? year)
   (and (=fx (remainderfx year 4) 0)
	(or (not (=fx (remainderfx year 100) 0))
	    (=fx (remainderfx year 400) 0))))

;*---------------------------------------------------------------------*/
;*    rfc2822-date->date ...                                           */
;*---------------------------------------------------------------------*/
(define (rfc2822-date->date string)
   (let ((port (open-input-string string)))
      (unwind-protect
	 (rfc2822-parse-date port)
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    rfc2822-parse-date ...                                           */
;*---------------------------------------------------------------------*/
(define (rfc2822-parse-date ip::input-port)
   (read/rp day-of-week-grammar ip))

;*---------------------------------------------------------------------*/
;*    date->rfc2822-date ...                                           */
;*---------------------------------------------------------------------*/
(define (date->rfc2822-date date)
   
   (define (2digits num)
      (if (<fx num 10)
          (string #\0 (integer->char (+fx (char->integer #\0) num)))
          (integer->string num)))
   
   (define (date/timezone date timezone)
      (let* ((tz (/fx timezone 60))
	     (h (/fx tz 60))
	     (m (remainderfx tz 60)))
	 (format "~a, ~a ~a ~a ~a:~a:~a ~a~a~a"
		 (day-aname (date-wday date))
		 (date-day date)
		 (month-aname (date-month date))
		 (date-year date)
		 (2digits (date-hour date))
		 (2digits (date-minute date))
		 (2digits (date-second date))
		 (if (<fx tz 0) "+" "-")
		 (2digits (absfx h))
		 (2digits m))))
   
   (if (>fx (date-is-dst date) 0)
       (let* ((dateu (make-date :sec (date-second date)
			:min (date-minute date)
			:hour (date-hour date)
			:day (date-day date)
			:month (date-month date)
			:year (date-year date)
			:timezone 0
			:dst 0))
	      (secu (date->seconds dateu))
	      (sec (date->seconds date)))
	  (date/timezone date (elong->fixnum (-elong sec secu))))
       (date/timezone date (date-timezone date))))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc message obj port)
   (raise
    (instantiate::&io-parse-error
       (proc proc)
       (msg message)
       (obj (if (char? obj)
		(string-append "{" (string obj) "}" (read-line port))
		obj))
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    day-of-week-grammar ...                                          */
;*---------------------------------------------------------------------*/
(define day-of-week-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((: (in "MTWFS") (= 2 (in "onuedhriat")) "," FWS)
       (let* ((day (read/rp fixnum-grammar (the-port)))
	      (month (read/rp month-grammar (the-port)))
	      (year (read/rp fixnum-grammar (the-port))))
	  (multiple-value-bind (hour minute second)
	     (read/rp time-grammar (the-port))
	     (let ((zone (read/rp zone-grammar (the-port))))
		(make-date :sec second
		   :min minute
		   :hour hour
		   :month month
		   :year (if (<fx year 100) (+fx year 2000) year)
		   :day day
		   :timezone zone
		   :dst 0)))))
      ((+ digit)
       (let* ((day (the-fixnum))
	      (month (read/rp month-grammar (the-port)))
	      (year (read/rp fixnum-grammar (the-port))))
	  (multiple-value-bind (hour minute second)
	     (read/rp time-grammar (the-port))
	     (let ((zone (read/rp zone-grammar (the-port))))
		(make-date :sec second
		   :min minute
		   :hour hour
		   :month month
		   :year (if (<fx year 100) (+fx year 2000) year)
		   :day day
		   :timezone zone
		   :dst 0)))))
      (else
       (parse-error 'rfc2822-parse-date
		    "Illegal day of week"
		    (the-failure) (the-port)))))

;*---------------------------------------------------------------------*/
;*    fixnum-grammar ...                                               */
;*---------------------------------------------------------------------*/
(define fixnum-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((+ digit)
       (the-fixnum))
      (else
       (parse-error 'rfc2822-parse-date
		    "Illegal integer"
		    (the-failure) (the-port)))))
      
;*---------------------------------------------------------------------*/
;*    month-grammar ...                                                */
;*---------------------------------------------------------------------*/
(define month-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((: (in "JFMASOND") (= 2 (in "anebrpyulgctov")))
       (case (the-symbol)
	  ((Jan) 1)
	  ((Feb) 2)
	  ((Mar) 3)
	  ((Apr) 4)
	  ((May) 5)
	  ((Jun) 6)
	  ((Jul) 7)
	  ((Aug) 8)
	  ((Sep) 9)
	  ((Oct) 10)
	  ((Nov) 11)
	  ((Dec) 12)
	  (else (parse-error 'rfc2822-parse-date
			     "Illegal month"
			     (the-string) (the-port)))))
      (else
       (parse-error 'rfc2822-parse-date "Illegal month"
		    (the-failure) (the-port)))))

;*---------------------------------------------------------------------*/
;*    the-digit ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (the-digit n)
   `(-fx (the-byte-ref ,n) (char->integer #\0)))

;*---------------------------------------------------------------------*/
;*    time-grammar ...                                                 */
;*---------------------------------------------------------------------*/
(define time-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((: digit #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b3 (the-digit 2))
	     (b4 (the-digit 3)))
	  (values b1
		  (+fx (*fx 10 b3) b4)
		  0)))
      ((: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 1))
	     (b3 (the-digit 3))
	     (b4 (the-digit 4)))
	  (values (+fx (*fx 10 b1) b2)
		  (+fx (*fx 10 b3) b4)
		  0)))
      ((: (= 2 digit) #\: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 1))
	     (b3 (the-digit 3))
	     (b4 (the-digit 4))
	     (b5 (the-digit 6))
	     (b6 (the-digit 7)))
	  (values (+fx (*fx 10 b1) b2)
		  (+fx (*fx 10 b3) b4)
		  (+fx (*fx 10 b5) b6) )))
      ((: digit #\: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b3 (the-digit 2))
	     (b4 (the-digit 3))
	     (b5 (the-digit 5))
	     (b6 (the-digit 6)))
	  (values b1
		  (+fx (*fx 10 b3) b4)
		  (+fx (*fx 10 b5) b6) )))
      ((: #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 2)))
	  (values 0
		  0
		  (+fx (*fx 10 13) b2))))
      (else
       (parse-error 'rfc2822-parse-date "Illegal time"
		    (the-failure) (the-port)))))

;*---------------------------------------------------------------------*/
;*    *time-zones* ...                                                 */
;*---------------------------------------------------------------------*/
(define *time-zones*
   '((EDT . -4)
     (EST . -5)
     (CDT . -5)
     (CST . -6)
     (MDT . -6)
     (MST . -7)
     (PDT . -7)
     (PST . -8)
     (CEST . +1)
     (UT . 0)))

;*---------------------------------------------------------------------*/
;*    zone-grammar ...                                                 */
;*---------------------------------------------------------------------*/
(define zone-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((: (in "+-") (= 4 digit))
       (let ((h (+fx (*fx 10 (the-digit 1)) (the-digit 2)))
	     (m (+fx (*fx 10 (the-digit 3)) (the-digit 4))))
	  (if (=fx (the-byte-ref 0) (char->integer #\+))
	      (negfx (*fx 60 (+fx (*fx h 60) m)))
	      (*fx 60 (+fx (*fx h 60) m)))))
      ((: (in "+-") (= 3 digit))
       (let ((h (the-digit 1))
	     (m (+fx (*fx 10 (the-digit 2)) (the-digit 3))))
	  (if (=fx (the-byte-ref 0) (char->integer #\+))
	      (negfx (*fx 60 (+fx (*fx h 60) m)))
	      (*fx 60 (+fx (*fx h 60) m)))))
      ((: "--" (= 3 digit))
       ;; this appears to be a frequent error in date format!
       (let ((h (the-digit 2))
	     (m (+fx (*fx 10 (the-digit 3)) (the-digit 4))))
	  (*fx 60 (+fx (*fx h 60) m))))
      ((>= 2 alpha)
       ;; time zone rough implementation
       (let ((c (assq (the-symbol) *time-zones*)))
	  (if (pair? c)
	      (*fx 3600 (cdr c))
	      0)))
      (else
       (parse-error 'rfc2822-parse-date "Illegal zone"
		    (the-failure) (the-port)))))

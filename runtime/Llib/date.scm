;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/date.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  4 10:35:59 2003                          */
;*    Last change :  Thu Mar 26 14:29:39 2020 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
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
	    __bit
	    	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
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
	    ($date-new::date (::llong ::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_make_date")
	    ($date-update::date (::date ::llong ::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_update_date")
	    ($date->gmtdate!::date (::date) "bgl_date_to_gmtdate")

	    (macro $date-integer->second::elong (::long) "(long)")
	    (macro $date-nanosecond::llong (::date) "BGL_DATE_NANOSECOND")
	    (macro $date-millisecond::long (::date) "BGL_DATE_MILLISECOND")
	    (macro $date-second::int (::date) "BGL_DATE_SECOND")
	    (macro $date-minute::int (::date) "BGL_DATE_MINUTE")
	    (macro $date-hour::int (::date) "BGL_DATE_HOUR")
	    (macro $date-day::int (::date) "BGL_DATE_DAY")
	    (macro $date-wday::int (::date) "BGL_DATE_WDAY")
	    (macro $date-yday::int (::date) "BGL_DATE_YDAY")
	    (macro $date-month::int (::date) "BGL_DATE_MONTH")
	    (macro $date-year::int (::date) "BGL_DATE_YEAR")
	    (macro $date-timezone::long (::date) "BGL_DATE_TIMEZONE")
	    (macro $date-is-dst::int (::date) "BGL_DATE_ISDST")
	    (macro $date-is-gmt::bool (::date) "BGL_DATE_ISGMT")
	    (macro $date-time::long (::date) "BGL_DATE_TIME")

	    (macro $date-update-millisecond::long (::date ::long) "BGL_DATE_UPDATE_MILLISECOND")
	    (macro $date-update-second::long (::date ::long) "BGL_DATE_UPDATE_SECOND")
	    (macro $date-update-minute::long (::date ::long) "BGL_DATE_UPDATE_MINUTE")
	    (macro $date-update-time::long (::date ::long) "BGL_DATE_UPDATE_TIME")
	    
	    ($date-day-name::bstring (::int) "bgl_day_name")
	    ($date-day-aname::bstring (::int) "bgl_day_aname")
	    ($date-month-name::bstring (::int) "bgl_month_name")
	    ($date-month-aname::bstring (::int) "bgl_month_aname")
	    
	    ($date-current-seconds::elong () "bgl_current_seconds")
	    ($date-current-milliseconds::llong () "bgl_current_milliseconds")
	    ($date-current-microseconds::llong () "bgl_current_microseconds")
	    ($date-current-nanoseconds::llong () "bgl_current_nanoseconds")
	    ($date-from-seconds::date (::elong) "bgl_seconds_to_date")
	    ($date-from-seconds-gmt::date (::elong) "bgl_seconds_to_gmtdate")
	    ($date-from-milliseconds-gmt::date (::llong) "bgl_milliseconds_to_gmtdate")
	    ($date-from-nanoseconds::date (::llong) "bgl_nanoseconds_to_date")
	    ($date-from-milliseconds::date (::llong) "bgl_milliseconds_to_date")
	    ($date-to-seconds::elong (::date) "bgl_date_to_seconds")
	    ($date-to-nanoseconds::llong (::date) "bgl_date_to_nanoseconds")
	    ($date-to-milliseconds::llong (::date) "bgl_date_to_milliseconds")
	    ($date-seconds-to-string::bstring (::elong) "bgl_seconds_to_string")
	    ($date-seconds-to-utc-string::bstring (::elong) "bgl_seconds_to_utc_string"))
   
   (java    (class foreign
	       (method static c-date?::bool (::obj) "DATEP")
	       (method static $date-new::date (::llong ::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_make_date")
	       (method static $date-update::date (::date ::llong ::int ::int ::int ::int ::int ::int ::long ::bool ::int) "bgl_update_date")
	       (method static $date-from-seconds::date (::elong) "bgl_seconds_to_date")
	       (method static $date-from-seconds-gmt::date (::elong) "bgl_seconds_to_gmtdate")
	       (method static $date-from-milliseconds-gmt::date (::llong) "bgl_miliseconds_to_gmtdate")
	       (method static $date-from-nanoseconds::date (::llong) "bgl_nanoseconds_to_date")
	       (method static $date-from-milliseconds::date (::llong) "bgl_milliseconds_to_date")
	       (method static $date-current-seconds::elong () "bgl_current_seconds")
	       (method static $date-current-milliseconds::llong () "bgl_current_milliseconds")
	       (method static $date-current-microseconds::llong () "bgl_current_microseconds")
	       (method static $date-current-nanoseconds::llong () "bgl_current_nanoseconds")
	       (method static $date-to-seconds::elong (::date) "bgl_date_to_seconds")
	       (method static $date-to-nanoseconds::llong (::date) "bgl_date_to_nanoseconds")
	       (method static $date-to-milliseconds::llong (::date) "bgl_date_to_milliseconds")
	       (method static $date-seconds-to-string::bstring (::elong) "bgl_seconds_to_string")
	       (method static $date-seconds-to-utc-string::bstring (::elong) "bgl_seconds_to_utc_string")
	       
	       (method static $date-integer->second::elong (::long) "bgl_integer_to_seconds")
	       (method static $date-nanosecond::llong (::date) "BGL_DATE_NANOSECOND")
	       (method static $date-millisecond::llong (::date) "BGL_DATE_MILLISECOND")
	       (method static $date-second::int (::date) "BGL_DATE_SECOND")
	       (method static $date-minute::int (::date) "BGL_DATE_MINUTE")
	       (method static $date-hour::int (::date) "BGL_DATE_HOUR")
	       (method static $date-day::int (::date) "BGL_DATE_DAY")
	       (method static $date-wday::int (::date) "BGL_DATE_WDAY")
	       (method static $date-yday::int (::date) "BGL_DATE_YDAY")
	       (method static $date-month::int (::date) "BGL_DATE_MONTH")
	       (method static $date-year::int (::date) "BGL_DATE_YEAR")
	       (method static $date-timezone::long (::date) "BGL_DATE_TIMEZONE")
	       (method static $date-is-dst::int (::date) "BGL_DATE_ISDST")
	       (method static $date-is-gmt::bool (::date) "BGL_DATE_ISGMT")
	       
	       (method static $date-day-name::bstring (::int) "bgl_day_name")
	       (method static $date-day-aname::bstring (::int) "bgl_day_aname")
	       (method static $date-month-name::bstring (::int) "bgl_month_name")
	       (method static $date-month-aname::bstring (::int) "bgl_month_aname")))
   
   (export  (inline date?::bool ::obj)
	    (make-date #!key
		       (nsec #l0) (sec 0) (min 0) (hour 0)
		       (day 1) (month 1) (year 1970)
		       timezone (dst -1))
	    (date-copy date::date #!key nsec sec min hour day month year timezone isdst)
	    (date-update! date::date #!key nsec sec min hour day month year)
	    (inline date->gmtdate!::date ::date)
	    (date-update-millisecond! ::date ::long)
	    (date-update-second! ::date ::long)
	    (date-update-minute! ::date ::long)
	    
	    (inline integer->second::elong ::long)
	    
	    (inline date-nanosecond::llong ::date)
	    (inline date-millisecond::llong ::date)
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
	    (inline current-milliseconds::llong)
	    (inline current-microseconds::llong)
	    (inline current-nanoseconds::llong)
	    (inline current-date::date)
	    (inline seconds->date::date ::elong)
	    (inline seconds->gmtdate::date ::elong)
	    (inline milliseconds->gmtdate::date ::llong)
	    (inline nanoseconds->date::date ::llong)
	    (inline milliseconds->date::date ::llong)
	    (inline date->seconds::elong ::date)
	    (inline date->nanoseconds::llong ::date)
	    (inline date->milliseconds::llong ::date)
	    (date->string::bstring ::date)
	    (date->utc-string::bstring ::date)
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
	    
	    (iso8601-date->date::date ::bstring)
	    (iso8601-parse-date::date ::input-port)
	    
	    (date->rfc2822-date::bstring ::date)
            (date->iso8601-date::bstring ::date))

   (pragma  (date? (predicate-of date) no-cfa-top nesting)
	    (c-date? (predicate-of date) no-cfa-top nesting)

	    ($date-integer->second args-safe)
	    ($date-second args-safe)
	    ($date-minute args-safe)
	    ($date-hour args-safe)
	    ($date-day args-safe)
	    ($date-wday args-safe)
	    ($date-yday args-safe)
	    ($date-month args-safe)
	    ($date-year args-safe)
	    ($date-timezone args-safe)))

;*---------------------------------------------------------------------*/
;*    date? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (date? obj)
   (c-date? obj))

;*---------------------------------------------------------------------*/
;*    make-date ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-date #!key
	   (nsec #l0)
	   (sec 0) (min 0) (hour 0)
	   (day 1) (month 1) (year 1970)
	   timezone (dst -1))
   (if (integer? timezone)
       ($date-new nsec sec min hour day month year timezone #t dst)
       ($date-new nsec sec min hour day month year 0 #f dst)))

;*---------------------------------------------------------------------*/
;*    date-copy ...                                                    */
;*---------------------------------------------------------------------*/
(define (date-copy date #!key nsec sec min hour day month year timezone isdst)
   ($date-new
      (or nsec (date-nanosecond date))
      (or sec (date-second date))
      (or min (date-minute date))
      (or hour (date-hour date))
      (or day (date-day date))
      (or month (date-month date))
      (or year (date-year date))
      (date-timezone date)
      ($date-is-gmt date)
      (or isdst -1)))

;*---------------------------------------------------------------------*/
;*    date-update! ...                                                 */
;*---------------------------------------------------------------------*/
(define (date-update! date #!key nsec sec min hour day month year)
   ($date-update date
      (or nsec (date-nanosecond date))
      (or sec (date-second date))
      (or min (date-minute date))
      (or hour (date-hour date))
      (or day (date-day date))
      (or month (date-month date))
      (or year (date-year date))
      (date-timezone date)
      ($date-is-gmt date)
      -1))

;*---------------------------------------------------------------------*/
;*    date->gmtdate! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (date->gmtdate! date)
   (cond-expand
      (bigloo-c
       (let ((gmtdate ($date->gmtdate! date)))
	  gmtdate))
      (else
       (error "date->gmtdate!" "not supported by this backend" date))))

;*---------------------------------------------------------------------*/
;*    date-update-millisecond! ...                                     */
;*---------------------------------------------------------------------*/
(define (date-update-millisecond! date ms)
   (cond-expand
      (bigloo-c
       (if (and (>=fx ms 0) (<fx ms 1000))
	   (let ((osec (date-second date)))
	      ($date-update-millisecond date ms)
	      date)
	   (date-update! date :nsec (*llong #l1000000 (fixnum->llong ms)))))
      (else
       (date-update! date :nsec (*llong #l1000000 (fixnum->llong ms))))))
   
;*---------------------------------------------------------------------*/
;*    date-update-second! ...                                          */
;*---------------------------------------------------------------------*/
(define (date-update-second! date sec)
   (cond-expand
      (bigloo-c
       (if (and (>=fx sec 0) (<fx sec 60))
	   (let ((osec (date-second date)))
	      ($date-update-second date sec)
	      ($date-update-time date (+fx ($date-time date) (-fx sec osec)))
	      date)
	   (date-update! date :sec sec)))
      (else
       (date-update! date :sec sec))))
   
;*---------------------------------------------------------------------*/
;*    date-update-minute! ...                                          */
;*---------------------------------------------------------------------*/
(define (date-update-minute! date min)
   (cond-expand
      (bigloo-c
       (if (and (>=fx min 0) (<fx min 60))
	   (let ((omin (date-minute date)))
	      ($date-update-minute date min)
	      ($date-update-time date
		 (+fx ($date-time date) (*fx 60 (-fx min omin))))
	      date)
	   (date-update! date :min min)))
      (else
       (date-update! date :min min))))
   
;*---------------------------------------------------------------------*/
;*    integer->second ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (integer->second i)
   ($date-integer->second i))

;*---------------------------------------------------------------------*/
;*    date-nanosecond ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (date-nanosecond d::date)
   ($date-nanosecond d))

;*---------------------------------------------------------------------*/
;*    date-millisecond ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (date-millisecond d::date)
   ($date-millisecond d))

;*---------------------------------------------------------------------*/
;*    date-second ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-second d::date)
   ($date-second d))

;*---------------------------------------------------------------------*/
;*    date-minute ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-minute d::date)
   ($date-minute d))

;*---------------------------------------------------------------------*/
;*    date-hour ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-hour d::date)
   ($date-hour d))

;*---------------------------------------------------------------------*/
;*    date-day ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (date-day d::date)
   ($date-day d))

;*---------------------------------------------------------------------*/
;*    date-week-day ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-week-day d::date)
   ($date-wday d))

;*---------------------------------------------------------------------*/
;*    date-wday ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-wday d::date)
   ($date-wday d))

;*---------------------------------------------------------------------*/
;*    date-year-day ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-year-day d::date)
   ($date-yday d))

;*---------------------------------------------------------------------*/
;*    date-yday ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-yday d::date)
   ($date-yday d))

;*---------------------------------------------------------------------*/
;*    date-month ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (date-month d::date)
   ($date-month d))

;*---------------------------------------------------------------------*/
;*    date-year ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (date-year d::date)
   ($date-year d))

;*---------------------------------------------------------------------*/
;*    date-timezone ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date-timezone d::date)
   ($date-timezone d))

;*---------------------------------------------------------------------*/
;*    date-zone-offset ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (date-zone-offset d::date)
   (*fx 3600 ($date-timezone d)))

;*---------------------------------------------------------------------*/
;*    date-is-dst ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (date-is-dst d::date)
   ($date-is-dst d))

;*---------------------------------------------------------------------*/
;*    current-seconds ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (current-seconds)
   ($date-current-seconds))

;*---------------------------------------------------------------------*/
;*    current-milliseconds ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (current-milliseconds)
   ($date-current-milliseconds))

;*---------------------------------------------------------------------*/
;*    current-microseconds ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (current-microseconds)
   ($date-current-microseconds))

;*---------------------------------------------------------------------*/
;*    current-nanoseconds ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (current-nanoseconds)
   ($date-current-nanoseconds))

;*---------------------------------------------------------------------*/
;*    current-date ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (current-date)
   ($date-from-nanoseconds ($date-current-nanoseconds)))

;*---------------------------------------------------------------------*/
;*    seconds->date ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (seconds->date elong)
   ($date-from-seconds elong))

;*---------------------------------------------------------------------*/
;*    seconds->gmtdate ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (seconds->gmtdate elong)
   ($date-from-seconds-gmt elong))

;*---------------------------------------------------------------------*/
;*    milliseconds->gmtdate ...                                        */
;*---------------------------------------------------------------------*/
(define-inline (milliseconds->gmtdate llong)
   ($date-from-milliseconds-gmt llong))

;*---------------------------------------------------------------------*/
;*    nanoseconds->date ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (nanoseconds->date llong)
   ($date-from-nanoseconds llong))

;*---------------------------------------------------------------------*/
;*    milliseconds->date ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (milliseconds->date llong)
   ($date-from-milliseconds llong))

;*---------------------------------------------------------------------*/
;*    date->seconds ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (date->seconds date)
   ($date-to-seconds date))

;*---------------------------------------------------------------------*/
;*    date->nanoseconds ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (date->nanoseconds date)
   ($date-to-nanoseconds date))

;*---------------------------------------------------------------------*/
;*    date->milliseconds ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (date->milliseconds date)
   ($date-to-milliseconds date))

;*---------------------------------------------------------------------*/
;*    date->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (date->string date)
   (date->rfc2822-date date))

;*---------------------------------------------------------------------*/
;*    blit-digit! ...                                                  */
;*---------------------------------------------------------------------*/
(define (blit-digit! buf w digit)
   (string-set! buf w (integer->char (+fx (char->integer #\0) digit))))

;*---------------------------------------------------------------------*/
;*    blit-int2! ...                                                   */
;*---------------------------------------------------------------------*/
(define (blit-int2! buf w int)
   (if (<fx int 10)
       (begin
	  (string-set! buf w #\0)
	  (blit-digit! buf (+fx w 1) int))
       (begin
	  (blit-digit! buf w (/fx int 10))
	  (blit-digit! buf (+fx w 1) (modulofx int 10))))
   2)

;*---------------------------------------------------------------------*/
;*    blit-int! ...                                                    */
;*---------------------------------------------------------------------*/
(define (blit-int! buf w int)
   (cond
      ((<fx int 10)
       (blit-digit! buf w int)
       1)
      ((<fx int 100)
       (blit-digit! buf w (/fx int 10))
       (blit-digit! buf (+fx w 1) (modulofx int 10))
       2)
      ((<fx int 1000)
       (blit-digit! buf w (/fx int 100))
       (let ((r (modulofx int 100)))
	  (blit-digit! buf (+fx w 1) (/fx r 10))
	  (blit-digit! buf (+fx w 2) (modulofx r 10))
	  3))
      (else
       (blit-digit! buf w (/fx int 1000))
       (let ((r (modulofx int 1000)))
	  (blit-digit! buf (+fx w 1) (/fx r 100))
	  (let ((r (modulofx int 100)))
	     (blit-digit! buf (+fx w 2) (/fx r 10))
	     (blit-digit! buf (+fx w 3) (modulofx r 10))
	     4)))))

;*---------------------------------------------------------------------*/
;*    blit-buf! ...                                                    */
;*---------------------------------------------------------------------*/
(define (blit-buf! tgt w str)
   (let ((l (string-length str)))
      (blit-string! str 0 tgt w l)
      l))

;*---------------------------------------------------------------------*/
;*    date->utc-string ...                                             */
;*---------------------------------------------------------------------*/
(define (date->utc-string date)

   (define (utc-string date)
      (let ((buf (make-string 29 #\space))
	    (w 0))
;*       (format "~a, ~a ~a ~a ~2,0d:~2,0d:~2,0d GMT"                  */
;* 	 (day-aname (date-wday date))                                  */
;* 	 (date-day date)                                               */
;* 	 (month-aname (date-month date))                               */
;* 	 (date-year date)                                              */
;* 	 (date-hour date)                                              */
;* 	 (date-minute date)                                            */
;* 	 (date-second date))                                           */
	 ;; date-aname
	 (set! w (+fx w (blit-buf! buf w (day-aname (date-wday date)))))
	 (string-set! buf w #\,)
	 (set! w (+fx w 2))
	 ;; date-day
	 (set! w (+fx w (blit-int! buf w (date-day date))))
	 (set! w (+fx w 1))
	 ;; montn-aname
	 (set! w (+fx w (blit-buf! buf w (month-aname (date-month date)))))
	 (set! w (+fx w 1))
	 ;; date-year
	 (set! w (+fx w (blit-int! buf w (date-year date))))
	 (set! w (+fx w 1))
	 ;; date-hour
	 (set! w (+fx w (blit-int2! buf w (date-hour date))))
	 (string-set! buf w #\:)
	 (set! w (+fx w 1))
	 ;; date-minute
	 (set! w (+fx w (blit-int2! buf w (date-minute date))))
	 (string-set! buf w #\:)
	 (set! w (+fx w 1))
	 ;; date-second
	 (set! w (+fx w (blit-int2! buf w (date-second date))))
	 (set! w (+fx w 1))
	 ;; GMT
	 (set! w (+fx w (blit-buf! buf w "GMT")))
	 (string-shrink! buf w)))
   
   (let ((tz (date-timezone date)))
      (if (=fx tz 0)
	  (utc-string date)
	  (utc-string (seconds->gmtdate (date->seconds date))))))

;*---------------------------------------------------------------------*/
;*    seconds->string ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (seconds->string sec)
   ($date-seconds-to-string sec))

;*---------------------------------------------------------------------*/
;*    seconds->utc-string ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (seconds->utc-string sec)
   ($date-seconds-to-utc-string sec))

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
       ($date-day-name (+fx 1 (remainderfx day 7))))
      (else
       ($date-day-name day))))

;*---------------------------------------------------------------------*/
;*    day-aname ...                                                    */
;*---------------------------------------------------------------------*/
(define (day-aname day)
   (cond
      ((<fx day 1)
       (error 'day-aname "Illegal day number" day))
      ((>fx day 7)
       ($date-day-aname (+fx 1 (remainderfx day 7))))
      (else
       ($date-day-aname day))))

;*---------------------------------------------------------------------*/
;*    month-name ...                                                   */
;*---------------------------------------------------------------------*/
(define (month-name month)
   (cond
      ((<fx month 1)
       (error 'month-aname "Illegal month number" month))
      ((>fx month 12)
       ($date-month-name (+fx 1 (remainderfx month 12))))
      (else
       ($date-month-name month))))

;*---------------------------------------------------------------------*/
;*    month-aname ...                                                  */
;*---------------------------------------------------------------------*/
(define (month-aname month)
   (cond
      ((<fx month 1)
       (error 'month-aname "Illegal month number" month))
      ((>fx month 12)
       ($date-month-aname (+fx 1 (remainderfx month 12))))
      (else
       ($date-month-aname month))))

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
      (let ((d (rfc2822-parse-date port)))
	 (close-input-port port)
	 d)))

;*---------------------------------------------------------------------*/
;*    rfc2822-parse-date ...                                           */
;*---------------------------------------------------------------------*/
(define (rfc2822-parse-date ip::input-port)
   (read/rp day-of-week-grammar ip))

;*---------------------------------------------------------------------*/
;*    date->rfc2822-date ...                                           */
;*---------------------------------------------------------------------*/
(define (date->rfc2822-date date)
   
   (define (rfc-string date tz)
      (let ((buf (make-string 32 #\space))
	    (w 0))
;* 	  (format "~a, ~a ~a ~a ~2,0d:~2,0d:~2,0d ~a~2,0d~2,0d"        */
;* 	     (day-aname (date-wday date))                              */
;* 	     (date-day date)                                           */
;* 	     (month-aname (date-month date))                           */
;* 	     (date-year date)                                          */
;* 	     (date-hour date)                                          */
;* 	     (date-minute date)                                        */
;* 	     (date-second date)                                        */
;* 	     (if (<fx tz 0) "-" "+")                                   */
;* 	     (absfx (/fx tz 3600))                                     */
;* 	     (absfx (remainder tz 3600)))                              */
	 ;; date-aname
	 (set! w (+fx w (blit-buf! buf w (day-aname (date-wday date)))))
	 (string-set! buf w #\,)
	 (set! w (+fx w 2))
	 ;; date-day
	 (set! w (+fx w (blit-int! buf w (date-day date))))
	 (set! w (+fx w 1))
	 ;; montn-aname
	 (set! w (+fx w (blit-buf! buf w (month-aname (date-month date)))))
	 (set! w (+fx w 1))
	 ;; date-year
	 (set! w (+fx w (blit-int! buf w (date-year date))))
	 (set! w (+fx w 1))
	 ;; date-hour
	 (set! w (+fx w (blit-int2! buf w (date-hour date))))
	 (string-set! buf w #\:)
	 (set! w (+fx w 1))
	 ;; date-minute
	 (set! w (+fx w (blit-int2! buf w (date-minute date))))
	 (string-set! buf w #\:)
	 (set! w (+fx w 1))
	 ;; date-second
	 (set! w (+fx w (blit-int2! buf w (date-second date))))
	 (set! w (+fx w 1))
	 ;; +/-
	 (string-set! buf w (if (<fx tz 0) #\- #\+))
	 (set! w (+fx w 1))
	 ;; tz/3600
	 (set! w (+fx w (blit-int2! buf w (/fx tz 3600))))
	 ;; tz%3600
	 (set! w (+fx w (blit-int2! buf w (remainderfx tz 3600))))
	 (string-shrink! buf w)))
   
   (let ((tz (date-timezone date)))
      (if (=fx tz 0)
	  (date->utc-string date)
	  (rfc-string date tz))))

;*---------------------------------------------------------------------*/
;*    date->iso8601-date ...                                           */
;*---------------------------------------------------------------------*/
(define (date->iso8601-date date)
   (let ((tz (date-timezone date)))
      (if (=fx tz 0)
          (format "~a-~2,0d-~2,0dT~2,0d:~2,0d:~2,0dZ"
             (date-year date)
             (date-month date)
             (date-day date)
             (date-hour date)
             (date-minute date)
             (date-second date))
          (format "~a-~2,0d-~2,0dT~2,0d:~2,0d:~2,0d~a~2,0d:~2,0d"
             (date-year date)
             (date-month date)
             (date-day date)
             (date-hour date)
             (date-minute date)
             (date-second date)
             (if (<fx tz 0) "-" "+")
	     (absfx (/fx tz 3600))
	     (absfx (remainder tz 3600))))))

;*---------------------------------------------------------------------*/
;*    the-digit ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (the-digit n)
   `(-fx (the-byte-ref ,n) (char->integer #\0)))

;*---------------------------------------------------------------------*/
;*    the-fix ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (the-fix b1 b2)
   `(+fx (*fx ,b1 10) ,b2))

;*---------------------------------------------------------------------*/
;*    iso8601-date->date ...                                           */
;*---------------------------------------------------------------------*/
(define (iso8601-date->date string)
   (let ((port (open-input-string string)))
      (unwind-protect
	 (iso8601-parse-date port)
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    iso8601-parse-date ...                                           */
;*---------------------------------------------------------------------*/
(define (iso8601-parse-date ip::input-port)

   (define iso8601-ZMM-grammar
      (regular-grammar (YYYY MM DD HH mm ss sss ZHH)
	 ((: ":" (= 2 digit))
	  (let* ((b1 (the-digit 1))
		 (b2 (the-digit 2))
		 (TZ (if (<fx ZHH 0)
			 (-fx ZHH (*fx 60 (the-fix b1 b2)))
			 (+fx ZHH (*fx 60 (the-fix b1 b2))))))
	     (make-date :year YYYY :month MM :day DD :hour HH :min mm :sec ss
		:nsec sss :timezone TZ)))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date :year YYYY :month MM :day DD :hour HH :min mm :sec ss
		 :nsec sss :timezone ZHH)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))

   (define iso8601-Z-grammar
      (regular-grammar (YYYY MM DD HH mm ss sss)
	 ((in "zZ")
	  (make-date :timezone 0
	     :year YYYY :month MM :day DD :hour HH :min mm :sec ss :nsec sss))
	 ((: "+" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-ZMM-grammar (the-port)
		YYYY MM DD HH mm ss sss (*fx 3600 (the-fix b1 b2)))))
	 ((: "-" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-ZMM-grammar (the-port)
		YYYY MM DD HH mm ss sss (negfx (*fx 3600 (the-fix b1 b2))))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date
		 :year YYYY :month MM :day DD :hour HH :min mm :sec ss
		 :nsec sss :timezone 0)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))
   
   (define iso8601-sss-grammar
      (regular-grammar (YYYY MM DD HH mm ss)
	 ((: "." (= 3 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2))
		(b3 (the-digit 3)))
	     (read/rp iso8601-Z-grammar (the-port)
		YYYY MM DD HH mm ss
		(*llong #l1000000 (fixnum->llong (the-fix (the-fix b1 b2) b3))))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date
		 :year YYYY :month MM :day DD :hour HH :min mm :sec ss
		 :timezone 0)
	      (begin
		 (unread-char! (the-failure) (the-port))
		 (read/rp iso8601-Z-grammar (the-port)
		    YYYY MM DD HH mm ss #l0))))))

   (define iso8601-ss-grammar
      (regular-grammar (YYYY MM DD HH mm)
	 ((: ":" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-sss-grammar (the-port)
		YYYY MM DD HH mm (the-fix b1 b2))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date 
		 :year YYYY :month MM :day DD :hour HH :min mm :timezone 0)
	      (begin
		 (unread-char! (the-failure) (the-port))
		 (read/rp iso8601-Z-grammar (the-port)
		    YYYY MM DD HH mm 0 #l0))))))

   (define iso8601-mm-grammar
      (regular-grammar (YYYY MM DD HH)
	 ((: ":" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-ss-grammar (the-port)
		YYYY MM DD HH (the-fix b1 b2))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date 
		 :year YYYY :month MM :day DD :hour HH :timezone 0 :dst 0)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))
   
   (define iso8601-HH-grammar
      (regular-grammar (YYYY MM DD)
	 ((: (in "T ") (= 2 digit))
	  ;; as an extension, Bigloo supports white spaces in ISO dates
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-mm-grammar (the-port)
		YYYY MM DD (the-fix b1 b2))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date
		 :year YYYY :month MM :day DD :hour 1)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))

   (define iso8601-DD-grammar
      (regular-grammar (YYYY MM)
	 ((: "-" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-HH-grammar (the-port)
		YYYY MM (the-fix b1 b2))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date
		 :year YYYY :month MM :hour 1)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))
   
   (define iso8601-MM-grammar
      (regular-grammar (YYYY)
	 ((: "-" (= 2 digit))
	  (let ((b1 (the-digit 1))
		(b2 (the-digit 2)))
	     (read/rp iso8601-DD-grammar (the-port)
		YYYY (the-fix b1 b2))))
	 (else
	  (if (eof-object? (the-failure))
	      (make-date :year YYYY :hour 1)
	      (parse-error "iso8601-parse-date" "Illegal time"
		 (the-failure) (the-port))))))

   (define iso8601-grammar
      (regular-grammar ()
	 ((: (= 4 digit))
	  (let ((b1 (the-digit 0))
		(b2 (the-digit 1))
		(b3 (the-digit 3))
		(b4 (the-digit 4)))
	     (read/rp iso8601-MM-grammar (the-port) (the-fixnum))))
	 (else
	  (parse-error "iso8601-parse-date" "Illegal time"
	     (the-failure) (the-port)))))
   
   (read/rp iso8601-grammar ip))

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
       (let* ((day (read/rp the-fixnum-grammar (the-port)))
	      (month (read/rp month-grammar (the-port)))
	      (year (read/rp the-fixnum-grammar (the-port))))
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
		   :dst -1)))))
      ((+ digit)
       (let* ((day (the-fixnum))
	      (month (read/rp month-grammar (the-port)))
	      (year (read/rp the-fixnum-grammar (the-port))))
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
		   :dst -1)))))
      (else
       (parse-error "rfc2822-parse-date"
	  "Illegal day of week"
	  (the-failure) (the-port)))))

;*---------------------------------------------------------------------*/
;*    the-fixnum-grammar ...                                           */
;*---------------------------------------------------------------------*/
(define the-fixnum-grammar
   (regular-grammar ((FWS (in " \t\n\r")))
      ((+ FWS)
       (ignore))
      ((+ digit)
       (the-fixnum))
      (else
       (parse-error "rfc2822-parse-date"
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
	  (else (parse-error "rfc2822-parse-date"
			     "Illegal month"
			     (the-string) (the-port)))))
      (else
       (parse-error "rfc2822-parse-date" "Illegal month"
		    (the-failure) (the-port)))))

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
	  (values b1 (the-fix b3 b4) 0)))
      ((: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 1))
	     (b3 (the-digit 3))
	     (b4 (the-digit 4)))
	  (values (the-fix b1 b2) (the-fix b3 b4) 0)))
      ((: (= 2 digit) #\: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 1))
	     (b3 (the-digit 3))
	     (b4 (the-digit 4))
	     (b5 (the-digit 6))
	     (b6 (the-digit 7)))
	  (values (the-fix b1 b2) (the-fix b3 b4) (the-fix b5 b6))))
      ((: digit #\: (= 2 digit) #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b3 (the-digit 2))
	     (b4 (the-digit 3))
	     (b5 (the-digit 5))
	     (b6 (the-digit 6)))
	  (values b1 (the-fix b3 b4) (the-fix b5 b6))))
      ((: #\: (= 2 digit))
       (let ((b1 (the-digit 0))
	     (b2 (the-digit 2)))
	  (values 0 0 (the-fix b1 b2))))
      (else
       (parse-error "rfc2822-parse-date" "Illegal time"
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
     (CEST . +2)
     (UT . 0)
     (GMT . 0)
     (BST . +1)))

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
	  (if (=fx (the-byte-ref 0) (char->integer #\-))
	      (negfx (*fx 60 (+fx (*fx h 60) m)))
	      (*fx 60 (+fx (*fx h 60) m)))))
      ((: (in "+-") (= 3 digit))
       (let ((h (the-digit 1))
	     (m (+fx (*fx 10 (the-digit 2)) (the-digit 3))))
	  (if (=fx (the-byte-ref 0) (char->integer #\-))
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
       (parse-error "rfc2822-parse-date" "Illegal zone"
		    (the-failure) (the-port)))))

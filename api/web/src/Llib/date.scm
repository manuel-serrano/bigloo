;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Llib/date.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Cyprien Nicolas                                   */
;*    Creation    :  Tue Jul 22 08:06:43 2008                          */
;*    Last change :  Tue Mar 22 08:20:59 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    W3C dates (i.e., ISO8601 date format described in RFC 3339).     */
;*    See:                                                             */
;*      http://www.w3.org/TR/1998/NOTE-datetime-19980827               */
;*      http://fr.wikipedia.org/wiki/ISO_8601                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_date

   (export (w3c-datetime-timestamp::bstring)
	   (w3c-datetime->date::date ::bstring)
	   (date->w3c-datetime::bstring ::date)))

;*---------------------------------------------------------------------*/
;*    w3c-datetime-timestamp ...                                       */
;*---------------------------------------------------------------------*/
(define (w3c-datetime-timestamp)
   (date->w3c-datetime (current-date)))

;*---------------------------------------------------------------------*/
;*    w3c-datetime-parse ...                                           */
;*---------------------------------------------------------------------*/
(define (w3c-datetime-parse string)
   ;; http://www.w3.org/TR/NOTE-datetime
   (let ((ip (open-input-string string)))
      
      (define date-grammar
	 (regular-grammar ((2d (= 2 digit)) (4d (= 4 digit)))
	    ((: 4d "-" 2d "-" 2d "T") ; Date followed by time
	     (let ((d1 (the-substring 0 4))
		   (d2 (the-substring 5 7))
		   (d3 (the-substring 8 10)))
		(cons* d1 d2 d3 (read/rp time-grammar ip))))
	    ((: 4d "-" 2d "-" 2d) ; Full date without time
	     (list (the-substring 0 4) (the-substring 5 7) (the-substring 8 10)))
	    ((: 4d "-" 2d) ; Year and month
	     (list (the-substring 0 4) (the-substring 5 7)))
	    ((: 4d) ; Year
	     (list (the-substring 0 4)))
	    (else
	     (error 'w3c-datetine-parse
		    "Invalid (ISO-8601:2000/W3C-NOTE-datetime) format"
		    string))))
      
      (define time-grammar
	 ; At this point, the 'T' character has been read by time-grammar
	 (regular-grammar ((2d (= 2 digit)))
	    ((: 2d ":" 2d ":" 2d (in ",.") (+ digit)) ; Full time
	     ;; decimal fraction is ignored as Bigloo ignores it too
	     (let ((d1 (the-substring 0 2))
		   (d2 (the-substring 3 5))
		   (d3 (the-substring 6 8)))
		(cons* d1 d2 d3 (read/rp tz-grammar ip))))
	    ((: 2d ":" 2d ":" 2d) ; Full time, without decimal fraction
	     (let ((d1 (the-substring 0 2))
		   (d2 (the-substring 3 5))
		   (d3 (the-substring 6 8)))
		(cons* d1 d2 d3 (read/rp tz-grammar ip))))
	    ((: 2d ":" 2d) ; Time without seconds
	     (let ((d1 (the-substring 0 2))
		   (d2 (the-substring 3 5)))
		(cons*  d1 d2 (read/rp tz-grammar ip))))
	    (else
	     (error 'w3c-datetine-parse
		    "Invalid (ISO-8601:2000/W3C-NOTE-datetime) format"
		    string))))
      
      (define tz-grammar
	 (regular-grammar ((2d (= 2 digit)))
	    ("Z" '()) ; UTC Timezone
	    ((: (in "-+") 2d ":" 2d) ; Other timezones
	     (let ((s (if (string=? (the-substring 0 1) "+") 1 -1))
		   (h (string->number (the-substring 1 3)))
		   (m (string->number (the-substring 4 6))))
		(list (number->string (* s 60 (+ m (* h 60)))))))
	    (else
	     (error 'w3c-datetine-parse
		    "Invalid (ISO-8601:2000/W3C-NOTE-datetime) format"
		    string))))

      (unwind-protect
	 (read/rp date-grammar ip)
	 (close-input-port ip))))

;*---------------------------------------------------------------------*/
;*    w3c-datetime->date ...                                           */
;*---------------------------------------------------------------------*/
(define (w3c-datetime->date string)
   (let loop ((merge '())
	      (l1 (w3c-datetime-parse string))
	      (l2 '(year: month: day: hour: min: sec: timezone:)))
      (cond
	 ((or (null? l1) (null? l2))
	  (apply make-date (reverse! merge)))
	 (else
	  (if (car l1)
	      (loop (cons* (string->number (car l1)) (car l2) merge)
		    (cdr l1)
		    (cdr l2))
	      (loop merge (cdr l1) (cdr l2)))))))
		    
;*---------------------------------------------------------------------*/
;*    date->w3c-datetime ...                                           */
;*---------------------------------------------------------------------*/
(define (date->w3c-datetime date)
   (let ((yr (integer->string (date-year date)))
	 (mo (integer->string/padding (date-month date) 2))
	 (dy (integer->string/padding (date-day date) 2))
	 (hr (integer->string/padding (date-hour date) 2))
	 (mi (integer->string/padding (date-minute date) 2))
	 (se (integer->string/padding (date-second date) 2))
	 (dst (let ((dst (date-is-dst date))) (if (< dst 0) 0 dst)))
	 (tz (date-timezone date)))
      (format "~a-~a-~aT~a:~a:~a~a"
	      yr mo dy hr mi se
	      (let ((h (integer->string/padding
			(+ dst (quotient (abs tz) 3600)) 2))
		    (m (integer->string/padding
			(quotient (remainder (abs tz) 3600) 60) 2)))
		 ;; WARNING: mismatch between on timezones between Bigloo dates
		 ;; and iso8601!
		 ;; It seems that Bigloo considers the local timezone as a
		 ;; reference with a timezome representing the shift from
		 ;; the localtime to UTC. For instance, for Paris (CET), the
		 ;; Bigloo timezone is -3600. On the other hand, iso8601
		 ;; considers the sun as the center of the galaxy, hence UTC
		 ;; is the temporal reference. Hence, the shift is not *to*
		 ;; UTC but *from* UTC. So, for instance, for Paris, the shift
		 ;; is +01:00.
		 (cond
		    ((< tz 0) (format "+~a:~a" h m))
		    ((> tz 0) (format "-~a:~a" h m))
		    (else #\Z))))))

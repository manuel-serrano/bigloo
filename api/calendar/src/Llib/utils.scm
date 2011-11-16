;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/calendar/src/Llib/utils.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 24 15:54:35 2006                          */
;*    Last change :  Tue Nov 15 10:24:21 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Calendar utitility functions.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __calendar_utils

   (import __calendar_types)
   
   (export (calendar-insert! ::calendar ::calevent)
	   (calendar-sort! ::calendar)
	   (calevent-during-day? ::calevent ::date)
	   (calendar->list ::date)))

;*---------------------------------------------------------------------*/
;*    calendar-insert! ...                                             */
;*    -------------------------------------------------------------    */
;*    Insert a calevent at the good location.                          */
;*---------------------------------------------------------------------*/
(define (calendar-insert! cal ce)
   (with-access::calendar cal (events)
      (with-access::calevent ce (dtstart)
	 (if (or (not (date? dtstart)) (null? events))
	     (set! events (cons ce events))
	     (let ((d1 (date->seconds dtstart)))
		(with-access::calevent (car events) ((d2 dtstart))
		   (if (and (date? d1) (>elong (date->seconds d2) d1))
		       (set! events (cons ce events))
		       (let loop ((events (cdr events))
				  (prev events))
			  (if (null? events)
			      (set-cdr! prev (list ce))
			      (with-access::calevent (car events) (dtstart)
				 (if (not (date? dtstart))
				     (loop (cdr events)
					   events)
				     (let ((d2 (date->seconds dtstart)))
					(if (>elong d2 d1)
					    (set-cdr! prev (cons ce events))
					    (loop (cdr events)
						  events))))))))))))))

;*---------------------------------------------------------------------*/
;*    calendar-sort! ...                                               */
;*---------------------------------------------------------------------*/
(define (calendar-sort! cal::calendar)
   (with-access::calendar cal (events)
      (set! events
	    (sort events
		  (lambda (e1 e2)
		     (with-access::calevent e1 ((d1 dtstart))
			(with-access::calevent e2 ((d2 dtstart))
			   (and (date? d1) (date? d2)
				(<elong (date->seconds d1)
					(date->seconds d2))))))))
      cal))

;*---------------------------------------------------------------------*/
;*    is-day? ...                                                      */
;*---------------------------------------------------------------------*/
(define (is-day? day d)
   (and (=fx (date-year day) (date-year d))
	(=fx (date-month day) (date-month d))
	(=fx (date-day day) (date-day d))))

;*---------------------------------------------------------------------*/
;*    simple-during-day? ...                                           */
;*---------------------------------------------------------------------*/
(define (simple-during-day? e::calevent d::date)
   (with-access::calevent e (dtstart dtend)
      (and (date? dtstart)
	   (or (is-day? dtstart d)
	       (and (date? dtend)
		    (or (>fx (date-hour dtend) 0)
			(>fx (date-minute dtend) 0)
			(>fx (date-second dtend) 0)
			(>fx (date-day dtend) 0))
		    (let ((ds (date->seconds d)))
		       (or (is-day? dtend d)
			   (and (<elong (date->seconds dtstart) ds)
				(>elong (date->seconds dtend) ds)))))))))

;*---------------------------------------------------------------------*/
;*    recurrence-during-day? ...                                       */
;*    -------------------------------------------------------------    */
;*    This implementation is highly partial. It is far from            */
;*    implementing the complete test required by ICAL (rfc 2445).      */
;*    Someone more courageous that I am should help here...            */
;*---------------------------------------------------------------------*/
(define (recurrence-during-day? e::calevent d::date)
   (let ((ds (date->seconds d)))
      (with-access::calevent e (dtstart (rec recurrence))
	 (and (with-access::calrecurrence rec (until count)
		 (or (not (date? until))
		     (>elong (date->seconds until) ds)))
 	      (with-access::calrecurrence rec ((i interval)
					       frequency
					       bymonth)
		 (case frequency
		    ((YEARLY)
		     (and (or (=fx i 1)
			      (and (date? dtstart)
				   (=fx (remainderfx (date-year dtstart) i) 0)
				   (=fx (date-year dtstart) (date-year d))))
			  (or (and (pair? bymonth)
				   (fixnum? (car bymonth))
				   (=fx (date-month d) (car bymonth)))
			      (and (date? dtstart)
				   (=fx (date-month dtstart) (date-month d))))
			  (and (date? dtstart)
			       (=fx (date-day dtstart) (date-day d)))))
		    (else
		     'no-implemented
		     #f)))))))
   
;*---------------------------------------------------------------------*/
;*    calevent-during-day? ...                                         */
;*---------------------------------------------------------------------*/
(define (calevent-during-day? e::calevent d::date)
   (or (simple-during-day? e d)
       (with-access::calevent e (recurrence)
	  (and (isa? recurrence calrecurrence) (recurrence-during-day? e d)))))

;*---------------------------------------------------------------------*/
;*    calendar->list ...                                               */
;*---------------------------------------------------------------------*/
(define (calendar->list dt)
   (let* ((mlen (date-month-length dt))
	  (ds (date-copy dt :day 1))
	  (de (date-copy dt :day mlen))
	  (start (-elong (date->seconds ds)
			 (*elong (fixnum->elong (-fx (date-week-day ds) 1))
				  (day-seconds))))
	  (end (+elong (date->seconds de)
		       (*elong (fixnum->elong (-fx 7 (date-week-day de)))
			       (day-seconds)))))
      (let loop ((i start)
		 (res '()))
	 (if (>elong i end)
	     (list-split! (reverse! res) 7)
	     (loop (+elong i (day-seconds))
		   (cons (seconds->date i) res))))))

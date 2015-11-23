;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/date.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb  5 10:03:10 2003                          */
;*    Last change :  Wed Nov 18 19:11:52 2015 (serrano)                */
;*    Copyright   :  2003-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Test date features                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module date
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-date)))

;*---------------------------------------------------------------------*/
;*    test-date ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-date)
   (test-module "date" "date.scm")
   (test "date?" (date? (pwd)) #f)
   (test "date?" (date? (current-date)) #t)
   (test "seconds->date.1" (date? (seconds->date (current-seconds))) #t)
   (let* ((s1 (current-seconds))
	  (d1 (seconds->date s1)))
      (test "seconds->date.2" (date->seconds d1) s1))
   (let* ((d1 (current-date))
	  (s1 (date->seconds d1)))
      (test "seconds->date.3" (seconds->date s1) d1))
   (test "date-day.1" (date-day (make-date :sec 1 :min 1 :hour 1
				   :day 19 :month 2 :year 2000))
	 19)
   (test "date-day.2" (date-day (make-date :sec 1 :min 1 :hour 1
				   :day 31 :month 5 :year 1999))
	 31)
   (test "date-day.3" (date-day
		       (seconds->date
			(date->seconds (make-date :sec 1 :min 1 :hour 1
					  :day 30 :month 5 :year 1998))))
	 30)
   (test "date-day.3b" (date-month
			(seconds->date
			 (date->seconds (make-date :sec 1 :min 1 :hour 1
					   :day 30 :month 5 :year 1998))))
	 5)
   (test "date-day.4" (date-day
		       (seconds->date
			(date->seconds (make-date :sec 1 :min 1 :hour 1
					  :day 31 :month 5 :year 1971))))
	 31)
   (test "date-week-day.1" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 2 :month 2 :year 2003))
	 1)
   (test "date-week-day.2" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 3 :month 2 :year 2003))
	 2)
   (test "date-week-day.3" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 4 :month 2 :year 2003))
	 3)
   (test "date-week-day.4" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 5 :month  2 :year 2003))
	 4)
   (test "date-week-day.5" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 6 :month  2 :year 2003))
	 5)
   (test "date-week-day.6" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 7 :month  2 :year 2003))
	 6)
   (test "date-week-day.7" (date-week-day (make-date :sec 1 :min 1 :hour 1
					     :day 8 :month  2 :year 2003))
	 7)
   (test "date-month" (date-month (make-date :sec 1 :min 1 :hour 1
				     :day 17 :month  5 :year 1980))
	 5)
   (test "date-year" (date-year (make-date :sec 1 :min 1 :hour 1
				   :day 17 :month  5 :year 1981))
	 1981)
   (test "leap-year?" (leap-year? 1966) #f)
   (test "leap-year?" (leap-year? 1968) #t)
   (test "leap-year?" (leap-year? 2000) #t)
   (test "leap-year?" (leap-year? 2100) #f)
   (test "dst.1" (date-hour (make-date :sec 0 :min 0 :hour 14
			       :day 6 :month 1 :year 2003))
	 14)
   (test "dst.2" (date-hour (make-date :sec 0 :min 0 :hour 14
			       :day 6 :month 7 :year 2003))
	 14)
   (test "dst.3" (date-hour (make-date :sec 0 :min 0 :hour 14
			       :day 6 :month 7 :year 2003))
	 14)
   (test "dst.4" (date-hour (make-date :sec 0 :min 0 :hour 23
			       :day 6 :month 7 :year 2003))
	 23)
   (test "dst.5" (date-day (make-date :sec 0 :min 30 :hour 23
			      :day 6 :month 7 :year 2003))
	 6)
   (let ((d1 (date->seconds (make-date :sec 0 :min 30 :hour 23
			       :day 6 :month 7 :year 2003))))
      (test "dst.6" (date->seconds (seconds->date d1)) d1))
   (let ((d1 (date->seconds (make-date :sec 0 :min 30 :hour 23
			       :day 6 :month 1 :year 2003))))
      (test "dst.7" (date->seconds (seconds->date d1)) d1))
   (test "seconds-add" (date-day
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 16 :month  5 :year 1996))
			  (day-seconds))))
	 17)
   (test "seconds-add" (date-month
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 31 :month  5 :year 1996))
			  (day-seconds))))
	 6)
   (test "seconds-add" (date-day
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 31 :month  5 :year 1996))
			  (day-seconds))))
	 1)
   (test "seconds-add" (date-day
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 17 :month  5 :year 1996))
			  (*elong #e31 (day-seconds)))))
	 17)
   (test "seconds-add" (date-month
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 17 :month  5 :year 1996))
			  (*elong #e31 (day-seconds)))))
	 6)
   (test "seconds-add" (date-day
			(seconds->date
			 (+elong
			  (date->seconds (make-date :sec 1 :min 1 :hour 1
					    :day 31 :month  5 :year 1996))
			  #e0)))
	 31)
   (test "seconds-sub" (date-day
			(seconds->date
			 (-elong
			  (date->seconds
			   (make-date :sec 1 :min 1 :hour 1
			      :day 18 :month  5 :year 1996))
			  (day-seconds))))
	 17)
   (test "=second" (=elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 20 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 20 :month 6 :year 1996)))
	 #t)
   (test "=second" (=elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 20 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 19 :month 6 :year 1996)))
	 #f)
   (test "<second" (<elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 18 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 19 :month 6 :year 1996)))
	 #t)
   (test "<second" (<elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 20 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 19 :month 6 :year 1996)))
	 #f)
   (test "<=second" (<=elong (date->seconds
			      (make-date :sec 1 :min 1 :hour 1
				 :day 18 :month 6 :year 1996))
			     (date->seconds
			      (make-date :sec 1 :min 1 :hour 1
				 :day 19 :month 6 :year 1996)))
	 #t)
   (test "<=second" (<=elong (date->seconds
			      (make-date :sec 1 :min 1 :hour 1
				 :day 18 :month 6 :year 1996))
			     (date->seconds
			      (make-date :sec 1 :min 1 :hour 1
				 :day 18 :month 6 :year 1996)))
	 #t)
   (test "<=second" (<elong (date->seconds
			     (make-date :sec 1 :min 1 :hour 1
				:day 20 :month 6 :year 1996))
			    (date->seconds
			     (make-date :sec 1 :min 1 :hour 1
				:day 19 :month 6 :year 1996)))
	 #f)
   (test ">second" (>elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 18 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 19 :month 6 :year 1996)))
	 #f)
   (test ">second" (>elong (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 20 :month 6 :year 1996))
			   (date->seconds
			    (make-date :sec 1 :min 1 :hour 1
			       :day 19 :month 6 :year 1996)))
	 #t)
   (test ">=second.1" (>=elong (date->seconds
				(make-date :sec 1 :min 1 :hour 1
				   :day 18 :month 6 :year 1996))
			       (date->seconds
				(make-date :sec 1 :min 1 :hour 1
				   :day 19 :month 6 :year 1996)))
	 #f)
   (test ">=second.2" (>=elong (date->seconds
				(make-date :sec 1 :min 1 :hour 1
				   :day 18 :month 6 :year 1996))
			       (date->seconds
				(make-date :sec 1 :min 1 :hour 1
				   :day 18 :month 6 :year 1996)))
	 #t)
   (test ">=second.3" (>elong (date->seconds
			       (make-date :sec 1 :min 1 :hour 1
				  :day 20 :month 6 :year 1996))
			      (date->seconds
			       (make-date :sec 1 :min 1 :hour 1
				  :day 19 :month 6 :year 1996)))
	 #t)
   (test "date->utc-string" (string? (date->utc-string (current-date))) #t)
   (test "rfc2822-date->date.1"
	 (rfc2822-date->date "Wed, 22 Oct 2008 17:04:22 +0200")
	 (make-date :year 2008 :month 10 :day 22
	    :hour 17 :min 04 :sec 22 :timezone 7200))
   (test "nano.1"
      (let* ((nsec (current-nanoseconds))
	     (usec (/llong nsec #l1000))
	     (sec (/llong usec #l1000000)))
	 (=llong sec (/llong nsec (*llong #l1000000 #l1000))))
      #t)
   (test "nano.2"
      (let* ((sec (current-seconds))
	     (d (nanoseconds->date
		   (+llong (*llong (elong->llong sec) #l1000000000) #l1))))
	 (=llong (date-nanosecond d) #l1))
      #t)
   (test "nano.3"
      (let* ((d (make-date :year 2000 :sec 14 :nsec #l23)))
	 (and (=llong (date-nanosecond d) #l23)
	      (=fx (date-second d) 14)))
      #t)
   (test "nano.4" (date? (nanoseconds->date (current-nanoseconds))) #t)
   (let* ((s1 (current-nanoseconds))
	  (d1 (nanoseconds->date s1)))
      (test "nano.5" (date->nanoseconds d1) s1))
   (let* ((d1 (current-date))
	  (s1 (date->nanoseconds d1)))
      (test "nano.6" (nanoseconds->date s1) d1))
   )

	    
	 

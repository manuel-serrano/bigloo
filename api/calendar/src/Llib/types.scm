;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/calendar/src/Llib/types.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 21 10:22:30 2005                          */
;*    Last change :  Fri Dec 13 12:01:12 2013 (serrano)                */
;*    Copyright   :  2005-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Calendar types                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __calendar_types
   
   (option (set! *dlopen-init-gc* #t))
   
   (export (class calendar
	      (name::bstring read-only (default ""))
	      (id::bstring read-only (default "bigloo calendar"))
	      (version::bstring (default "2.0"))
	      (events::pair-nil (default '()))
	      (method (default #unspecified)))
	   
	   (class calevent
	      (calendar (default #f))
	      (uid (default #unspecified))
	      (dtstart (default #unspecified))
	      (dtend (default #unspecified))
	      (summary (default #unspecified))
	      (description (default #unspecified))
	      (recurrence (default #f))
	      (klass (default #unspecified))
	      (categories (default #unspecified))
	      (%optionals::pair-nil (default '()))
	      (attach (get (calevent-optional-getter 'attach))
		      (set (calevent-optional-setter 'attach)))
	      (priority (get (calevent-optional-getter 'priority))
			(set (calevent-optional-setter 'priority)))
	      (ressources (get (calevent-optional-getter 'ressources))
			  (set (calevent-optional-setter 'ressources)))
	      (location (get (calevent-optional-getter 'location))
			(set (calevent-optional-setter 'location)))
	      (status (get (calevent-optional-getter 'status))
		      (set (calevent-optional-setter 'status)))
	      (duration (get (calevent-optional-getter 'duration))
			(set (calevent-optional-setter 'duration)))
	      (related-to (get (calevent-optional-getter 'related-to))
			  (set (calevent-optional-setter 'related-to)))
	      (url (get (calevent-optional-getter 'url))
		   (set (calevent-optional-setter 'url)))
	      (action (get (calevent-optional-getter 'action))
		      (set (calevent-optional-setter 'action)))
	      (trigger (get (calevent-optional-getter 'trigger))
		       (set (calevent-optional-setter 'trigger)))
	      (repeat (get (calevent-optional-getter 'repeat))
		       (set (calevent-optional-setter 'repeat)))
	      (flush (get (calevent-optional-getter 'flush))
		     (set (calevent-optional-setter 'flush))))
	   
	   (class caltodo::calevent)
	   
	   (class calrecurrence
	      (frequency::symbol (default 'NONE))
	      (interval::int (default 1))
	      (count (default #f))
	      (until (default #f))
	      (bysecond::pair-nil (default '()))
	      (byminute::pair-nil (default '()))
	      (byhour::pair-nil (default '()))
	      (byweekday::pair-nil (default '()))
	      (bymonthday::pair-nil (default '()))
	      (byyearday::pair-nil (default '()))
	      (byweekno::pair-nil (default '()))
	      (bymonth::pair-nil (default '()))
	      (bysetpos::pair-nil (default '()))
	      (wkst (default #f)))

	   (calevent-optional-getter ::symbol)
	   (calevent-optional-setter ::symbol)))

;*---------------------------------------------------------------------*/
;*    calevent-optional-getter ...                                     */
;*---------------------------------------------------------------------*/
(define (calevent-optional-getter key)
   (lambda (o)
      (with-access::calevent o (%optionals)
	 (let ((c (assq key %optionals)))
	    (if (pair? c)
		(cdr c)
		#unspecified)))))
	      
;*---------------------------------------------------------------------*/
;*    calevent-optional-setter ...                                     */
;*---------------------------------------------------------------------*/
(define (calevent-optional-setter key)
   (lambda (o v)
      (with-access::calevent o (%optionals)
	 (let ((c (assq key %optionals)))
	    (if (pair? c)
		(set-cdr! c v)
		(set! %optionals (cons (cons key v) %optionals)))))))

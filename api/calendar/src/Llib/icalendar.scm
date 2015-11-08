;*=====================================================================*/
;*    .../prgm/project/bigloo/api/calendar/src/Llib/icalendar.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 21 10:14:19 2005                          */
;*    Last change :  Sat Nov  7 19:50:31 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    iCalendar parser                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __calendar_ical
   
   (import __calendar_types)
   
   (export (write-icalendar ::calendar ::output-port #!optional pred)
	   (port->icalendar::calendar ::input-port #!optional cal))

   (cond-expand
      ((not bigloo-class-generate)
       (include "icalendar.sch")))
   
   (static (class line
	      (name::symbol read-only)
	      (params::pair-nil read-only)
	      (val::bstring read-only)
	      fname
	      location)
	   
	   (class block
	      (l0::line read-only)
	      (ln::line read-only)
	      (body::pair-nil read-only))))

;*---------------------------------------------------------------------*/
;*    write-icalendar ...                                              */
;*---------------------------------------------------------------------*/
(define (write-icalendar cal::calendar oport::output-port #!optional pred)
   (write-block
    "VCALENDAR"
    oport
    (lambda (op)
       (with-access::calendar cal (id version method events)
	  (write-line "PRODID" '() id op)
	  (write-line "VERSION" '() version op)
	  (unless (eq? method #unspecified)
	     (write-line "METHOD" '() method op))
	  (if pred
	      (for-each (lambda (e)
			   (when (pred e)
			      (with-handler
				 (lambda (e)
				    (exception-notify e))
				 (write-calevent e op))))
		 events)
	      (for-each (lambda (e)
			   (with-handler
			      (lambda (e)
				 (exception-notify e))
			      (write-calevent e op)))
		 events))))))

;*---------------------------------------------------------------------*/
;*    write-block ...                                                  */
;*---------------------------------------------------------------------*/
(define (write-block name oport::output-port proc::procedure)
   (write-line "BEGIN" '() name oport)
   (proc oport)
   (write-line "END" '() name oport))

;*---------------------------------------------------------------------*/
;*    write-line ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-line name params val oport)
   (display name oport)
   (for-each (lambda (param)
		(display ";" oport)
		(display (car param) oport)
		(display "=" oport)
		(write-list (cdr param) oport))
	     params)
   (display ":" oport)
   (write-value val oport)
   (display "\r\n" oport))

;*---------------------------------------------------------------------*/
;*    write-list ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-list lst oport)
   (when (pair? lst)
      (let loop ((lst lst))
	 (write-value (car lst) oport)
	 (when (pair? (cdr lst))
	    (display "," oport)
	    (loop (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    write-list-line ...                                              */
;*---------------------------------------------------------------------*/
(define (write-list-line name params val oport)
   (display name oport)
   (for-each (lambda (param)
		(display ";" oport)
		(display (car param) oport)
		(display "=" oport)
		(write-list (cdr param) oport))
	     params)
   (display ":" oport)
   (write-list val oport)
   (display "\r\n" oport))

;*---------------------------------------------------------------------*/
;*    write-value ...                                                  */
;*---------------------------------------------------------------------*/
(define (write-value val op)
   (define (write-string val op)
      (let ((len (string-length val)))
	 (if (<=fx len 75)
	     ;; display as is
	     (display val op)
	     ;; fold the string
	     (begin
		(display-substring val 0 75 op)
		(let loop ((i 75))
		   (if (<fx i len)
		       (begin
			  (display "\r\n" op)
			  (display " " op)
			  (display-substring val i (minfx len (+fx i 75)) op)
			  (loop (+fx i 75)))))))))
   (cond
      ((or (integer? val) (symbol? val))
       (display val op))
      ((string? val)
       (write-string val op))
      (else
       (error 'write-icalendar "Illegal value" val))))

;*---------------------------------------------------------------------*/
;*    write-calevent ...                                               */
;*---------------------------------------------------------------------*/
(define (write-calevent ev op)
   (write-block
    "VEVENT"
    op
    (lambda (op)
       (with-access::calevent ev (dtstart dtend summary description
					  uid categories klass priority attach
					  ressources status duration
					  related-to url action trigger
					  repeat recurrence flush)
	  (when (date? dtstart)
	     (write-line "DTSTART" '() (date->icalendar-date-time dtstart) op))
	  (when (date? dtend)
	     (write-line "DTEND" '() (date->icalendar-date-time dtend) op))
	  (when (string? summary)
	     (write-line "SUMMARY" '() summary op))
	  (when (string? description)
	     (if (string-index description #\Newline)
		 (write-line "DESCRIPTION"
			     '(("ENCODING" "BASE64"))
			     (base64-encode description #f)
			     op)
		 (write-line "DESCRIPTION" '() description op)))
	  (when (string? uid)
	     (write-line "UID" '() uid op))
	  (when (string? attach)
	     (write-line "ATTACH" '() attach op))
	  (when (string? categories)
	     (write-list-line "CATEGORIES" '() categories op))
	  (when (string? klass)
	     (write-line "CLASS" '() klass op))
	  (when (string? priority)
	     (write-line "PRIORITY" '() priority op))
	  (when (string? ressources)
	     (write-line "RESSOURCES" '() ressources op))
	  (when (string? status)
	     (write-line "STATUS" '() status op))
	  (unless (eq? duration #unspecified)
	     (write-line "DURATION" '() duration op))
	  (unless (eq? related-to #unspecified)
	     (write-line "RELATED-TO" '() related-to op))
	  (unless (eq? url #unspecified)
	     (write-line "URL" '() url op))
	  (unless (eq? action #unspecified)
	     (write-line "ACTION" '() action op))
	  (unless (eq? trigger #unspecified)
	     (write-line "TRIGGER" '() trigger op))
	  (unless (eq? repeat #unspecified)
	     (write-line "REPEAT" '() repeat op))
	  (unless (eq? flush #unspecified)
	     (write-line "FLUSH" '() flush op))
	  (when recurrence
	     (with-access::calrecurrence recurrence (frequency interval count
							       until bysecond
							       byminute byhour
							       byweekday
							       bymonthday
							       byyearday
							       byweekno
							       bymonth
							       bysetpos
							       wkst)
		(with-output-to-port op
		   (lambda ()
		      (display "RRULE:")
		      (display "FREQ=")
		      (display frequency)
		      (display ";")
		      (display "INTERVAL=")
		      (display interval)
		      (display ";")
		      (when count
			 (display "COUNT=")
			 (display count)
			 (display ";"))
		      (when until
			 (display "UNTIL=")
			 (display until)
			 (display ";"))
		      (when (pair? bymonth)
			 (display "BYMONTH=")
			 (write-list bymonth op)
			 (display ";")))))
	     (display "\r\n"))))))

;*---------------------------------------------------------------------*/
;*    port->icalendar ...                                              */
;*---------------------------------------------------------------------*/
(define (port->icalendar iport #!optional cal)
   
   (define (evt b cal)
      (cond
	 ((not (block? b))
	  #f)
	 ((is-line-value? (block-l0 b) "VEVENT")
	  (vevent->calevent b (instantiate::calevent
				 (calendar cal))))
	 ((is-line-value? (block-l0 b) "VTODO")
	  (vevent->calevent b (instantiate::caltodo
				 (calendar cal))))
	 (else
	  #f)))

   (let ((cal (if (isa? cal calendar)
		  cal
		  (instantiate::calendar
		     (name (input-port-name iport)))))
	 (b (read-block iport)))
      (if (is-line-value? (block-l0 b) "VCALENDAR")
	  (with-access::calendar cal (events version method)
	     (for-each (lambda (l)
			  (unless (block? l)
			     (cond
				((is-line-name? l 'VERSION)
				 (set! version (line-val l)))
				((is-line-name? l 'METHOD)
				 (set! method (line-val l))))))
		       (block-body b))
	     (set! events
		   (sort (filter-map (lambda (b) (evt b cal))
				     (block-body b))
			 (lambda (e1 e2)
			    (with-access::calevent e1 ((d1 dtstart))
			       (with-access::calevent e2 ((d2 dtstart))
				  (and (date? d1) (date? d2)
				       (<elong (date->seconds d1)
					       (date->seconds d2))))))))
	     cal)
	  (raise (instantiate::&io-parse-error
		    (proc 'icalendar)
		    (msg "Illegal BEGIN:VCALENDAR line")
		    (obj (line-show (block-l0 b)))
		    (fname (line-fname (block-l0 b)))
		    (location (line-location (block-l0 b))))))))

;*---------------------------------------------------------------------*/
;*    read-block ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-block iport::input-port)
   (let ((l0 (read-icalendar-line iport)))
      (cond
	 ((eof-object? l0)
	  (raise (instantiate::&io-parse-error
		    (proc 'icalendar)
		    (msg "premature end of file")
		    (obj (line-show l0)) 
		    (fname (input-port-name iport))
		    (location 1))))
	 ((is-line-name? l0 'BEGIN)
	  (read-block-body l0 iport))
	 (else
	  (raise (instantiate::&io-parse-error
		    (proc 'icalendar)
		    (msg "Illegal BEGIN: line")
		    (obj (line-show l0))
		    (fname (line-fname l0))
		    (location (line-location l0))))))))

;*---------------------------------------------------------------------*/
;*    read-block-body ...                                              */
;*---------------------------------------------------------------------*/
(define (read-block-body l0 iport::input-port)
   (let ((name (line-val l0)))
      (let loop ((res '()))
	 (let ((l (read-icalendar-line iport)))
	    (cond
	       ((eof-object? l)
		(raise (instantiate::&io-parse-error
			  (proc 'icalendar)
			  (msg "unmatch BEGIN: line")
			  (obj (line-show l0))
			  (fname (line-fname l0))
			  (location (line-location l0)))))
	       ((is-line? l 'END name)
		(instantiate::block
		   (l0 l0)
		   (ln l)
		   (body (reverse! res))))
	       ((is-line-name? l 'BEGIN)
		(loop (cons (read-block-body l iport) res)))
	       (else
		(loop (cons l res))))))))

;*---------------------------------------------------------------------*/
;*    line-show ...                                                    */
;*---------------------------------------------------------------------*/
(define (line-show l)
   (if (eof-object? l)
       l
       (format "~a:~a" (line-name l) (line-val l))))

;*---------------------------------------------------------------------*/
;*    is-line? ...                                                     */
;*---------------------------------------------------------------------*/
(define (is-line? line name val)
   (and (is-line-name? line name)
	(is-line-value? line val)))

;*---------------------------------------------------------------------*/
;*    is-line-name? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-line-name? line name::symbol)
   (eq? (line-name line) name))

;*---------------------------------------------------------------------*/
;*    is-line-value? ...                                               */
;*---------------------------------------------------------------------*/
(define (is-line-value? line val)
   (string=? (line-val line) val))
       
;*---------------------------------------------------------------------*/
;*    read-icalendar-line ...                                          */
;*---------------------------------------------------------------------*/
(define (read-icalendar-line iport)
   (read/rp *contentline-lexer* iport))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error msg obj port)
   (raise
    (instantiate::&io-parse-error
       (proc 'icalendar)
       (msg msg)
       (obj obj)
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    lexer-error ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (lexer-error msg c)
   (let ((tmp (gensym)))
      `(let ((,tmp ,c))
	  (parse-error ,msg
		       (format "{~a}"
			       (cond
				  ((eof-object? ,tmp) ,tmp)
				  ((char>=? ,tmp #\space) ,tmp)
				  (else (format "#a~a" (char->integer ,tmp)))))
		       (the-port)))))

;*---------------------------------------------------------------------*/
;*    *contentline-lexer* ...                                          */
;*---------------------------------------------------------------------*/
(define *contentline-lexer*
   (regular-grammar ((vid (>= 3 (or alpha digit)))
		     (x-name (: "X-" (? (: vid "-")) (+ (or alpha digit "-"))))
		     (iana-token (+ (or alpha digit "-"))))
      ((or x-name iana-token)
       (let* ((loc (-fx (input-port-position (the-port)) (the-length)))
	      (fname (input-port-name (the-port)))
	      (name (the-symbol))
	      (params (read/rp *params-lexer* (the-port)))
	      (encv (apply string-append (read/rp *value-lexer* (the-port))))
	      (val (if (member '("ENCODING" "BASE64") params)
		       (base64-decode encv)
		       encv)))
	  (instantiate::line
	     (name name)
	     (params params)
	     (val val)
	     (fname fname)
	     (location loc))))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      (lexer-error "Illegal character in content line" c))))))

;*---------------------------------------------------------------------*/
;*    *params-lexer* ...                                               */
;*---------------------------------------------------------------------*/
(define *params-lexer* 
   (regular-grammar ((blank (in " \n\t\r")))
      ((: (* blank) ":")
       '())
      ((: (* blank) ";")
       (let* ((name (read/rp *param-name-lexer* (the-port)))
	      (vals (read/rp *param-values-lexer* (the-port))))
	  (cons (cons name vals) (ignore))))
      (else
       (lexer-error "Illegal character in parameter list" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    *param-name-lexer* ...                                           */
;*---------------------------------------------------------------------*/
(define *param-name-lexer*
   (regular-grammar ((vid (>= 3 (or alpha digit)))
		     (x-name (: "X-" (? (: vid "-")) (+ (or alpha digit "-"))))
		     (iana-token (+ (or alpha digit "-"))))
      ((: (or iana-token x-name) #\=)
       (the-substring 0 -1))
      (else
       (lexer-error "Illegal character in parameter name" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    *param-values-lexer* ...                                         */
;*---------------------------------------------------------------------*/
(define *param-values-lexer*
   (regular-grammar ((WSP (or #a032 #a009))
		     (QSAFE-CHAR (or WSP #a033
				     (in (#x02d #x39))
				     (in (#x3c #x7e))
				     (in (#x80 #xf8))))
		     (SAFE-CHAR (or WSP #a033
				    (in (#x023 #x02b))
				    (in (#x02d #x39))
				    (in (#x3c #x7e))
				    (in (#x80 #xf8))))
		     (DQUOTE #\")
		     (paramtext (* SAFE-CHAR))
		     (quoted-string (: DQUOTE (* QSAFE-CHAR) DQUOTE)))
      ((or paramtext quoted-string)
       (let ((val (the-string)))
	  (cons val (ignore))))
      (","
       (ignore))
      ((+ (in " \t\n\r"))
       (ignore))
      (else
       (let ((c (the-failure)))
	  (if (and (char? c) (char=? c #\:))
	      (begin
		 (rgc-buffer-unget-char (the-port) (char->integer #\:))
		 '())
	      (lexer-error "Illegal character in parameter value" c))))))
      
;*---------------------------------------------------------------------*/
;*    *value-lexer* ...                                                */
;*---------------------------------------------------------------------*/
(define *value-lexer*
   (regular-grammar ((WSP (or #a032 #a009))
		     (VALUE-CHAR (or WSP (in (#x21 #x7e) (in (#x80 #xf8)))))
		     (CR #a013)
		     (LF #a010)
		     (SPACE #a032)
		     (TAB #\tab)
		     (CRLF (: (? CR) LF)))
      ((+ VALUE-CHAR)
       (let ((val (the-string)))
	  (cons val (ignore))))
      ((: CRLF)
       '())
      ((: CRLF (+ (or SPACE TAB)))
       (ignore))
      (else
       (lexer-error "Illegal character in value line" (the-failure)))))

;*---------------------------------------------------------------------*/
;*    *rrule-value-lexer* ...                                          */
;*---------------------------------------------------------------------*/
(define *rrule-value-lexer*
   (regular-grammar ((IDENT (+ (uncase (in ("az")))))
		     rec l)
      ((: IDENT #\=)
       (let ((s (string->symbol (the-substring 0 -1))))
	  (define (read-token pred)
	     (let ((f (read (the-port))))
		(if (pred f)
		    f
		    (raise
		     (instantiate::&io-parse-error
			(proc 'icalendar)
			(msg (format "Illegal ~a value: " s))
			(obj f)
			(fname (line-fname l))
			(location (line-location l)))))))
	  (define (read-date)
	     (let ((g (regular-grammar ()
			 ((+ (+ (or (in ("09") ":" "ZT"))))
			  (icalendar-date->date (the-string)))
			 (else
			  (let ((c (the-failure)))
			     (if (or (eof-object? c) (char=? c #\;))
				 c
				 (raise
				  (instantiate::&io-parse-error
				     (proc 'icalendar)
				     (msg (format "Illegal date character: " c))
				     (obj c)
				     (fname (line-fname l))
				     (location (line-location l))))))))))
		(read/rp g (the-port))))
	  (define (read-list read pred)
	     (let loop ((e '()))
		(let ((o (read)))
		   (if (pred o)
		       (let ((c (read-char)))
			  (case c
			     ((#\,)
			      (loop (cons o e)))
			     ((#\;)
			      (reverse! (cons o e)))
			     (else
			      (if (eof-object? c)
				  (reverse! (cons o e))
				  (raise
				   (instantiate::&io-parse-error
				      (proc 'icalendar)
				      (msg (format "Illegal character: " c))
				      (obj o)
				      (fname (line-fname l))
				      (location (line-location l))))))))
		       (raise
			(instantiate::&io-parse-error
			   (proc 'icalendar)
			   (msg (format "Illegal list value: " o))
			   (obj o)
			   (fname (line-fname l))
			   (location (line-location l))))))))
	  (define (read-wday)
	     (let ((o (read)))
		(cond
		   ((symbol? o)
		    (if (memq o '(SU MO TU WE TH FR SA))
			o
			(raise
			 (instantiate::&io-parse-error
			    (proc 'icalendar)
			    (msg (format "Illegal week day: " o))
			    (obj o)
			    (fname (line-fname l))
			    (location (line-location l))))))
		   ((fixnum? o)
		    (if (or (and (>fx o 0) (<fx o 53))
			    (and (<fx o 0) (>fx o -53)))
			(let ((d (read-wday)))
			   (if (symbol? d)
			       (cons o d)
			       (raise
				(instantiate::&io-parse-error
				   (proc 'icalendar)
				   (msg (format "Illegal week day: " o))
				   (obj o)
				   (fname (line-fname l))
				   (location (line-location l))))))))
		   (else
		    (raise
		     (instantiate::&io-parse-error
			(proc 'icalendar)
			(msg (format "Illegal week day: " o))
			(obj o)
			(fname (line-fname l))
			(location (line-location l))))))))
	  (define (read-byweekday-list)
	     (read-list read-wday (lambda (x) #t)))
	  (case s
	     ((FREQ)
	      (let ((t (read-token
			(lambda (x)
			   (memq x '(SECONDLY MINUTELY HOURLY DAILY
					      WEEKLY MONTHLY YEARLY))))))
		 (with-access::calrecurrence rec (frequency)
		    (set! frequency t) )
		 (ignore)))
	     ((COUNT)
	      (with-access::calrecurrence rec (count)
		 (set! count (read-token integer?)))
	      (ignore))
	     ((UNTIL)
	      (with-access::calrecurrence rec (until)
		 (set! until (read-date)))
	      (ignore))
	     ((INTERVAL)
	      (with-access::calrecurrence rec (interval)
		 (set! interval (read-token integer?)))
	      (ignore))
	     ((BYSECOND)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (>= x 0)
				  (<fx x 60)))))
		 (with-access::calrecurrence rec (bysecond)
		    (set! bysecond (read-list read pred))))
	      (ignore))
	     ((BYMINUTE)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (>= x 0)
				  (<fx x 60)))))
		 (with-access::calrecurrence rec (byminute)
		    (set! byminute (read-list read pred))))
	      (ignore))
	     ((BYHOUR)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (>= x 0)
				  (<fx x 24)))))
		 (with-access::calrecurrence rec (byhour)
		    (set! byhour (read-list read pred))))
	      (ignore))
	     ((BYDAY)
	      (with-access::calrecurrence rec (byweekday)
		 (set! byweekday (read-byweekday-list)))
	      (ignore))
	     ((BYMONTHDAY)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (or (and (>fx x 0) (<fx x 32))
				      (and (<fx x 0) (>fx x -32)))))))
		 (with-access::calrecurrence rec (bymonthday)
		    (set! bymonthday (read-list read pred))))
	      (ignore))
	     ((BYYEARDAY)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (or (and (>fx x 0) (<fx x 13))
				      (and (<fx x 0) (>fx x -13)))))))
		 (with-access::calrecurrence rec (byyearday)
		    (set! byyearday (read-list read pred))))
	      (ignore))
	     ((BYWEEKNO)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (or (and (>fx x 0) (<fx x 53))
				      (and (<fx x 0) (>fx x -52)))))))
		 (with-access::calrecurrence rec (byweekno)
		    (set! byweekno (read-list read pred))))
	      (ignore))
	     ((BYMONTH)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (or (and (>fx x 0) (<fx x 13))
				      (and (<fx x 0) (>fx x -13)))))))
		 (with-access::calrecurrence rec (bymonth)
		    (set! bymonth (read-list read pred))))
	      (ignore))
	     ((BYSETPOS)
	      (let ((pred (lambda (x)
			     (and (fixnum? x)
				  (and (>fx x 0) (<fx x 366))))))
		 (with-access::calrecurrence rec (bysetpos)
		    (set! bysetpos (read-list read pred))))
	      (ignore))
	     ((WKST)
	      (let ((l (read-token
			(lambda (x) (memq x '(SU MO TU WE TH FR SA))))))
		 (with-access::calrecurrence rec (wkst)
		    (set! wkst l)))
	      (ignore))
	     (else
	      (raise
	       (instantiate::&io-parse-error
		  (proc 'icalendar)
		  (msg "Illegal RRULE attribute: ")
		  (obj s)
		  (fname (line-fname l))
		  (location (line-location l))))))))
      (#\;
       (ignore))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      rec
	      (raise
	       (instantiate::&io-parse-error
		  (proc 'icalendar)
		  (msg "Illegal character in RRULE value")
		  (obj c)
		  (fname (line-fname l))
		  (location (line-location l)))))))))
      
;*---------------------------------------------------------------------*/
;*    vevent->calevent ...                                             */
;*---------------------------------------------------------------------*/
(define (vevent->calevent b::block evt::calevent)
   (with-access::calevent evt (dtstart dtend
				       summary description recurrence
				       uid attach categories
				       klass location priority ressources
				       status duration related-to url
				       action trigger repeat flush)
      (for-each (lambda (l)
		   (cond
		      ((not (line? l))
		       #f)
		      ((is-line-name? l 'DTSTART)
		       (set! dtstart (icalendar-date->date (line-val l))))
		      ((is-line-name? l 'DTEND)
		       (let ((d (icalendar-date->date (line-val l))))
			  (if (and (=fx (date-hour d) 0)
				   (=fx (date-minute d) 0)
				   (=fx (date-second d) 0))
			      (set! dtend (seconds->date
					   (-elong (date->seconds d) 1)))
			      (set! dtend d))))
		      ((is-line-name? l 'SUMMARY)
		       (set! summary (line-val l)))
		      ((is-line-name? l 'DESCRIPTION)
		       (set! description (line-val l)))
		      ((is-line-name? l 'UID)
		       (set! uid (line-val l)))
		      ((is-line-name? l 'ATTACH)
		       (set! attach (line-val l)))
		      ((is-line-name? l 'CATEGORIES)
		       (set! categories (icalendar-line-list->list l)))
		      ((is-line-name? l 'CLASS)
		       (set! klass (line-val l)))
		      ((is-line-name? l 'LOCATION)
		       (set! location (line-val l)))
		      ((is-line-name? l 'PRIORITY)
		       (set! priority (line-val l)))
		      ((is-line-name? l 'RESSOURCES)
		       (set! ressources (line-val l)))
		      ((is-line-name? l 'STATUS)
		       (set! status (line-val l)))
		      ((is-line-name? l 'DURATION)
		       (set! duration (line-val l)))
		      ((is-line-name? l 'RELATED-TO)
		       (set! related-to (line-val l)))
		      ((is-line-name? l 'URL)
		       (set! url (line-val l)))
		      ((is-line-name? l 'ACTION)
		       (set! action (line-val l)))
		      ((is-line-name? l 'TRIGGER)
		       (set! trigger (line-val l)))
		      ((is-line-name? l 'REPEAT)
		       (set! repeat (line-val l)))
		      ((is-line-name? l 'FLUSH)
		       (set! flush (line-val l)))
		      ((is-line-name? l 'RRULE)
		       (let ((rec (instantiate::calrecurrence)))
			  (with-input-from-string (line-val l)
			     (lambda ()
				(read/rp *rrule-value-lexer*
					 (current-input-port)
					 rec
					 l)))
			  (set! recurrence rec)))))
		(block-body b))
      evt))

;*---------------------------------------------------------------------*/
;*    icalendar-line-list->list ...                                    */
;*---------------------------------------------------------------------*/
(define (icalendar-line-list->list l)
   (let ((g (regular-grammar (l)
	       ((+ (or (out #\,) (: #\\ #\,)))
		(cons (the-string) (ignore)))
	       (#\,
		(ignore))
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       '()
		       (raise
			(instantiate::&io-parse-error
			   (proc 'icalendar)
			   (msg "Illegal character in list value")
			   (obj c)
			   (fname (line-fname l))
			   (location (line-location l))))))))))
      (with-input-from-string (line-val l)
	 (lambda ()
	    (read/rp g (current-input-port) l)))))
      
;*---------------------------------------------------------------------*/
;*    icalendar-date->date ...                                         */
;*---------------------------------------------------------------------*/
(define (icalendar-date->date date)
   (let ((len (string-length date)))
      (if (<fx len 8)
	  (error 'icalendar "Illegal date" date)
	  (let ((year (string->integer (substring date 0 4)))
		(month (string->integer (substring date 4 6)))
		(day (string->integer (substring date 6 8))))
	     (cond
		((=fx len 8)
		 (make-date :year year :month month :day day
			    :hour 0 :min 0 :sec 0))
		((or (not (char=? (string-ref date 8) #\T)) (<fx len 15))
		 (error 'icalendar "Illegal date" date))
		(else
		 (let ((h (string->integer (substring date 9 11)))
		       (m (string->integer (substring date 11 13)))
		       (s (string->integer (substring date 13 15))))
		    (cond
		       ((=fx len 15)
			;; locale time
			(make-date :year year :month month :day day
				   :hour h :min m :sec s))
		       ((and (=fx len 16) (char=? (string-ref date 15) #\Z))
			;; utc time
			(make-date :year year :month month :day day
				   :hour h :min m :sec s
				   :timezone 0))
		       (else
			(error 'icalendar "Illegal date" date))))))))))

;*---------------------------------------------------------------------*/
;*    int4->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (int4->string i)
   (cond
      ((>fx i 999)
       (integer->string i))
      ((>=fx i 100)
       (string-append "0" (integer->string i)))
      ((>=fx i 10)
       (string-append "00" (integer->string i)))
      (else
       (string-append "000" (integer->string i)))))

;*---------------------------------------------------------------------*/
;*    int2->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (int2->string i)
   (case i
      ((0) "00")
      ((1) "01")
      ((2) "02")
      ((3) "03")
      ((4) "04")
      ((5) "05")
      ((6) "06")
      ((7) "07")
      ((8) "08")
      ((9) "09")
      (else (integer->string i))))

;*---------------------------------------------------------------------*/
;*    date->icalendar-date ...                                         */
;*---------------------------------------------------------------------*/
(define (date->icalendar-date date)
   (let ((s (make-string 16 #\0)))
      (string-append (int4->string (date-year date))
		     (int2->string (date-month date))
		     (int2->string (date-day date)))))
	 
;*---------------------------------------------------------------------*/
;*    date->icalendar-date-time ...                                    */
;*---------------------------------------------------------------------*/
(define (date->icalendar-date-time date)
   (let ((s (make-string 16 #\0)))
      (string-append (int4->string (date-year date))
		     (int2->string (date-month date))
		     (int2->string (date-day date))
		     "T"
		     (int2->string (date-hour date))
		     (int2->string (date-minute date))
		     (int2->string (date-second date)))))
	 
		
			      
   

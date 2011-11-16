;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/vcard.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 11 16:34:38 2008                          */
;*    Last change :  Tue Nov 15 20:21:59 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    vCard, rfc2646 - http://tools.ietf.org/html/rfc2426.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_vcard
   
   (import __mail_rfc2045)
   
   (export (class vcard
	      (version::bstring (default "2.1"))
	      (fn (default #f))
	      (familyname (default #f))
	      (firstname (default #f))
	      (face (default #f))
	      (url (default #f))
	      (org (default #f))
	      (emails::pair-nil (default '()))
	      (phones::pair-nil (default '()))
	      (addresses::pair-nil (default '()))
	      (notes::pair-nil (default '())))
	   
	   (port->vcard::vcard ::input-port #!key charset-encoder)
	   (string->vcard::vcard ::bstring #!key charset-encoder)))
   
;*---------------------------------------------------------------------*/
;*    port->vcard ...                                                  */
;*---------------------------------------------------------------------*/
(define (port->vcard iport #!key charset-encoder)
   (let ((line (read-line iport)))
      (if (and (string? line) (string-ci=? line "begin:vcard"))
	  (let ((vcard (instantiate::vcard)))
	     (read/rp vcard-line-grammar iport vcard charset-encoder)
	     vcard)
	  (parse-error "Illegal BEGIN:VCARD" line iport))))

;*---------------------------------------------------------------------*/
;*    string->vcard ...                                                */
;*---------------------------------------------------------------------*/
(define (string->vcard str #!key charset-encoder)
   (let* ((p (open-input-string str))
	  (r (port->vcard p :charset-encoder charset-encoder)))
      (close-input-port p)
      r))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error msg obj port)
   (raise
    (instantiate::&io-parse-error
       (proc 'vcard)
       (msg msg)
       (obj obj)
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    vcard-date->date ...                                             */
;*---------------------------------------------------------------------*/
(define (vcard-date->date date)
   (let ((len (string-length date)))
      (if (<fx len 8)
	  (error 'vcard "Illegal date" date)
	  (let ((year (string->integer (substring date 0 4)))
		(month (string->integer (substring date 4 6)))
		(day (string->integer (substring date 6 8))))
	     (cond
		((=fx len 8)
		 (make-date :year year :month month :day day
			    :hour 0 :min 0 :sec 0))
		((or (not (char=? (string-ref date 8) #\T)) (<fx len 15))
		 (error 'vcard "Illegal date" date))
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
			(error 'vcard "Illegal date" date))))))))))

;*---------------------------------------------------------------------*/
;*    unescape ...                                                     */
;*---------------------------------------------------------------------*/
(define (unescape str cset decode)
   (let ((str2 (if (procedure? cset)
		   (cset str)
		   str)))
      (if (procedure? decode)
	  (decode str2)
	  str2)))

;*---------------------------------------------------------------------*/
;*    read-values ...                                                  */
;*---------------------------------------------------------------------*/
(define (read-values port options cset)
   (let ((g (regular-grammar (cset decode)
	       ((+ (or (out #\return #\newline #\; #\\) "\\n"))
		(let ((val (unescape (the-string) cset decode)))
		   (cons val (ignore))))
	       ((or (: #\return #\Newline) #\Newline)
		'())
	       ((: #\Newline (+ (or #\tab #\space)))
		(let ((val (the-string)))
		   (cons val (ignore))))
	       ((: #\; (+ #\;))
		(let ((len (-fx (the-length) 1)))
		   (append (make-list len "") (ignore))))
	       (#\;
		(ignore))
	       (else
		(parse-error "Illegal values"
			     (read-line (the-port))
			     (the-port))))))
      (if (or (memq 'quoted-printable options)
	      (member '(encoding . "QUOTED-PRINTABLE") options))
	  (read/rp g port cset quoted-printable-decode)
	  (read/rp g port cset #f))))

;*---------------------------------------------------------------------*/
;*    read-options ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-options port)
   (letrec ((optgram (regular-grammar ((ID (+ (uncase (or #\- (in ("az")))))))
			((: ID #\=)
			 (let* ((opt (string-downcase! (the-substring 0 -1)))
				(val (read/rp valgram (the-port))))
			    (cons (cons (string->symbol opt) val) (ignore))))
			((: ID)
			 (let ((opt (the-downcase-symbol)))
			    (cons opt (ignore))))
			(#\:
			 '())
			(#\;
			 (ignore))
			(else
			 (parse-error "Illegal options"
				      (read-line (the-port))
				      (the-port)))))
	    (valgram (regular-grammar ()
			((+ (or (out #\return #\newline #\; #\: #\\) "\\n"))
			 (the-string))
			(else
			 (parse-error "Illegal option value"
				      (read-line (the-port))
				      (the-port))))))
      (read/rp optgram port)))
	       
;*---------------------------------------------------------------------*/
;*    vcard-line-grammar ...                                           */
;*---------------------------------------------------------------------*/
(define vcard-line-grammar
   (regular-grammar ((IDENT (+ (uncase (or #\- (in ("az"))))))
		     vcard cset)
      
      (define (parse-content-line keyword options)
	 (case keyword
	    ((end:)
	     (let ((line (read-line (the-port))))
		(if (string-ci=? line "vcard")
		    vcard
		    (parse-error "Illegal END:VCARD" line (the-port)))))
	    ((fn:)
	     (let ((vals (read-values (the-port) options cset)))
		(with-access::vcard vcard (fn)
		   (set! fn (if (pair? vals) (car vals) #f)))))
	    ((n:)
	     (let ((vals (read-values (the-port) options cset)))
		(when (pair? vals)
		   (with-access::vcard vcard (familyname)
		      (set! familyname (car vals)))
		   (when (pair? (cdr vals))
		      (with-access::vcard vcard (firstname)
			 (set! firstname (cadr vals)))))))
	    ((version:)
	     (with-access::vcard vcard (version)
		(set! version (read-line (the-port)))))
	    ((url:)
	     (with-access::vcard vcard (url)
		(set! url (read-line (the-port)))))
	    ((org:)
	     (with-access::vcard vcard (org)
		(set! org (read-values (the-port) options cset))))
	    ((tel:)
	     (with-access::vcard vcard (phones)
		(let ((num (read-values (the-port) options cset))
		      (loc (if (pair? options)
			       (string-downcase (symbol->string (car options)))
			       "work")))
		   (when (pair? num)
		      (set! phones (cons (list loc (car num)) phones))))))
	    ((adr:)
	     (with-access::vcard vcard (addresses)
		(let ((vals (read-values (the-port) options cset)))
		   (match-case vals
		      ((?po ?eadr ?street ?city ?region ?zip ?country)
		       (set! addresses
			     (list (list po
					 (list street)
					 city region zip country))))))))
	    ((email:)
	     (with-access::vcard vcard (emails)
		(let ((vals (read-values (the-port) options cset)))
		   (set! emails vals))))
	    (else
	     (read-values (the-port) options cset))))
      
      ((: IDENT #\:)
       (parse-content-line (the-downcase-keyword) '())
       (ignore))
      ((: IDENT #\;)
       (parse-content-line (the-downcase-keyword) (read-options (the-port)))
       (ignore))
      ((+ (in #\space #\newline #\return #\tab))
       (ignore))
      (else
       (unless (eof-object? (the-failure))
	  (parse-error "parse error" (read-line (the-port)) (the-port))))))
	   

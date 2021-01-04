;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/vcard.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 11 16:34:38 2008                          */
;*    Last change :  Tue Nov 15 20:21:59 2011 (serrano)                */
;*    Copyright   :  2008-21 Manuel Serrano                            */
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
	      (uid (default #f))
	      (fn (default #f))
	      (familyname (default #f))
	      (firstname (default #f))
	      (nickname (default #f))
	      (photo (default #f))
	      (sound (default #f))
	      (url (default #f))
	      (org (default #f))
	      (emails::pair-nil (default '()))
	      (phones::pair-nil (default '()))
	      (birthday (default #f))
	      (addresses::pair-nil (default '()))
	      (lang (default #f))
	      (related (default #f))
	      (key (default #f))
	      (notes::pair-nil (default '()))
	      (x-thumbnail (default #f))
	      (x-color (default #f))
	      (xx-extras::pair-nil (default '())))
	   
	   (port->vcard ::input-port #!key charset-encoder)
	   (read-vcard ::input-port #!key charset-encoder)
	   (string->vcard::vcard ::bstring #!key charset-encoder)))
   
;*---------------------------------------------------------------------*/
;*    port->vcard ...                                                  */
;*---------------------------------------------------------------------*/
(define (port->vcard iport #!key charset-encoder)
   (let ((line (read-line iport)))
      (unless (eof-object? line)
	 (if (and (string? line) (string-ci=? line "begin:vcard"))
	     (let ((vcard (instantiate::vcard)))
		(read/rp vcard-line-grammar iport vcard charset-encoder)
		vcard)
	     (parse-error "Illegal BEGIN:VCARD" line iport)))))

;*---------------------------------------------------------------------*/
;*    read-vcard ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-vcard iport #!key charset-encoder)
   (let ((line (read-line iport)))
      (if (eof-object? line)
	  line
	  (if (and (string? line) (string-ci=? line "begin:vcard"))
	      (let ((vcard (instantiate::vcard)))
		 (read/rp vcard-line-grammar iport vcard charset-encoder))
	      (parse-error "Illegal BEGIN:VCARD" line iport)))))

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
;*    parse-error-port ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-error-port msg port)
   (let* ((fname (input-port-name port))
	  (location (input-port-position port)))
      (raise
	 (instantiate::&io-parse-error
	    (proc 'vcard)
	    (msg msg)
	    (obj (read-line port))
	    (fname fname)
	    (location location)))))

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
   
   (define base64-grammar
      (regular-grammar ()
	 ((: (or "=" "==" "===" "====") #\Return #\Newline)
	  (list (the-substring 0 -2)))
	 ((: (or "=" "==" "===" "====") #\Newline)
	  (list (the-substring 0 -1)))
	 ((+ (out #\Newline #\Return #\space #\=))
	  (let ((str (the-string)))
	     (cons str (ignore))))
	 ((+ (or #\Newline #\Return #\space))
	  (ignore))
	 (else
	  (parse-error "Illegval value"
	     (read-line (the-port))
	     (the-port)))))
   
   (define g
      (regular-grammar (cset decode)
	 ((+ (or (out #\return #\newline #\; #\\) "\\n"))
	  (let ((val (unescape (the-string) cset decode)))
	     (cons val (ignore))))
	 ((or (: #\return #\Newline) #\Newline)
	  '())
	 ((: #\Newline (+ (or #\tab #\space)))
	  (let ((val (the-string)))
	     (cons val (ignore))))
	 ((: (: #\return #\Newline) (+ (or #\tab #\space)))
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
	     (the-port)))))
   
   (cond
      ((or (memq 'quoted-printable options)
	   (member '(encoding . "QUOTED-PRINTABLE") options))
       (read/rp g port cset quoted-printable-decode))
      ((or (memq 'base64 options)
	   (member '(encoding . "BASE64") options))
       (apply string-append (read/rp base64-grammar port)))
      (else
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
			 (parse-error-port "Illegal option value"
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
		  'end
		  (parse-error-port "Illegal END:VCARD" (the-port)))))
	  ((uid:)
	   (let ((vals (read-values (the-port) options cset)))
	      (with-access::vcard vcard (uid)
		 (set! uid (if (pair? vals) (uid vals) #f)))))
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
	  ((nickname:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (nickname)
		    (set! nickname (car vals))))))
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
		    (lbl (if (pair? options)
			     (string-downcase (symbol->string (car options)))
			     "default")))
		 (when (pair? num)
		    (set! phones (cons (list lbl (car num)) phones))))))
	  ((adr:)
	   (with-access::vcard vcard (addresses)
	      (let ((vals (read-values (the-port) options cset))
		    (lbl (if (pair? options)
			     (string-downcase (symbol->string (car options)))
			     "home")))
		 (match-case vals
		    ((?po ?ext ?street ?city ?region ?zip ?country)
		     (set! addresses
			(cons `(:label, lbl
				  :pobox ,po :ext ,ext :street ,street
				  :city ,city :region ,region
				  :zip ,zip :country ,country)
			   addresses)))
		    ((?ext ?street ?city ?region ?zip ?country)
		     (set! addresses
			(cons `(:label ,lbl
				  :ext ,ext
				  :street ,street
				  :city ,city :region ,region
				  :zip ,zip
				  :country ,country)
			   addresses)))
		    ((?po ?street ?city ?region ?country)
		     (set! addresses
			(cons `(:label ,lbl
				  :pobox ,po :street ,street
				  :city ,city :region ,region
				  :country ,country)
			   addresses)))
		    
		    (else
		     (tprint (format "ADDRE PAS BON: ~s" vals)))))))
	  ((email:)
	   (with-access::vcard vcard (emails)
	      (let ((vals (read-values (the-port) options cset)))
		 (set! emails vals))))
	  ((photo:)
	   (with-access::vcard vcard (photo)
	      (let ((vals (read-values (the-port) options cset)))
		 (set! photo (cons options vals)))))
	  ((bday:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (birthday)
		    (set! birthday (car vals))))))
	  ((sound:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (sound)
		    (set! sound (car vals))))))
	  ((related:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (related)
		    (set! related (car vals))))))
	  ((lang:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (lang)
		    (set! lang (car vals))))))
	  ((key:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (key)
		    (set! key (car vals))))))
	  ((note:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (notes)
		    (set! notes (car notes))))))
	  ((x-color:)
	   (let ((vals (read-values (the-port) options cset)))
	      (when (pair? vals)
		 (with-access::vcard vcard (x-color)
		    (set! x-color (car vals))))))
	  ((x-thumbnail:)
	   (with-access::vcard vcard (x-thumbnail)
	      (let ((vals (read-values (the-port) options cset)))
		 (set! x-thumbnail (cons options vals)))))
	  (else
	   (with-access::vcard vcard (xx-extras)
	      (let ((vals (read-values (the-port) options cset)))
		 (when (pair? vals)
		    (set! xx-extras
		       (cons (cons keyword vals) xx-extras))))))))
      
      ((: IDENT #\:)
       (let ((r (parse-content-line (the-downcase-keyword) '())))
	  (if (eq? r 'end)
	      vcard
	      (ignore))))
      ((: IDENT #\;)
       (parse-content-line (string->keyword
			      (string-downcase! (the-substring 0 -1)))
	  (read-options (the-port)))
       (ignore))
      ((+ (in #\space #\newline #\return #\tab))
       (ignore))
      (else
       (unless (eof-object? (the-failure))
	  (parse-error-port "parse error" (the-port))))))
	   

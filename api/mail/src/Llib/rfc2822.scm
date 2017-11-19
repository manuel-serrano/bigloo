;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/rfc2822.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 30 12:51:46 2007                          */
;*    Last change :  Fri Nov 17 08:44:31 2017 (serrano)                */
;*    Copyright   :  2007-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements parser following the RFC2822              */
;*    (Internet Message Format) specification.                         */
;*    RFC2822, may be found at:                                        */
;*      http://tools.ietf.org/html/rfc2822                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_rfc2822

   (export (mail-header->list::pair-nil ::obj)
	   (email-normalize::bstring ::bstring)

	   (rfc2822-address-display-name::bstring ::bstring)))
 
;*---------------------------------------------------------------------*/
;*    mail-header->list ...                                            */
;*---------------------------------------------------------------------*/
(define (mail-header->list header)

   (define (safe-char c)
      (if (char=? c #\Return)
	  "{\\r}"
	  (string #\{ c #\})))
   
   (define value-grammar
      (regular-grammar ()
	 ((+ (or (out "\r\n") (: "\r" (out "\n"))))
	  (the-string))
	 ((: (+ (or (out "\r\n") (: "\r" (out "\n"))))
	     (? "\r") "\n" (+ (in " \t")))
	  (let ((s1 (the-string)))
	     (string-append s1 (ignore))))
	 ((: (or "\r\n" "\n") (+ (in " \t")))
	  (ignore))
	 ((or "\r\n" "\n")
	  "")
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 ""
		 (raise (instantiate::&io-parse-error
			   (proc "mail-header->list")
			   (msg "Illegal value character")
			   (obj (string-append (safe-char c)
				   (read-line (the-port)))))))))))

   (define (trim s)
      (let ((i (string-skip s #\space)))
	 (if (or (not i) (=fx i 0))
	     s
	     (substring s i))))
   
   (define field-grammar
      (regular-grammar ((id (+ (out ":\n\t\r ,;"))))
	 ((bol (: id ":" (? " ")))
	  (let* ((len (the-length))
		 (bref (the-byte-ref (-fx len 1)))
		 (delta (if (= bref (char->integer #\Space)) 2 1))
		 (id (string->symbol
			(string-downcase!
			   (the-substring 0 (-fx len delta)))))
		 (val (trim (read/rp value-grammar (the-port)))))
	     (with-handler
		(lambda (e)
		   (raise
		      (with-access::&error e (obj)
			 (duplicate::&io-parse-error e
			    (obj (cons (cons id val) obj))))))
		(cons (cons id val) (ignore)))))
	 ((bol (: id ": " (= 2 #\Newline)))
	  (let ((id (string->symbol
		       (string-downcase!
			  (the-substring 0 (-fx (the-length) 4))))))
	     (list (cons id ""))))
	 ((bol (: id ": " (= 2 (: #\Return #\Newline))))
	  (let ((id (string->symbol
		       (string-downcase!
			  (the-substring 0 (-fx (the-length) 6))))))
	     (list (cons id ""))))
	 ((or (= 2 #\Newline) (= 2 (: #\Return #\Newline)))
	  '())
	 ((bol (or #\Newline (: #\Return #\Newline)))
	  '())
	 ((or #\Return #\Newline)
	  (ignore))
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 '()
		 (let ((line (read-line (the-port))))
		    (cond
		       ((and (or (char=? c #\F) (char=? c #\f))
			     (substring-at? line "rom " 0))
			(ignore))
		       ((eof-object? line)
			(raise (instantiate::&io-parse-error
				  (proc "mail-header->list")
				  (msg "Premature end of file")
				  (obj c))))
		       (else
			(raise (instantiate::&io-parse-error
				  (proc "mail-header->list")
				  (msg "Illegal field character")
				  (obj (string-append (safe-char c)
					  line))))))))))))

   (cond
      ((string? header)
       (with-input-from-string header
	  (lambda ()
	     (read/rp field-grammar (current-input-port)))))
      ((input-port? header)
       (read/rp field-grammar header))
      (else
       (bigloo-type-error "mail-header->list" "string or input-port" header))))
		 
;*---------------------------------------------------------------------*/
;*    email-normalize ...                                              */
;*---------------------------------------------------------------------*/
(define (email-normalize from)
   (let ((len (string-length from)))
      (cond
	 ((<=fx len 1)
	  from)
	 ((char=? (string-ref from (-fx len 1)) #\>)
	  (let liip ((i (-fx len 1)))
	     (cond
		((=fx i 0)
		 from)
		((char=? (string-ref from i) #\<)
		 (substring from (+fx i 1) (-fx len 1)))
		(else
		 (liip (-fx i 1))))))
	 ((char=? (string-ref from (-fx len 1)) #\))
	  (let liip ((i (-fx len 1)))
	     (cond
		((=fx i 0)
		 from)
		((char=? (string-ref from i) #\()
		 (let loop ((i i))
		    (cond
		       ((=fx i 0)
			from)
		       ((char-whitespace? (string-ref from i))
			(loop (-fx i 1)))
		       (else
			(substring from 0 (-fx i 1))))))
		(else
		 (liip (-fx i 1))))))
	 (else
	  (let loop ((i 0))
	     (cond
		((=fx i len)
		 "")
		((char-whitespace? (string-ref from i))
		 (loop (+fx i 1)))
		(else
		 (substring from i len))))))))

;*---------------------------------------------------------------------*/
;*    rfc2822-address-display-paren-name ...                           */
;*---------------------------------------------------------------------*/
(define (rfc2822-address-display-paren-name address i c)
   (let ((end (string-index address c i)))
      (if (not end)
	  address
	  (substring address (+fx i 1) end))))

;*---------------------------------------------------------------------*/
;*    rfc2822-address-display-bracket-name ...                         */
;*---------------------------------------------------------------------*/
(define (rfc2822-address-display-bracket-name address i)
   (if (char=? (string-ref address 0) #\")
       (let ((end (string-index-right address #\")))
	  (if (and end (>fx end 0))
	      (substring address 1 end)
	      (let ((j (string-skip-right address " \t" i)))
		 (substring address 0 (+fx j 1)))))
       (let ((j (string-skip-right address " \t" i)))
	  (substring address 0 (+fx j 1)))))

;*---------------------------------------------------------------------*/
;*    rfc2822-address-display-name ...                                 */
;*---------------------------------------------------------------------*/
(define (rfc2822-address-display-name address)
   (let ((i (string-index address "<")))
      (if (or (not i) (=fx i 0))
	  (let ((j (string-index address "(")))
	     (if (or (not j) (=fx j 0))
		 ;; try a last heuritic
		 (let ((i1 (string-index address #\.)))
		    (if i1
			(let ((i2 (string-index address #\@)))
			   (if (and i2 (<fx i1 i2))
			       (let ((s (if (char=? (string-ref address 0) #\<)
					    (substring address 1 i2)
					    (substring address 0 i2))))
				  (string-replace! s #\. #\space))
			       (if i
				   (rfc2822-address-display-paren-name
				    address i #\>)
				   address)))
			(if i
			    (rfc2822-address-display-paren-name address i #\>)
			    address)))
		 (rfc2822-address-display-paren-name address j #\))))
	  (rfc2822-address-display-bracket-name address i))))

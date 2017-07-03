;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library jabber
	    pthread)
   (main    main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    do-something-else ...                                            */
;*---------------------------------------------------------------------*/
(define (do-something-else)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
	     (print "error.")
	     (print "   ==> provided: [" (with-output-to-string
					    (lambda () (write provided)))
		    "]\n       expected: ["
		    (let ((r (if (procedure? res) (res 'result) res)))
		       (with-output-to-string
			  (lambda () (write r))))
		    "]")))))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))

;*---------------------------------------------------------------------*/
;*    cond-expand ...                                                  */
;*---------------------------------------------------------------------*/
(define-test cond-expand
   (cond-expand
      (jabber #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    create user account                                              */
;*---------------------------------------------------------------------*/
(define-test create-user-account
  (let ((connection (open-connection "jabber.fr" :register? #t)))
    (if connection
	(let ((register? (register "bigloo" "bigloo" "jabber.fr" connection)))
	        (if register?
		    (print "user account create")
		    (print "user account not create")))
	(print "connection fail"))))

;*---------------------------------------------------------------------*/
;*   identification on jabber server                                   */
;*---------------------------------------------------------------------*/
(define-test ident-on-server
  (let ((connection  (open-connection "jabber.fr")))
    (if connection
	(let ((ident? (identification "bigloo" "bigloo" connection)))
	  (if ident? 
	      (print "user connected")
	      (print "identification fail")))
	(print "connection fail"))))

;*---------------------------------------------------------------------*/
;*   add new contact                                                   */
;*---------------------------------------------------------------------*/
(define-test add-new-contact 
  (let ((connection  (open-connection "jabber.fr")))
    (if connection
	(let ((ident? (identification "bigloo" "bigloo" connection)))
	  (if ident? 
	      (begin (print "user connected")
		     (let ((add? (add "bigloo@jabber.org" connection)))
		       (if add?
			   (print "contact added")
			   (print "contact not added")))
		      (close-connection connection))
	      (print "identification fail")))
	(print "connection fail"))))

;*---------------------------------------------------------------------*/
;*   display contacts                                                  */
;*---------------------------------------------------------------------*/
(define-test display-contacts
  (let ((connection  (open-connection "jabber.fr")))
    (if connection
	(let ((ident? (identification "bigloo" "bigloo" connection)))
	  (if ident?
	       (begin (print "user connected")
		      (let ((contacts-list (get-contacts connection)))
			(map (lambda(contact) 
			       (map (lambda (attr) (print (car attr) ": " (cdr attr))) 
				    contact) 
			       (print "----------------------"))
			     contacts-list))
		       (close-connection connection))
	       (print "identification fail")))
	(print "connection fail"))))

;*---------------------------------------------------------------------*/
;*   receive and send messages                                         */
;*---------------------------------------------------------------------*/
(define-test rcv-send
  (let ((connection  (open-connection "jabber.fr" :message (lambda(from text html conn) (print from " : " text)))))
    (if connection
	(let ((ident? (identification "bigloo" "bigloo" connection)))
	  (if ident?
	      (begin (print "user connected")
		     (let loop ((line (read-line)))
		       (let* ((space (pregexp-match-positions " " line))(to (and space (substring line 0 (caar space))))(text (and space (substring line (caar space) (string-length line)))))
			 (cond  ((string=? line "quit") #t)
				((and to text)  (sendMessage text to connection) (loop (read-line)))
				((and to (string=? to "quit")) #t)
				(else  (loop (read-line))))))
		     (close-connection connection))
	        (print "identification fail")))
	(print "connection fail"))))		     

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (for-each (lambda (pvn)
		  (apply test pvn))
		(if (null? tests)
		    (reverse *tests*)
		    (reverse (filter (lambda (t) (memq (car t) tests))
				     *tests*))))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/text/recette/recette.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Fri Aug 18 18:44:42 2017 (serrano)                */
;*    Copyright   :  2002-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of Text.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library text)
   (main main))

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
      (text #t)
      (else #f))
   :result #t)

;*---------------------------------------------------------------------*/
;*    en-hyphens-path ...                                              */
;*---------------------------------------------------------------------*/
(define (en-hyphens-path)
   (unless (file-exists? "../data/en-hyphens.sch")
      (error "text" "wrong execution path, hyphen file does not exists"
	 "../data/en-hyphens.sch"))
   "../data/en-hyphens.sch")

;*---------------------------------------------------------------------*/
;*    hyphens ...                                                      */
;*---------------------------------------------------------------------*/
(define-test hyphens
   (begin (load-hyphens (en-hyphens-path)) #t)
   :result #t)

;*---------------------------------------------------------------------*/
;*    hyphenate-english ...                                            */
;*---------------------------------------------------------------------*/
(define-test hyphenate-english
   (let ((h (load-hyphens (en-hyphens-path))))
      (map (lambda (w)
	      (hyphenate w h))
	   '("This" "program" "is" "free" "software" "you" "can"
	     "redistribute" "it" "and/or" "modify" "it" "under" "the"
	     "terms" "of" "the" "GNU" "General" "Public" "License")))
   :result '(("This") ("pro" "gram") ("is") ("free") ("soft" "ware") ("you")
	     ("can") ("re" "dis" "tribute") ("it") ("and/or") ("mod" "i" "fy")
	     ("it") ("un" "der") ("the") ("terms") ("of") ("the") ("GNU")
	     ("Gen" "er" "al") ("Pub" "lic") ("Li" "cense")))

;*---------------------------------------------------------------------*/
;*    gb2312 ...                                                       */
;*---------------------------------------------------------------------*/
(define-test gb2312
   (gb2312->ucs2 (base64-decode "VmVyc2lvbiBwYXMgbWFsIHJlc3NlcnKopmUsIGV0IGplIHBlbnNlIA=="))
   :result (utf8-string->ucs2-string "Version pas mal resserr\303\251e, et je pense "))

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
      (unwind-protect
	 (for-each (lambda (pvn)
		      (apply test pvn))
		   (if (null? tests)
		       (reverse *tests*)
		       (reverse (filter (lambda (t) (memq (car t) tests))
					*tests*))))
	 (when (file-exists? "test.db") (delete-file "test.db")))
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

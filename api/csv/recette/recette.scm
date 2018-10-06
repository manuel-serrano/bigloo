;;;; Copyright(c) 2011, 2012 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of bigloo-csv.
;;;;
;;;;     bigloo-csv is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     bigloo-csv is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with bigloo-csv.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
(module testcsv
   (library csv)
   (main main))




;;;; testing infrastructure copied from the recette for Bigloo's pthread library

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
	     (print "   ==> provided: [" provided
		    "]\n       expected: ["
		    (if (procedure? res) (res 'result) res)
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


;;;; cond-expand	   
(define-test cond-expand 
   (cond-expand	   
      (csv #t) 
      (else #f))	   
   :result #t)



(define (csv-record=? r1 r2)
   (and (= (length r1)
	   (length r2))
	(let loop ((c1 r1)
		   (c2 r2))
	   (if (null? c1)
	       #t
	       (if (string=? (car c1) (car c2))
		   (loop (cdr c1) (cdr c2))
		   #f)))))


(define-test no-quotes
   (let* ((test-string "dog,cat,horse,pig\n")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "horse" "pig")
		  (csv-record=? v '("dog" "cat" "horse" "pig")))))


(define-test single-line-no-newline
   (let* ((test-string "dog,cat,horse,pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "horse" "pig")
		  (csv-record=? v '("dog" "cat" "horse" "pig")))))

(define-test just-newline
   (let* ((test-string "\n")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '()
		  (csv-record=? v '()))))

(define-test empty
   (let* ((test-string "")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "the end-of-file object"
		  (eof-object? v))))

(define-test escaped-newline
   (let* ((test-string "dog,cat,\"horse\n\n\",pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "horse\n\n" "pig")
		  (csv-record=? v '("dog" "cat" "horse\n\n" "pig")))))

(define-test escaped-quote
   (let* ((test-string "dog,cat,\"\"\"horse\"\"\",pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "\"horse\"" "pig")
		  (csv-record=? v '("dog" "cat" "\"horse\"" "pig")))))

(define-test multiple-records
   (let* ((test-string "dog,cat,horse,pig\npig,horse,cat,dog")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-records in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '(("dog" "cat" "horse" "pig")
		    ("pig" "horse" "cat" "dog"))
		  (every csv-record=? v '(("dog" "cat" "horse" "pig")
					  ("pig" "horse" "cat" "dog"))))))


(define +psv-lexer+ (make-csv-lexer #\# #\"))

(define-test custom-lexer-separator
   (let* ((test-string "dog#cat#horse#pig\npig#horse#cat#dog")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-records in +psv-lexer+)
       	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '(("dog" "cat" "horse" "pig")
		    ("pig" "horse" "cat" "dog")) 
		  (every csv-record=? v '(("dog" "cat" "horse" "pig")
					  ("pig" "horse" "cat" "dog"))))))

(define +psv2-lexer+ (make-csv-lexer #\# #\$))     
(define-test custom-quote
   (let* ((test-string "dog#cat#$horse $#pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in +psv2-lexer+)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "horse " "pig")
		  (csv-record=? v '("dog" "cat" "horse " "pig")))))

(define-test custom-escaped-quote
   (let* ((test-string "dog#cat#$$$horse$$$#pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in +psv2-lexer+)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "cat" "$horse$" "pig")
		  (csv-record=? v '("dog" "cat" "$horse$" "pig")))))


(define-test csv-for-each
   (with-output-to-string
      (lambda ()
	 (let* ((test-string "dog,cat\nhorse,pig")
		(in (open-input-string test-string)))
	    (unwind-protect
	       (csv-for-each (lambda (r)
				(print r))
		  in)
	       (close-input-port in)))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "(dog cat)\n(horse pig)\n"
		  (string=? v "(dog cat)\n(horse pig)\n"))))


(define-test csv-map
   (let* ((test-string "dog,cat\nhorse,pig")
		(in (open-input-string test-string)))
	    (unwind-protect
	       (csv-map (lambda (r)
				(map (lambda (s) (string-upcase s)) r))
		  in)
	       (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '(("DOG" "CAT")("HORSE" "PIG"))
		  (every csv-record=? v
		     '(("DOG" "CAT")("HORSE" "PIG"))))))


(define-test unclosed-quote
   (let* ((test-string "dog,cat,\"horse")
		(in (open-input-string test-string)))
      (with-handler (lambda (e)
		       (if (isa? e &io-parse-error) 
			   #t #f))
		    (unwind-protect
		       (read-csv-record in)
		       (close-input-port in))))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "an &io-parse-error exception"
		  (eq? v #t))))

(define-test invalid-input-port
   (with-handler (lambda (e)
		    (if (isa? e &io-port-error)
		    #t #f))
		 (let* ((in #unspecified))
		    (read-csv-record in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "an &io-port-error exception"
		  (eq? v #t))))


(define-test quoted-empty-field
   (let* ((test-string "\"\",\"\",\"\"")
	  (in (open-input-string test-string)))
       (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a list of three empty strings"
		  (equal? v (list "" "" "")))))



(define-test custom-quoted-empty-field
   (let* ((test-string "$$#$$#$$")
	  (in (open-input-string test-string)))
       (unwind-protect
	 (read-csv-record in +psv2-lexer+)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  "a list of three empty strings"
		  (equal? v (list "" "" "")))))

(define-test space-in-fields-without-quotes
   (let* ((test-string "dog , cat ,pig")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog " " cat " "pig")
		  (csv-record=? v '("dog " " cat " "pig")))))

(define-test space-after-sep-and-before-quote
   (let* ((test-string "\"dog\", \"cat\"")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" " cat")
		  (csv-record=? v '("dog" " cat")))))


(define-test space-after-quote-before-sep
   (let* ((test-string "\"dog\" ,\"cat\"")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog " "cat")
		  (csv-record=? v '("dog " "cat")))))

(define-test quotes-with-spaces-around-sep
   (let* ((test-string "\"dog\" , \"cat\"")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog " " cat")
		  (csv-record=? v '("dog " " cat")))))

(define-test empty-fields
   (let* ((test-string "dog,,,cat")
	  (in (open-input-string test-string)))
      (unwind-protect
	 (read-csv-record in)
	 (close-input-port in)))
   :result (lambda (v)
	      (if (eq? v 'result)
		  '("dog" "" "" "cat")
		  (csv-record=? v '("dog" "" ""  "cat")))))




;; runs parse-args several times over the arguments.
;; each time only the 'selected' clauses are evaluated.
;; default-pass selects all clauses that have no explicit pass identifier.
;; each clause may have a clause-identifier as follows:
;;      (("-help2" (help "The exhaustive help message"))
;;       (pass 2 (usage args-parse-usage 2 #f)))
;; passes are compared by 'equal?'. So passes can be numbers, symbols, strings,
;; etc.
;; sample invocation:
;;  (pass-parse-args (1 2 default) default
;;      args
;;      (section "Misc")
;;      ;; priliminary test
;;      (("-" (help "Read source code on current input channel"))
;;       (pass 1 (set! *src-files* (cons 'stdin *src-files*))))
;;      (("-o" ?file (help "Name the output FILE"))
;;       (pass 2 (set! *dest* file)))
;;      ;; help
;;      (("?")
;;       (usage args-parse-usage 1 #f)))

(define-macro (pass-args-parse passes default-pass
			       args . clauses)
   (define *pass* (gensym 'pass))

   (define (clause->pass-clause clause)
      (match-case clause
	 ;; a clause with explicit pass information.
	 ;; something like:
	 ;; (("-help2" (help "The exhaustive help message"))
	 ;;  (pass 2 (usage args-parse-usage 2 #f)))
	 ;; or
	 ;; (else (pass 2 ...))
	 (((and (or else
		    (? pair?))
		?arg/help)
	   (pass ?pass . ?body-exprs))
	  `(,arg/help (when (equal? ,pass ,*pass*)
			 ,@body-exprs)))
	 ;; a clause without explicit pass information.
	 ;; something like:
	 ;; (("-help2" (help "The exhaustive help message"))
	 ;;  (usage args-parse-usage 2 #f))
	 ;; or
	 ;; (else ...)
	 (((and (or else
		    (? pair?))
		?arg/help)
	  . ?body-exprs)
	  `(,arg/help (when (equal? ',default-pass ,*pass*)
			 ,@body-exprs)))
	 (else ;; just leave it untouched. (maybe a 'section clause?)
	  clause)))

   (when (not (list? passes))
      (error 'pass-parse-args
	     "first argument must be a list of passes. eg: (1 2 default)"
	     passes))
   `(for-each (lambda (,*pass*)
		 (args-parse
		  ,args
		  ,@(map clause->pass-clause clauses)))
	      ',passes))

(define-syntax packrat-parser
  (syntax-rules (<- quote ! @ /)
    ((_ start (nonterminal (alternative body0 body ...) ...) ...)
     (let ()
       (define nonterminal
	 (lambda (results)
	   (results->result results 'nonterminal
			    (lambda ()
			      ((packrat-parser #f "alts" nonterminal
					       ((begin body0 body ...) alternative) ...)
			       results)))))
       ...
       start))

    ((_ #f "alts" nt (body alternative))
     (packrat-parser #f "alt" nt body alternative))

    ((_ #f "alts" nt (body alternative) rest0 rest ...)
     (packrat-or (packrat-parser #f "alt" nt body alternative)
		 (packrat-parser #f "alts" nt rest0 rest ...)))

    ((_ #f "alt" nt body ())
     (lambda (results) (make-result body results)))

    ((_ #f "alt" nt body ((! fails ...) rest ...))
     (packrat-unless (string-append "Nonterminal " (symbol->string 'nt)
				    " expected to fail "
				    (object->external-representation '(fails ...)))
		     (packrat-parser #f "alt" nt #t (fails ...))
		     (packrat-parser #f "alt" nt body (rest ...))))

    ((_ #f "alt" nt body ((/ alternative ...) rest ...))
     (packrat-check (packrat-parser #f "alts" nt (#t alternative) ...)
		    (lambda (result) (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- 'val rest ...))
     (packrat-check-base 'val
			 (lambda (var)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (var <- @ rest ...))
     (lambda (results)
       (let ((var (parse-results-position results)))
	 ((packrat-parser #f "alt" nt body (rest ...)) results))))

    ((_ #f "alt" nt body (var <- val rest ...))
     (packrat-check val
		    (lambda (var)
		      (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body ('val rest ...))
     (packrat-check-base 'val
			 (lambda (dummy)
			   (packrat-parser #f "alt" nt body (rest ...)))))

    ((_ #f "alt" nt body (val rest ...))
     (packrat-check val
		    (lambda (dummy)
		      (packrat-parser #f "alt" nt body (rest ...)))))))



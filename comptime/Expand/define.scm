;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Expand/define.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:56:53 1994                          */
;*    Last change :  Thu Aug 30 11:51:57 2018 (serrano)                */
;*    Copyright   :  1994-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `define' forms                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_define
   (include "Expand/expander.sch"
	    "Tools/trace.sch")
   (import  tools_progn
	    tools_args
	    tools_error
	    tools_speek
	    tools_misc
	    tools_location
	    expand_expander
	    expand_eps
	    expand_lambda
	    engine_param
	    type_type
	    type_cache
	    ast_ident)
   (export  (expand-define  ::obj ::procedure)
	    (expand-method  ::obj ::procedure)
	    (expand-inline  ::obj ::procedure)
	    (expand-generic ::obj ::procedure)
	    (expand-set!    ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-define ...                                                */
;*    -------------------------------------------------------------    */
;*    on divise en deux sous:                                          */
;*       1- on define une lambda.                                      */
;*       2- on define une valeur (autre qu'un lambda).                 */
;*---------------------------------------------------------------------*/
(define (expand-define x e)
   (trace expand "expand-define: " x
	  " " (if internal-definition? "[internal]" "[external]")
	  #\Newline)
   (if internal-definition?
       (expand-internal-define x e)
       (expand-external-define x e)))

;*---------------------------------------------------------------------*/
;*    expand-external-define ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-external-define x e)
   (set! internal-definition? #t)
   (let ((res (match-case x
		 ;; 1- on definit une lambda 
		 ((or (?- ((and (? symbol?) ?name) . ?args) .
			  (and ?body (not ())))
		      (?- (and (? symbol?) ?name)
			  (lambda ?args . (and ?body (not ())))))
		  (do-external-define-lambda e name args body x))
		 ;; 3- on definit une valeur non typee
		 ((?-  (and (? symbol?) ?name) . (and ?value (not ())))
		  (do-external-define-value e name value x))
		 ;; 3b- on definit une valeur typee
		 (else
		  (error #f "Illegal `define' form" x)))))
      (set! internal-definition? #f)
      (replace! x res)))

;*---------------------------------------------------------------------*/
;*    expand-internal-define ...                                       */
;*---------------------------------------------------------------------*/
(define (expand-internal-define x e)
   (let ((e (internal-begin-expander e)))
      (match-case x
	 ;; 1- (define (foo ..) ...) form
	 ((?- (?name . ?args) . (and ?body (not ())))
	  (if (not (symbol? name))
	      (error "define" "Illegal `define' form" x)
	      (let* ((loc     (find-location x))
		     (pid     (parse-id name loc))
		     (name-id (car pid))
		     (type    (cdr pid))
		     (type-id (type-id type)))
		 (with-lexical
		  (cons name (args*->args-list args))
		  '_
		  loc
		  (lambda ()
		     (let* ((loc (find-location/loc (cddr x)
						    (find-location x)))
			    (nbody `(,(if (eq? type (get-default-type))
					  'lambda
					  (make-typed-ident 'lambda type-id))
				     ,args
				     ,(e (normalize-progn/loc body loc) e)))
			    (enbody (epairify nbody body)))
			(replace! x `(define ,name-id ,enbody))))))))
	 ;; 1b- (define foo (lambda ...)) form
	 ((?- (and (? symbol?) ?name)
	      (and ?value ((and ?lam (? symbol?)) ?args . (and ?body (not ())))))
	  (let ((loc (find-location x)))
	     (if (eq? (fast-id-of-id lam loc) 'lambda)
		 (if (not (symbol? name))
		     (error "define" "Illegal `define' form" x)
		     (let* ((pid     (parse-id name loc))
			    (name-id (car pid))
			    (type    (cdr pid))
			    (type-id (type-id type)))
			(with-lexical
			 (cons name (args*->args-list args))
			 '_
			 loc
			 (lambda ()
			    (let* ((loc (find-location/loc value
							   (find-location x)))
				   (nbody `(,(if (eq? type (get-default-type))
						 'lambda
						 (make-typed-ident 'lambda
								   type-id))
					    ,(expand-args args e)
					    ,(e (normalize-progn/loc body loc) e)))
				   (enbody (epairify nbody body)))
			       (replace! x `(define ,name-id ,enbody)))))))
		 (let* ((nvalue (e value e))
			(envalue (if (pair? nvalue)
				     (epairify nvalue value)
				     nvalue)))
		    (replace! x `(define ,name ,envalue))))))
	 ;; 2- a value binding
	 ((?- (and (? symbol?) ?name) . (and ?value (not ())))
	  (let* ((loc (find-location/loc (cddr x) (find-location x)))
		 (nvalue (e (normalize-progn/loc value loc) e))
		 (envalue (if (pair? nvalue)
			      (epairify nvalue value)
			      nvalue)))
	     (replace! x `(define ,name ,envalue))))
	 (else
	  (error #f "Illegal `define' form" x)))))

;*---------------------------------------------------------------------*/
;*    expand-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-set! x e)
   (define (internal-expand-set! x e)
      (match-case x
	 ((?- (and (? symbol?) ?var) ?value)
	  ;; on test si la variable est liee quelque part
	  (enter-function var)
	  (let ((ev (e value e)))
	     (leave-function)
	     (replace! x `(set! ,var ,ev))))
	 ((?- (and (@ (and ?id (? symbol?)) (? symbol?)) ?var) ?value)
	  ;; on test si la variable est liee quelque part
	  (enter-function id)
	  (let ((ev (e value e)))
	     (leave-function)
	     (replace! x `(set! ,var ,ev))))
	 ((?- (and ?fa (-> . ?-)) ?value)
	  (let ((ev (e value e))
		(efa (e fa e)))
	     (replace! x `(set! ,efa ,ev))))
	 (else
	  (error #f "Illegal `set!' form" x))))
   (if internal-definition?
       (internal-expand-set! x e)
       (begin
	  (set! internal-definition? #t)
	  (let ((res (internal-expand-set! x (internal-begin-expander e))))
	     (set! internal-definition? #f)
	     (replace! x res)))))

;*---------------------------------------------------------------------*/
;*    expand-method ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-method x e)
   (match-case x
      ((?kw ((and (? symbol?) ?id) . ?args) . (and ?body (not ())))
       (with-lexical
	(args*->args-list args)
	'_
	(find-location x)
	(lambda ()
	   (replace! x (do-inline/generic/method kw e id id args body x)))))
      (else 
       (error #f
	      (string-append "Illegal `define-method' form")
	      x))))

;*---------------------------------------------------------------------*/
;*    expand-inline ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-inline x e)
   (if internal-definition?
       (expand-internal-define x e)
       (expand-external-define-inline x e)))

;*---------------------------------------------------------------------*/
;*    expand-external-define-inline ...                                */
;*---------------------------------------------------------------------*/
(define (expand-external-define-inline x e)
   (match-case x
      ((?kw ((and ?id (or (and (? symbol?) ?name)
			  (@ (and (? symbol?) ?name) (? symbol?)))) . ?args)
	  . (and ?body (not ())))
       (with-lexical
	  (args*->args-list args)
	  '_
	  (find-location x)
	  (lambda ()
	     (replace! x (do-inline/generic/method kw e id name args body x)))))
      (else
       (error #f
	  (string-append "Illegal `define-inline' form")
	  x))))

;*---------------------------------------------------------------------*/
;*    expand-generic ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-generic x e)
   (match-case x
      ((?kw ((and ?id (or (and (? symbol?) ?name)
			  (@ (and (? symbol?) ?name) (? symbol?)))) . ?args)
	    . ?body)
       (with-lexical
	(args*->args-list args)
	'_
	(find-location x)
	(lambda ()
	   (replace! x (do-inline/generic/method kw e id name args body x)))))
      (else
       (error #f "Illegal `define-generic' form" x))))

;*---------------------------------------------------------------------*/
;*    do-external-define-lambda ...                                    */
;*---------------------------------------------------------------------*/
(define (do-external-define-lambda e name::symbol args body src)
   (enter-function name)
   (let* ((symbol name)
	  (O-exp (find-O-expander symbol))
	  (G-exp (find-G-expander symbol))
	  (e (internal-begin-expander e)))
      ;; est-ce qu'on n'est pas en train de redefinir une fonction
      ;; librairie qui, pour etre optimisee, etait aussi une macro ?
      (if (and (expander? O-exp) (not *lib-mode*))
	  (begin
	     (user-warning/location (find-location src)
		"top-level"
		"Disabling optimization for library function"
		name)
	     (unbind-O-expander! symbol)))
      (if (and (expander? G-exp) (not *lib-mode*))
	  (begin
	     (warning/location (find-location src)
		"top-level"
		"Disabling debug information for library function"
		name)
	     (unbind-G-expander! symbol)))
      (let* ((loc (find-location/loc (cddr src) (find-location src)))
	     (ebody (with-lexical
		       (args*->args-list args)
		       '_
		       (find-location src)
		       (lambda () (e (normalize-progn/loc body loc) e)))))
	 (leave-function)
	 `(define ,(cons name (expand-args args e)) ,ebody))))

;*---------------------------------------------------------------------*/
;*    do-external-define-value ...                                     */
;*---------------------------------------------------------------------*/
(define (do-external-define-value e name value src)
   (let* ((symbol name)
	  (O-exp  (find-O-expander symbol))
	  (G-exp  (find-G-expander symbol))
	  (e      (internal-begin-expander e)))
      ;; est-ce qu'on n'est pas en train de redefinir une fonction
      ;; librairie qui, pour etre optimisee, etait aussi une macro ?
      (if (and (expander? O-exp) (not *lib-mode*))
	  (begin
	     (user-warning/location (find-location src)
		"define"
		"Disabling optimization for library function"
		name)
	     (unbind-O-expander! symbol)))
      (if (and (expander? G-exp) (not *lib-mode*))
	  (begin
	     (user-warning/location (find-location src)
		"define"
		"Disabling debug information for library function"
		name)
	     (unbind-G-expander! symbol)))
      (let* ((loc (find-location/loc (cddr src) (find-location src)))
	     (evalue (e (normalize-progn/loc value loc) e)))
	 `(define ,name ,evalue))))

;*---------------------------------------------------------------------*/
;*    do-inline/generic/method ...                                     */
;*---------------------------------------------------------------------*/
(define (do-inline/generic/method define-keyword e id name args body src)
   (set! internal-definition? #t)
   (enter-function name)
   (let* ((O-exp  (find-O-expander name))
	  (G-exp  (find-G-expander name))
	  (e      (internal-begin-expander e))
	  (loc    (find-location/loc (cddr src) (find-location src)))
	  (ebody  (if (pair? body)
		      (with-lexical
		       (args*->args-list args)
		       '_
		       (find-location src)
		       (lambda ()
			  (e (normalize-progn/loc body loc) e)))
		      '())))
      (leave-function)
      ;; est-ce qu'on n'est pas en train de redefinir une fonction
      ;; librairie qui, pour etre optimisee, etait aussi une macro ?
      (if (and (expander? O-exp) (not *lib-mode*))
	  (begin
	     (user-warning/location (find-location src)
		"define"
		"Disabling optimization for library function"
		name)
	     (unbind-O-expander! name)))
      (if (and (expander? G-exp) (not *lib-mode*))
	  (begin
	     (user-warning/location (find-location src)
		"define"
		"Disabling debug information for library function"
		name)
	     (unbind-G-expander! name)))
      (set! internal-definition? #f)
      (if (null? ebody)
	  `(,define-keyword ,(cons id (expand-args args e)))
	  `(,define-keyword ,(cons id (expand-args args e)) ,ebody))))


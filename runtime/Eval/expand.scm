;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/expand.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 09:57:39 1994                          */
;*    Last change :  Fri Sep 26 11:33:27 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    La macro expansion de l'interprete                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expand
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __param
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __evenv
	    __macro)
   
   (extern  (macro $lexical-stack::pair-nil ()
		   "BGL_LEXICAL_STACK")
	    (macro $lexical-stack-set!::void (::pair-nil)
		   "BGL_LEXICAL_STACK_SET"))
	    
   (java    (class foreign
	       (method static $lexical-stack::pair-nil ()
		       "BGL_LEXICAL_STACK")
	       (method static $lexical-stack-set!::void (::pair-nil)
		       "BGL_LEXICAL_STACK_SET")))
   
   (export  (expand <expression>)
	    (expand! <expression>)
	    (expand-once <expression>)
	    (parse-formal-ident <expression>)
	    (%lexical-stack::pair-nil)
	    (%with-lexical ::pair-nil ::obj ::procedure ::obj)
	    (args->list ::obj)
	    (bindings->list ::obj)))

;*---------------------------------------------------------------------*/
;*    expand ...                                                       */
;*---------------------------------------------------------------------*/
(define (expand x)
   (initial-expander x initial-expander))
 
;*---------------------------------------------------------------------*/
;*    expand! ...                                                      */
;*---------------------------------------------------------------------*/
(define (expand! x)
   (initial-expander! x initial-expander!))

;*---------------------------------------------------------------------*/
;*    expand-once ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-once x)
   (initial-expander x (lambda (x e) x)))

;*---------------------------------------------------------------------*/
;*    initial-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (initial-expander x e)
   (cond
      ((symbol? x)
       (identifier-eval-expander x e))
      ((not (pair? x))
       x)
      (else
       (let ((e1 (cond
		    ((not (symbol? (car x)))
		     application-eval-expander)
		    ((pair? (assq (car x) (%lexical-stack)))
		     application-eval-expander)
		    (else
		     (let ((bf (get-eval-expander (car x))))
			(if bf
			    bf
			    (let ((id (car (parse-formal-ident (car x)))))
			       (let ((b (get-eval-expander id)))
				  (if b
				      b
				      application-eval-expander)))))))))
	  (let ((new (e1 x e)))
	     (if (and (pair? new) (not (epair? new)) (epair? x))
		 (econs (car new) (cdr new) (cer x))
		 new))))))

;*---------------------------------------------------------------------*/
;*    initial-expander! ...                                            */
;*---------------------------------------------------------------------*/
(define (initial-expander! x e)
   (cond
      ((symbol? x)
       (identifier-eval-expander x e))
      ((not (pair? x))
       x)
      ((and (symbol? (car x)) (not (pair? (assq (car x) (%lexical-stack)))))
       (let* ((bf (get-eval-expander (car x)))
	      (e1 (or bf
		      (let ((id (id-of-typed-id (car x))))
			 (and id (get-eval-expander id))))))
	  (if (procedure? e1)
	      (e1 x e)
	      (application-eval-expander! x e))))
      (else
       (application-eval-expander! x e))))

;*---------------------------------------------------------------------*/
;*    identifier-eval-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (identifier-eval-expander x e)
   x)

;*---------------------------------------------------------------------*/
;*    application-eval-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (application-eval-expander x e)
   (let loop ((x x))
      (cond
	 ((null? x)
	  '())
	 ((not (pair? x))
	  (error "application" "Illegal form" x))
	 ((epair? x)
	  (econs (e (car x) e) (loop (cdr x)) (cer x)))
	 (else
	  (cons (e (car x) e) (loop (cdr x)))))))

;*---------------------------------------------------------------------*/
;*    application-eval-expander! ...                                   */
;*---------------------------------------------------------------------*/
(define (application-eval-expander! x e)
   (let loop ((y x))
      (cond
	 ((null? y)
	  x)
	 ((not (pair? y))
	  (error "application" "Illegal form" y))
	 (else
	  (set-car! y (e (car y) e))
	  (loop (cdr y))))))

;*---------------------------------------------------------------------*/
;*    parse-formal-ident ...                                           */
;*---------------------------------------------------------------------*/
(define (parse-formal-ident ident)
   (cond
      ((or (eq? ident #!optional)
	   (eq? ident #!rest)
	   (eq? ident #!key))
       (cons (gensym 'dsssl) '()))
      ((and (pair? ident) (symbol? (car ident)))
       (cons ident '()))
      ((not (symbol? ident))
       (error #f "Illegal formal identifier" ident))
      (else
       (let* ((string (symbol->string ident))
	      (len    (string-length string)))
	  (let loop ((walker     0)
		     (id-stop    0)
		     (type-start 0))
	     (cond
		((=fx walker len)
		 (cond
		    ((and (=fx id-stop 0)
			  (>fx type-start 0))
		     ;; this empty name variable can be usefull to declare
		     ;; prototype so it is legal.
		     (cons (string->symbol "")
			   (string->symbol (substring string type-start len))))
		    ((=fx id-stop 0)
		     (cons ident '()))
		    ((=fx type-start len)
		     ;; empty type are erroneous
		     (error #f "Illegal formal identifier" ident))
		    (else
		     (cons (string->symbol
			    (substring string 0 id-stop))
			   (string->symbol
			    (substring string type-start len))))))
		((and (char=? (string-ref string walker) #\:)
		      (<fx walker (-fx len 1))
		      (char=? (string-ref string (+fx walker 1)) #\:))
		 (if (>fx type-start 0)
		     (error #f "Illegal formal identifier" ident)
		     (loop (+fx walker 2)
			   walker
			   (+fx walker 2))))
		(else
		 (loop (+fx walker 1)
		       id-stop
		       type-start))))))))

;*---------------------------------------------------------------------*/
;*    id-of-typed-id ...                                               */
;*---------------------------------------------------------------------*/
(define (id-of-typed-id ident)
   (when (symbol? ident)
      (let* ((string (symbol->string ident))
	     (len (string-length string)))
	 (let loop ((walker 0)
		    (id-stop 0)
		    (type-start 0))
	    (cond
	       ((=fx walker len)
		(cond
		   ((and (=fx id-stop 0) (>fx type-start 0))
		    #f)
		   ((=fx id-stop 0)
		    #f)
		   ((=fx type-start len)
		    ;; empty type are erroneous
		    (error #f "Illegal formal identifier" ident))
		   (else
		    (string->symbol (substring string 0 id-stop)))))
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(if (>fx type-start 0)
		    (error #f "Illegal formal identifier" ident)
		    (loop (+fx walker 2) walker (+fx walker 2))))
	       (else
		(loop (+fx walker 1) id-stop type-start)))))))

;*---------------------------------------------------------------------*/
;*    %lexical-stack ...                                               */
;*---------------------------------------------------------------------*/
(define (%lexical-stack)
   ($lexical-stack))

;*---------------------------------------------------------------------*/
;*    %with-lexical ...                                                */
;*---------------------------------------------------------------------*/
(define (%with-lexical new form e key)
   (let ((old-lexical-stack (%lexical-stack)))
      ($lexical-stack-set! (append (map (lambda (n) (cons n key)) new)
				   old-lexical-stack))
      (unwind-protect
	 (e form e)
	 ($lexical-stack-set! old-lexical-stack))))
    
;*---------------------------------------------------------------------*/
;*    args->list ...                                                   */
;*---------------------------------------------------------------------*/
(define (args->list args)
   (cond
      ((null? args)
       '())
      ((symbol? args)
       (list args))
      ((pair? args)
       (cons (car args) (args->list (cdr args))))
      (else
       (error 'args->list "Illegal args list" args))))

;*---------------------------------------------------------------------*/
;*    bindings->list ...                                               */
;*---------------------------------------------------------------------*/
(define (bindings->list bindings)
   (cond
      ((null? bindings)
       '())
      ((not (pair? bindings))
       (error 'bindings->list "Illegal bindings list" bindings))
      ((symbol? (car bindings))
       (cons bindings (bindings->list (cdr bindings))))
      ((not (pair? (car bindings)))
       (error 'bindings->list "Illegal bindings list" bindings))
      (else
       (cons (car bindings) (bindings->list (cdr bindings))))))


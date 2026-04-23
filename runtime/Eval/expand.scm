;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Eval/expand.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 09:57:39 1994                          */
;*    Last change :  Thu Apr 23 08:40:53 2026 (serrano)                */
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
	    __reader
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __evenv
	    __evutils
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
   
   (export  (expand ::obj)
	    (expand! ::obj)
	    (expand/env ::obj ::obj)
	    (expand/env! ::obj ::obj)
	    (expand-once ::obj)
	    (expand-eval ::obj)
	    (%lexical-stack::pair-nil)
	    (%with-lexical ::pair-nil ::obj ::procedure ::obj)
	    (expand-error proc msg obj)))

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
;*    expand/env ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand/env x env)
   (let ((old *module5-env*))
      (set! *module5-env* env)
      (unwind-protect
	 (expand x)
	 (set! *module5-env* old))))

;*---------------------------------------------------------------------*/
;*    expand/env! ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand/env! x env)
   (let ((env *module5-env*))
      (unwind-protect
	 (expand! x)
	 (set! *module5-env* env))))

;*---------------------------------------------------------------------*/
;*    expand-once ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-once x)
   (initial-expander x (lambda (x e) x)))

;*---------------------------------------------------------------------*/
;*    expand-eval ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-eval x)
   (eval-expander x eval-expander))

;*---------------------------------------------------------------------*/
;*    initial-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (initial-expander x e)
   (initial-expander/application x e application-eval-expander #t))

;*---------------------------------------------------------------------*/
;*    eval-expander ...                                                */
;*---------------------------------------------------------------------*/
(define (eval-expander x e)
   (initial-expander/application x e application-eval-expander #f))

;*---------------------------------------------------------------------*/
;*    initial-expander! ...                                            */
;*---------------------------------------------------------------------*/
(define (initial-expander! x e)
   (initial-expander/application x e application-eval-expander! #t))

;*---------------------------------------------------------------------*/
;*    initial-expander/application ...                                 */
;*---------------------------------------------------------------------*/
(define (initial-expander/application x e ae module5::bool)
   (let ((e1 (cond
		((symbol? x)
		 identifier-eval-expander)
		((null? x)
		 (error "expand" "Illegal form" '()))
		((not (pair? x))
		 (lambda (x e) x))
		((symbol? (car x))
		 (cond
		    ((and module5 (get-module5-expander (car x)))
		     =>
		     (lambda (e1) e1))
		    ((get-eval-expander (car x))
		     =>
		     (lambda (e1) e1))
		    (else
		     (let* ((loc (get-source-location x))
			    (id (car (parse-formal-ident (car x) loc))))
			(cond
			   ((pair? (assq id (%lexical-stack)))
			    ae)
			   ((eq? id (car x))
			    ae)
			   ((and module5 (get-module5-expander id))
			    =>
			    (lambda (e1) e1))
			   ((get-eval-expander id)
			    =>
			    (lambda (e1) e1))
			   (else
			    ae))))))
		(else
		 ae))))
      (let ((new (e1 x e)))
	 (if (and (pair? new) (not (epair? new)) (epair? x))
	     (econs (car new) (cdr new) (cer x))
	     new))))

;*---------------------------------------------------------------------*/
;*    identifier-eval-expander ...                                     */
;*---------------------------------------------------------------------*/
(define (identifier-eval-expander x e)
   x)

;*---------------------------------------------------------------------*/
;*    application-eval-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (application-eval-expander x e)
   (let loop ((y x))
      (cond
	 ((null? y)
	  '())
	 ((not (pair? y))
	  (expand-error "application" "Illegal form" x))
	 ((epair? y)
	  (econs (e (car y) e) (loop (cdr y)) (cer x)))
	 (else
	  (cons (e (car y) e) (loop (cdr y)))))))

;*---------------------------------------------------------------------*/
;*    application-eval-expander! ...                                   */
;*---------------------------------------------------------------------*/
(define (application-eval-expander! x e)
   (let loop ((y x))
      (cond
	 ((null? y)
	  x)
	 ((not (pair? y))
	  (expand-error "application" "Illegal form" x))
	 (else
	  (set-car! y (e (car y) e))
	  (loop (cdr y))))))

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
      ($lexical-stack-set!
	 (append (map (lambda (n)
			 (let ((f (parse-formal-ident n
				     (get-source-location e))))
			    (if (pair? f)
				(cons (car f) key)
				(cons n key))))
		    new)
	    old-lexical-stack))
      (unwind-protect
	 (e form e)
	 ($lexical-stack-set! old-lexical-stack))))
    
;*---------------------------------------------------------------------*/
;*    expand-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-error proc msg obj)
   (if (epair? obj)
       (match-case (cer obj)
	  ((at ?fname ?loc)
	   (error/location proc msg obj fname loc))
	  (else
	   (error proc msg obj)))
       (error proc msg obj)))

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/expand.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 09:57:39 1994                          */
;*    Last change :  Sun Aug 25 09:12:58 2019 (serrano)                */
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
	    (expand-once ::obj)
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
;*    expand-once ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-once x)
   (initial-expander x (lambda (x e) x)))

;*---------------------------------------------------------------------*/
;*    initial-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (initial-expander x e)
   (initial-expander/application x e application-eval-expander))

;*---------------------------------------------------------------------*/
;*    initial-expander! ...                                            */
;*---------------------------------------------------------------------*/
(define (initial-expander! x e)
   (initial-expander/application x e application-eval-expander!))

;*---------------------------------------------------------------------*/
;*    initial-expander/application ...                                 */
;*---------------------------------------------------------------------*/
(define (initial-expander/application x e ae)
   (let ((e1 (cond
		((symbol? x)
		 identifier-eval-expander)
		((null? x)
		 (error "expand" "Illegal form" '()))
		((not (pair? x))
		 (lambda (x e) x))
		((symbol? (car x))
		 (cond
		    ((get-eval-expander (car x))
		     =>
		     (lambda (x) x))
		    (else
		     (let* ((loc (get-source-location x))
			    (id (car (parse-formal-ident (car x) loc))))
			(cond
			   ((pair? (assq id (%lexical-stack)))
			    ae)
			   ((get-eval-expander id)
			    =>
			    (lambda (x) x))
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

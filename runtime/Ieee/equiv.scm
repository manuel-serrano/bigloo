;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/equiv.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 09:57:55 1995                          */
;*    Last change :  Mon Jan 14 14:00:37 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.2. Equivalence predicates (page 13, r4)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_equivalence_6_2
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
            __weakptr
	    __structure
	    __object
	    __ucs2
	    __unicode
	    __custom
	    __foreign
	    __os
	    __date
	    __srfi4
	    __bignum
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_pairs_and_lists_6_3
	    __r4_vectors_6_8
	    __r4_numbers_6_5_flonum
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    
	    __evenv)
   
   (extern  (infix macro c-eq?::bool (::obj ::obj) "=="))
   
   (java    (class foreign
	       (method static c-eq?::bool (::obj ::obj) "EQ")
	       (method static c-boxed-eq?::bool (::obj ::obj) "BOXED_EQ")))
   
   (export  (eqv?::bool ::obj ::obj)
	    (inline eq?::bool  ::obj ::obj)
	    (equal?::bool ::obj ::obj))
   
   (pragma  (eqv? side-effect-free nesting fail-safe)
	    (eq? side-effect-free nesting fail-safe)
	    (c-eq? side-effect-free no-cfa-top nesting args-safe fail-safe)
	    (equal? side-effect-free no-cfa-top nesting fail-safe)))

;*---------------------------------------------------------------------*/
;*    eq? ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (eq? obj1 obj2)
   (c-eq? obj1 obj2))

;*---------------------------------------------------------------------*/
;*    eqv? ...                                                         */
;*---------------------------------------------------------------------*/
(define (eqv? obj1 obj2)
   (cond
      ((eq? obj1 obj2)
       #t)
      ((exact? obj1)
       (and (exact? obj2) (= obj1 obj2)))
      ((inexact? obj1)
       (and (inexact? obj2) (= obj1 obj2)))
      ((symbol? obj1)
       (and (symbol? obj2)
	    (string=? (symbol->string! obj1) (symbol->string! obj2))))
      ((foreign? obj1)
       (and (foreign? obj2) (foreign-eq? obj1 obj2)))
      ((weakptr? obj1)
       (and (weakptr? obj2) (eqv? (weakptr-data obj1) (weakptr-data obj2))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    equal? ...                                                       */
;*---------------------------------------------------------------------*/
(define (equal? obj1 obj2)
   (cond
      ((eq? obj1 obj2)
       #t)
      ((string? obj1)
       (and (string? obj2) (string=? obj1 obj2)))
      ((symbol? obj1)
       #f)
      ((pair? obj1)
       (and (pair? obj2)
	    (equal? (car obj1) (car obj2))
	    (equal? (cdr obj1) (cdr obj2))))
      ((vector? obj1)
       (let ((lobj1 (vector-length obj1)))
	  (and (vector? obj2)
	       (=fx (vector-length obj2) lobj1)
	       (=fx (vector-tag obj1) (vector-tag obj2))
	       (let test ((i 0))
		  (or (=fx i lobj1)
		      (and (equal? (vector-ref-ur obj1 i)
				   (vector-ref-ur obj2 i))
			   (test (+fx i 1))))))))
      ((eqv? obj1 obj2)
       #t)
      ((fixnum? obj1)
       #f)
      ((homogeneous-vector? obj1)
       (let ((lobj1 ($hvector-length obj1)))
	  (and ($hvector? obj2)
	       (=fx ($hvector-length obj2) lobj1)
	       (multiple-value-bind (tag1 _ get _ cmp)
		  (homogeneous-vector-info obj1)
		  (multiple-value-bind (tag2 _ _ _ _)
		     (homogeneous-vector-info obj2)
		     (and (eq? tag1 tag2)
			  (let test ((i 0))
			     (or (=fx i lobj1)
				 (and (cmp (get obj1 i) (get obj2 i))
				      (test (+fx i 1)))))))))))
      ((c-flonum? obj1)
       #f)
      (($struct? obj1)
       (let ((lobj1 (struct-length obj1)))
	  (and (struct? obj2)
	       (=fx (struct-length obj2) lobj1)
	       (let test ((i 0))
		  (or (=fx i lobj1)
		      (and (equal? (struct-ref obj1 i) (struct-ref obj2 i))
			   (test (+fx i 1))))))))
      ((cell? obj1)
       (and (cell? obj2) (equal? (cell-ref obj1) (cell-ref obj2))))
      ((object? obj1)
       (and (object? obj2) (object-equal? obj1 obj2)))
      ((ucs2-string? obj1)
       (and (ucs2-string? obj2) (ucs2-string=? obj1 obj2)))
      ((custom? obj1)
       (and (custom? obj2) (custom-equal? obj1 obj2)))
      ((ucs2? obj1)
       (and (ucs2? obj2) (ucs2=? obj1 obj2)))
      ((number? obj1)
       #f)
      ((date? obj1)
       (and (date? obj2) (=elong (date->seconds obj1) (date->seconds obj2))))
      ((foreign? obj1)
       (and (foreign? obj2) (foreign-eq? obj1 obj2)))
      ((weakptr? obj1)
       (and (weakptr? obj2) (equal? (weakptr-data obj1) (weakptr-data obj2))))
      (else
       #f)))


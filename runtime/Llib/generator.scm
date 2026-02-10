;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/generator.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Mon Feb  9 16:19:48 2026                          */
;*    Last change :  Mon Feb  9 17:55:26 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Generators (aka lambda*)                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __generator
   
   (import  __error
	    __object)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __structure
	    __date
	    __os
	    __bit
	    __thread

	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __foreign
	    __evenv)

   (include "Llib/cps.sch")

   (export  (class generator
	       (kont::procedure (default list))
	       (done::bool (default #f)))
	    (inline generator-yield ::generator ::obj ::procedure)
	    (inline next ::generator ::obj)
	    (inline throw ::generator ::obj)
	    (expand-lambda* x e)))

;*---------------------------------------------------------------------*/
;*    generator-yield ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (generator-yield g::generator value::obj kont::procedure)
   (set! (-> g kont) kont)
   value)

;*---------------------------------------------------------------------*/
;*    next ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (next g::generator value::obj)
   ((-> g kont) value #f))
   
;*---------------------------------------------------------------------*/
;*    throw ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (throw g::generator value::obj)
   ((-> g kont) value #t))
   
;*---------------------------------------------------------------------*/
;*    expand-lambda* ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-lambda* x e)
   (match-case x
      ((lambda* ?args . ?body)
       (let ((v (gensym 'e))
	     (t (gensym 't))
	     (g (gensym 'g))
	     (r (gensym 'r)))
	  (e `(lambda ,args
		 (let ((,g (instantiate::generator)))
		    (with-access::generator ,g (kont)
		       (set! kont
			  (lambda (,v ,t)
			     ,(cps g '()
				 `(let ((,r (begin ,@body)))
				     (with-access::generator ,g (done)
					(set! done #t)
					,r))
				 (lambda (x) x)))))
		    ,g))
	     e)))
      (else
       (error "lambda*" "wrong syntax" x))))
      

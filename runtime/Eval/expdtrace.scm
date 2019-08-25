;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expdtrace.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel SERRANO                                    */
;*    Creation    :  Tue Sep  1 16:21:59 1992                          */
;*    Last change :  Sun Aug 25 09:15:55 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Trace forms expansion                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_trace
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    
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
	    
	    __progn
	    __param
	    __expand)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (make-expand-when-trace::procedure ::symbol)
	    (make-expand-with-trace::procedure ::symbol)
	    (make-expand-trace-item::procedure ::symbol)))

;*---------------------------------------------------------------------*/
;*    make-expand-when-trace ...                                       */
;*---------------------------------------------------------------------*/
(define (make-expand-when-trace mode)
   (lambda (x e)
      (match-case x
	 ((?- ?level . ?exp)
	  (if (if (=fx (bigloo-profile) 0)
		  (if (eq? mode 'compiler)
		      (>fx (bigloo-compiler-debug) 0)
		      (>fx (bigloo-debug) 0))
		  #f)
	      (e `(if (trace-active? ,level)
		      (begin ,@exp)
		      #unspecified)
		 e)
	      #unspecified))
	 (else
	  (expand-error "when-trace" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    make-with-trace ...                                              */
;*---------------------------------------------------------------------*/
(define (make-expand-with-trace mode)
   (lambda (x e)
      (match-case x
	 ((?- ?level ?lbl . ?arg*)
	  (if (if (=fx (bigloo-profile) 0)
		  (if (eq? mode 'compiler)
		      (>fx (bigloo-compiler-debug) 0)
		      (>fx (bigloo-debug) 0))
		  #f)
	      (let* ((f (gensym 'f))
		     (nx `(let ((,f (lambda () (begin ,@arg*))))
			     (if (>fx (bigloo-debug) 0)
				 (%with-trace ,level ,lbl ,f)
				 (,f)))))
		 (e nx e))
	      (e `(begin ,@arg*) e)))
	 (else
	  (expand-error "with-trace" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    make-expand-trace-item ...                                       */
;*---------------------------------------------------------------------*/
(define (make-expand-trace-item mode)
   (lambda (x e)
      (if (if (=fx (bigloo-profile) 0)
	      (if (eq? mode 'compiler)
		  (>fx (bigloo-compiler-debug) 0)
		  (>fx (bigloo-debug) 0))
	      #f)
	  `(if (>fx (bigloo-debug) 0)
	       (trace-item ,@(map (lambda (x) (e x e)) (cdr x)))
	       #unspecified)
	  #unspecified)))

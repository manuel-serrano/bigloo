;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/expdtrace.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel SERRANO                                    */
;*    Creation    :  Tue Sep  1 16:21:59 1992                          */
;*    Last change :  Thu Apr 30 14:33:00 2009 (serrano)                */
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
	    
	    __progn
	    __param)
   
   (use     __type
	    __evenv)
   
   (export  (expand-when-trace ::obj ::procedure)
	    (expand-with-trace ::obj ::procedure)
	    (expand-trace-item ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-when-trace ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-when-trace x e)
   (match-case x
      ((?- ?level . ?exp)
       (if (>fx (bigloo-compiler-debug) 0)
	   (e `(if (>=fx (bigloo-debug) ,level)
		   (begin ,@exp)
		   #unspecified)
	      e)
	   #unspecified))
      (else
       (error 'when-trace "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    with-trace ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-with-trace x e)
   (match-case x
      ((?- ?level ?lbl . ?arg*)
       (if (>fx (bigloo-compiler-debug) 0)
	   (e `(if (>fx (bigloo-debug) 0)
		   (%with-trace ,level ,lbl (lambda () (begin ,@arg*)))
		   (begin ,@arg*)) e)
	   (e `(begin ,@arg*) e)))
      (else
       (error 'with-trace "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-trace-item ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-trace-item x e)
   (if (>fx (bigloo-compiler-debug) 0)
       `(if (>fx (bigloo-debug) 0)
	    (trace-item ,@(map (lambda (x) (e x e)) (cdr x))))
       #unspecified))

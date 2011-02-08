;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_fsize.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:42:27 2011                          */
;*    Last change :                                                    */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Compute the size of stack needed for an abstraction              */
;*    of the space for free variables is not included.                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_fsize

   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __dsssl
	    __bit
	    __param
	    __bexit
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
	    __r5_control_features_6_4
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __pp
	    __reader
	    __progn
	    __expand
	    __evenv
	    __evcompile
	    __everror
	    __evmodule
	    
	    __evaluate_types)

   (export  (frame-size::int ::ev_expr)))

(define (frame-size::int e::ev_expr);
   (fsize e 0) )

(define-generic (fsize::int e::ev_expr n::int);
   (error "eval" "internal error: not defined for" e) )

(define-method (fsize::int e::ev_var n::int);
   n )

(define-method (fsize::int var::ev_global n::int);
   n )

(define-method (fsize::int e::ev_litt n::int);
   n )

(define-method (fsize::int e::ev_if n::int);
   (with-access::ev_if e (p t e)
      (max (fsize p n) (fsize t n) (fsize e n)) ))

(define-method (fsize::int e::ev_list n::int);
   (let rec ( (l (ev_list-args e)) (r n) )
      (if (null? l)
	  r
	  (rec (cdr l) (max r (fsize (car l) n))) )))

(define-method (fsize::int e::ev_prog2 n::int);
   (with-access::ev_prog2 e (e1 e2)
      (max (fsize e1 n) (fsize e2 n)) ))

(define-method (fsize::int e::ev_hook n::int);
   (with-access::ev_hook e (e)
      (fsize e n) ))

(define-method (fsize::int e::ev_bind-exit n::int);
   (with-access::ev_bind-exit e (body)
      (fsize body (+fx n 1)) ))

(define-method (fsize::int e::ev_unwind-protect n::int);
   (with-access::ev_unwind-protect e (e body)
      (max (fsize e n) (fsize body n)) ))

(define-method (fsize::int e::ev_with-handler n::int);
   (with-access::ev_with-handler e (handler body)
      (max (fsize handler n) (fsize body n)) ))

(define-method (fsize::int e::ev_let n::int);
   (with-access::ev_let e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_let* n::int);
   (with-access::ev_let* e (vals body)
      (let rec ( (l vals) (n n) (r n) )
	 (if (null? l)
	     (max (fsize body n) r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_letrec n::int);
   (with-access::ev_letrec e (vals body)
      (let ( (n (+fx n (length vals))) )
	 (let rec ( (l vals) (r n) )
	    (if (null? l)
		(max (fsize body n) r)
		(rec (cdr l) (max (fsize (car l) n) r)) )))))

(define-method (fsize::int e::ev_app n::int);
   (with-access::ev_app e (fun args)
      (let rec ( (l args) (n n) (r (fsize fun n)) )
	 (if (null? l)
	     (max n r)
	     (rec (cdr l) (+fx n 1) (max (fsize (car l) n) r)) ))))

(define-method (fsize::int e::ev_abs n::int);
   (with-access::ev_abs e (arity vars body)
      (let ( (nn (fsize body (length vars))) )
	 (ev_abs-size-set! e nn)
	 n )))




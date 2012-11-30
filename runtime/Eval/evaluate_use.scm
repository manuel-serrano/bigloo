;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_use.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:47:50 2011                          */
;*    Last change :  Fri Nov 30 09:28:40 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute the vars used by an expression                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_use

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
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
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

   (export (generic use ::ev_expr ::obj)))

(define-generic (use e::ev_expr done))

(define (use* l done)
   (if (null? l)
       done
       (use* (cdr l) (use (car l) done)) ))

(define-method (use var::ev_var done);
   (unless (memq var done) (set! done (cons var done)))
   done )

(define-method (use var::ev_global done);
   done )

(define-method (use e::ev_litt done);
   done )

(define-method (use e::ev_if done);
   (with-access::ev_if e (p t e)
      (use p (use t (use e done))) ))

(define-method (use e::ev_list done);
   (with-access::ev_list e (args)
      (use* args done) ))

(define-method (use e::ev_prog2 done);
   (with-access::ev_prog2 e (e1 e2)
      (use e1 (use e2 done)) ))

(define-method (use e::ev_hook done);
   (with-access::ev_hook e (e)
      (use e done) ))

(define-method (use e::ev_setlocal done);
   (with-access::ev_setlocal e (v e)
      (unless (memq v done) (set! done (cons v done)))
      (use e done) ))
      
(define-method (use e::ev_bind-exit done);
   (with-access::ev_bind-exit e (body)
      (use body done) ))
      
(define-method (use e::ev_unwind-protect done);
   (with-access::ev_unwind-protect e (e body)
      (use e (use body done)) ))
      
(define-method (use e::ev_with-handler done);
   (with-access::ev_with-handler e (handler body)
      (use handler (use body done)) ))

(define-method (use e::ev_synchronize done);
   (with-access::ev_synchronize e (mutex prelock body)
      (use mutex (use prelock (use body done))) ))

(define-method (use e::ev_binder done);
   (with-access::ev_binder e (vals body)
      (use body (use* vals done)) ))

(define-method (use e::ev_binder done);
   (with-access::ev_binder e (vals body)
      (use body (use* vals done)) ))

(define-method (use e::ev_labels done);
   (with-access::ev_labels e (vals body)
      (let rec ( (l vals) (done done) )
	 (if (null? l)
	     (use body done)
	     (rec (cdr l) (use (cdar l) done)) ))))

(define-method (use e::ev_goto done);
   (with-access::ev_goto e (args)
      (use* args done) ))

(define-method (use e::ev_app done);
   (with-access::ev_app e (fun args)
      (use fun (use* args done)) ))

(define-method (use e::ev_abs done);
   (with-access::ev_abs e (body)
      (use body done) ))




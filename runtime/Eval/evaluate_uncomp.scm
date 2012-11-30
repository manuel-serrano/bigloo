;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_uncomp.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:46:26 2011                          */
;*    Last change :  Fri Nov 30 09:28:19 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    AST back to list                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_uncomp

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

   (export (uncompile ::ev_expr)))

(define (uncompile e::ev_expr);
   (uncomp e) )

(define-generic (uncomp e::ev_expr))

(define-method (uncomp var::ev_var);
   (with-access::ev_var var (name)
      name ))

(define-method (uncomp var::ev_global);
   (with-access::ev_global var (name)
      name ))

(define-method (uncomp e::ev_litt);
   (with-access::ev_litt e (value)
      `',value ))

(define-method (uncomp e::ev_if);
   (with-access::ev_if e (p t e)
      `(if ,(uncomp p) ,(uncomp t) ,(uncomp e)) ))

(define-method (uncomp e::ev_or);
   (with-access::ev_or e (args)
      `(or ,@(map uncomp args)) ))

(define-method (uncomp e::ev_and);
   (with-access::ev_and e (args)
      `(and ,@(map uncomp args)) ))

(define-method (uncomp e::ev_prog2);
   (with-access::ev_prog2 e (e1 e2)
      (let ( (e1 (uncomp e1)) (e2 (uncomp e2)) )
	 (if (and (pair? e2) (eq? (car e2) 'begin))
	     `(begin ,e1 ,@(cdr e2))
	     `(begin ,e1 ,e2) ))))

(define-method (uncomp e::ev_trap);
   (with-access::ev_trap e (e)
      `(trap ,(uncomp e)) ))

(define-method (uncomp e::ev_setglobal);
   (with-access::ev_setglobal e (name e)
      `(set! ,name ,(uncomp e)) ))

(define-method (uncomp e::ev_defglobal);
   (with-access::ev_defglobal e (name e)
      `(define ,name ,(uncomp e)) ))

(define-method (uncomp e::ev_setlocal);
   (with-access::ev_setlocal e (v e)
      `(set! ,(uncomp v) ,(uncomp e)) ))
      
(define-method (uncomp e::ev_bind-exit);
   (with-access::ev_bind-exit e (var body)
      `(bind-exit (,(uncomp var)) ,(uncomp body)) ))
      
(define-method (uncomp e::ev_unwind-protect);
   (with-access::ev_unwind-protect e (e body)
      `(unwind-protect ,(uncomp e) ,(uncomp body)) ))
      
(define-method (uncomp e::ev_with-handler);
   (with-access::ev_with-handler e (handler body)
      `(with-handler ,(uncomp handler) ,(uncomp body)) ))

(define-method (uncomp e::ev_synchronize);
   (with-access::ev_synchronize e (mutex prelock body)
      `(synchronize ,(uncomp mutex) :prelock ,(uncomp prelock) ,(uncomp body)) ))

(define-method (uncomp e::ev_let);
   (with-access::ev_let e (vars vals body)
      `(let ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_let*);
   (with-access::ev_let* e (vars vals body)
      `(let* ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_letrec);
   (with-access::ev_letrec e (vars vals body)
      `(letrec ,(map (lambda (v a) `(,(uncomp v) ,(uncomp a))) vars vals) ,(uncomp body)) ))

(define-method (uncomp e::ev_labels);
   (with-access::ev_labels e (vars vals body)
      `(letrec ,(map (lambda (v a) `(,(uncomp v) (lambda ,(map uncomp (car a))
						    ,(uncomp (cdr a)) )))
		     vars vals )
	  ,(uncomp body)) ))

(define-method (uncomp e::ev_goto);
   (with-access::ev_goto e (label args)
      `(,(uncomp label) ,@(map uncomp args)) ))

(define-method (uncomp e::ev_app);
   (with-access::ev_app e (fun args)
      `(,(uncomp fun) ,@(map uncomp args)) ))

(define-method (uncomp e::ev_abs);
   (with-access::ev_abs e (arity vars body)
      (define (redovars n l)
	 (if (>=fx n 0)
	     l
	     (let rec ( (l l) )
		(if (null? (cdr l))
		    (car l)
		    (cons (car l) (rec (cdr l))) ))))
      `(lambda ,(redovars arity (map (lambda (v) (with-access::ev_var v (name) name)) vars)) ,(uncomp body)) ))



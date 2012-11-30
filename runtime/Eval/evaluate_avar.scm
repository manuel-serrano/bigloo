;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate_avar.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb  8 16:40:08 2011                          */
;*    Last change :  Fri Nov 30 09:25:02 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute free/closed variable for the lambda-based evaluator      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate_avar

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

   (export  (analyse-vars ::ev_expr)
	    (check-var ::ev_var ::obj ::ev_abs)))

;;
;; Lib
;;
(define (index x l)
   (define (walk l n)
      (cond ((null? l) #f)
	    ((eq? x (car l)) n)
	    (else (walk (cdr l) (+fx n 1))) ))
   (walk l 0) )

(define (_index x l)
   (or (index x l) (error "eval" "internal error: not found" (list x " in " (map lname l)))) )

(define (union l1 l2)
   (if (null? l1)
       l2
       (let ( (x (car l1)) )
	  (union (cdr l1) (if (memq x l2) l2 (cons x l2))) )))

(define (inter l1 l2)
   (if (null? l1)
       '()
       (let ( (x (car l1)) )
	  (if (memq x l2)
	      (cons x (inter (cdr l1) l2))
	      (inter (cdr l1) l2) ))))

(define (diff l1 l2)
   (if (null? l1)
       '()
       (let ( (x (car l1)) )
	  (if (memq x l2)
	      (diff (cdr l1) l2)
	      (cons x (diff (cdr l1) l2)) ))))

(define (lname v)
   (if (isa? v ev_var)
       (with-access::ev_var v (name) name)
       v ))

(define **call/cc-compliant** #f)
;(define (call/cc proc)
;   (call-cc (lambda (cont)
;	       (let ( (evc (get-evaluation-contexte)) )
;		  (let ( (cont (lambda (x)
;				  (set-evaluation-contexte! evc)
;				  (cont x) )) )
; ...

(define (analyse-vars e::ev_expr);
   (let ( (fake (instantiate::ev_abs (loc 'never) (where 'fake) (arity 0) (vars '()) (body e))) )
      ;; We can check something in fake.
      (avar e '() fake) ))

(define (check-var var::ev_var local abs::ev_abs);
   (unless (memq var local)
      (with-access::ev_abs abs (free)
	 (unless (memq var free) (set! free (cons var free))) )))

(define-generic (avar e::ev_expr local abs))

(define-method (avar var::ev_var local abs);
   (check-var var local abs) )

(define-method (avar var::ev_global local abs);
   #unspecified )

(define-method (avar e::ev_litt local abs);
   #unspecified )

(define-method (avar e::ev_if local abs);
   (with-access::ev_if e (p t e)
      (avar p local abs)
      (avar t local abs)
      (avar e local abs) ))

(define-method (avar e::ev_list local abs);
   (with-access::ev_list e (args)
      (for-each (lambda (e) (avar e local abs)) args) ))

(define-method (avar e::ev_prog2 local abs);
   (with-access::ev_prog2 e (e1 e2)
      (avar e1 local abs)
      (avar e2 local abs) ))

(define-method (avar e::ev_hook local abs);
   (with-access::ev_hook e (e)
      (avar e local abs) ))

(define-method (avar e::ev_setlocal local abs);
   (with-access::ev_setlocal e (v e)
      (check-var v local abs)
      (with-access::ev_var v (eff)
	 (set! eff #t))
      (avar e local abs) ))

(define-method (avar e::ev_bind-exit local abs);
   (with-access::ev_bind-exit e (var body)
      (avar body (cons var local) abs)
      (bind-and-reset-effect abs (cons var '())) ))

(define-method (avar e::ev_unwind-protect local abs);
   (with-access::ev_unwind-protect e (e body)
      (avar e local abs)
      (avar body local abs) ))

(define-method (avar e::ev_with-handler local abs);
   (with-access::ev_with-handler e (handler body)
      (avar handler local abs)
      (avar body local abs) ))

(define-method (avar e::ev_synchronize local abs);
   (with-access::ev_synchronize e (mutex body prelock)
      (avar mutex local abs)
      (avar prelock local abs)
      (avar body local abs) ))

(define-method (avar e::ev_let local abs);
   (with-access::ev_let e (vars vals body boxes)
      (for-each (lambda (e) (avar e local abs)) vals)
      (avar body (append vars local) abs)
      (bind-and-reset-effect abs vars)
      (set! boxes (filter (lambda (v) (with-access::ev_var v (eff) eff)) vars)) ))

(define-method (avar e::ev_let* local abs);
   (with-access::ev_let* e (vars vals body boxes)
      (let ( (local (append vars local)) )
	 (for-each (lambda (e) (avar e local abs)) vals)
	 (avar body local abs)
	 (bind-and-reset-effect abs vars)
	 (set! boxes (map (lambda (v) (with-access::ev_var v (eff) eff)) vars)) )))
	 
(define-method (avar e::ev_letrec local abs);
   (with-access::ev_letrec e (vars vals body)
      (let ( (local (append vars local)) )
	 (for-each (lambda (e) (avar e local abs)) vals)
	 (avar body local abs)
	 (with-access::ev_abs abs (bind)
	    (set! bind (append vars bind)) )
	 (for-each (lambda (v) (with-access::ev_var v (eff) (set! eff #t))) vars) )))
	 
(define-method (avar e::ev_labels local abs);
   (with-access::ev_labels e (vars vals body boxes)
      (let ( (local (append vars local)) )
	 (for-each
	  (lambda (s)
	     (avar (cdr s) (append (car s) local) abs)
	     (bind-and-reset-effect abs (car s)) )
	  vals )
	 (set! boxes
	    (map (lambda (var s) (cons var (map (lambda (v) (with-access::ev_var v (eff) eff)) (car s)))) vars vals) )
	 (avar body local abs) )))
	 
(define-method (avar e::ev_goto local abs);
   (with-access::ev_goto e (args)
      (for-each (lambda (e) (avar e local abs)) args) ))

(define-method (avar e::ev_app local abs);
   (with-access::ev_app e (fun args)
      (avar fun local abs)
      (for-each (lambda (e) (avar e local abs)) args) ))


(define (bind-and-reset-effect abs::ev_abs vars);
   ;; clean the effect flag for non-closed variables
   (define (funion l)
      (if (null? l)
	  '()
	  (union (with-access::ev_abs (car l) (free) free) (funion (cdr l))) ))
   (let ( (ifree (funion (with-access::ev_abs abs (inner) inner))) )
      (with-access::ev_abs abs (bind)
	 (set! bind (append vars bind)))
      (unless **call/cc-compliant**
	 (for-each (lambda (v)
		      (with-access::ev_var v (eff)
			 (set! eff #f) ))
	    (diff vars ifree) ))
      ifree ))

(define-method (avar e::ev_abs local abs);
   (with-access::ev_abs e (arity vars body)
      (with-access::ev_abs abs (inner)
	 (set! inner (cons e inner)))
      (avar body vars e)
      (let ( (ifree (bind-and-reset-effect e vars)) )
	 (with-access::ev_abs e (free boxes bind)
	    (set! free (diff (union ifree free) bind))
	    (set! boxes (filter (lambda (v) (with-access::ev_var v (eff) eff))
			   vars) )))))

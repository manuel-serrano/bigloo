;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evmeaning.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug  4 10:48:41 1993                          */
;*    Last change :  Sat Jul 20 10:44:40 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bigloo's interpreter.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __evmeaning
   
   (include "Eval/byte-code.sch")
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __bit
	    __param
	    __bexit
	    __object
	    __thread
	    __rgc
	    
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
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __progn
	    __evenv
	    __evcompile
	    __everror
	    __evmodule)
   
   (export  (evmeaning ::obj ::pair-nil ::dynamic-env))
   
   (extern  (%funcall-0::obj (::procedure)
			     "eval_funcall_0")
	    (%funcall-1::obj (::procedure ::obj)
			     "eval_funcall_1")
	    (%funcall-2::obj (::procedure ::obj ::obj)
			     "eval_funcall_2")
	    (%funcall-3::obj (::procedure ::obj ::obj ::obj)
			     "eval_funcall_3")
	    (%funcall-4::obj (::procedure ::obj ::obj ::obj ::obj)
			     "eval_funcall_4")
	    (%eval-apply::obj (::procedure ::obj)
			      "eval_apply"))
   
   (java    (class foreign
	       (method static %funcall-0::obj (::procedure)
		       "eval_funcall_0")
	       (method static %funcall-1::obj (::procedure ::obj)
		       "eval_funcall_1")
	       (method static %funcall-2::obj (::procedure ::obj ::obj)
		       "eval_funcall_2")
	       (method static %funcall-3::obj (::procedure ::obj ::obj ::obj)
		       "eval_funcall_3")
	       (method static %funcall-4::obj (::procedure ::obj ::obj ::obj ::obj)
		       "eval_funcall_4")
	       (method static %eval-apply::obj (::procedure ::obj)
		       "eval_apply")))

   ;; Disabling user-inlining is strictly required in order
   ;; to avoid stack blow up in the evmeaning function.
   ;; Enabing JVM inlining is required for the same reason.
   (option (set! *user-inlining?* #f)
	   ;;(set! *inlining-kfactor* (lambda (x) 1))
	   (set! *saw-register-allocation?* #t)
	   (set! *saw-register-allocation-max-size* 8000)))

;*---------------------------------------------------------------------*/
;*    case-bounce ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (case-bounce test . clauses)
   (let* ((bounce '())
	  (r `(case ,test
		 ,@(map (lambda (c)
			   (match-case c
			      (((bounce ?vars ((and (? integer?) ?n))) . ?body)
			       (let ((id (string->symbol
					  (string-append
					   "evmeaning-bounce-"
					   (integer->string n)))))
				  (set! bounce
					(cons `(define (,id ,@vars) ,@body)
					      bounce))
				  `((,n) (,id ,@vars))))
			      (((bounce . ?-) . ?-)
			       (error 'case-bounce "Illegal clause" c))
			      (else
			       c)))
			clauses))))
      (putprop! 'case-bounce 'bouncing bounce)
      r))

;*---------------------------------------------------------------------*/
;*    emit-bounced! ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (emit-bounced!)
   `(begin ,@(getprop 'case-bounce 'bouncing)))

;*---------------------------------------------------------------------*/
;*    %inline-let ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (%with-inline body)
   `(let ((type-check #t)
	  (a0 #unspecified))
       ,body))
	  
;*---------------------------------------------------------------------*/
;*    %inline1 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (%inline1 fun code stack denv . type)
   (if (null? type)
       `(let ((a0 (evmeaning (evcode-ref ,code 2) ,stack ,denv)))
	   `(,fun a0))
       (let ((pred (symbol-append (car type) '?)))
	  `(let ((a0 (evmeaning (evcode-ref ,code 2) ,stack ,denv)))
	      (if (,pred a0)
		  (,fun a0)
		  (evtype-error (evcode-loc code) "eval" ',(car type) a0))))))

;*---------------------------------------------------------------------*/
;*    %inline2 ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (%inline2 fun code stack denv . type)
   (if (null? type)
       `(let ((a0 (evmeaning (evcode-ref ,code 2) ,stack ,denv))
	      (a1 (evmeaning (evcode-ref ,code 3) ,stack ,denv)))
	   (,fun a0 a1))
       (let ((pred (symbol-append (car type) '?)))
	  `(let ((a0 (evmeaning (evcode-ref ,code 2) ,stack ,denv))
		 (a1 (evmeaning (evcode-ref ,code 3) ,stack ,denv)))
	      (if (,pred a0)
		  (if (,pred a1)
		      (,fun a0 a1)
		      (evtype-error (evcode-loc code) "eval" ',(car type) a1))
		  (evtype-error (evcode-loc code) "eval" ',(car type) a0))))))

;*---------------------------------------------------------------------*/
;*    evmeaning-unbound ...                                            */
;*---------------------------------------------------------------------*/
(define (evmeaning-unbound loc name mod)
   (everror loc
	    "eval"
	    (if (evmodule? mod)
		(format "Unbound variable (from module `~a')"
			(evmodule-name mod))
		"Unbound variable (from top-level)")
	    name))

;*---------------------------------------------------------------------*/
;*    evmeaning-uninitialized ...                                      */
;*---------------------------------------------------------------------*/
(define (evmeaning-uninitialized loc name)
   (everror loc "eval" "Uninitialized variable" name))

;*---------------------------------------------------------------------*/
;*    evmeaning ...                                                    */
;*---------------------------------------------------------------------*/
(define (evmeaning code stack denv)
   (if (evcode? code)
       (%with-inline
	(case-bounce (evcode-op code)
	   ((-2)
	    ;; errors
	    (apply everror (evcode-loc code) (evcode-ref code 0)))
	   ((-1)
	    ;; La seule constante qui nessecite un codage: les `vecteurs'
	    (evcode-ref code 0))
	   ((0)
	    ;; first local variable
	    (car stack))
	   ((1)   
	    ;; second local variable
	    (cadr stack))
	   ((2)   
	    ;; third local variable
	    (caddr stack))
	   ((3)   
	    ;; fourth local variable
	    (cadddr stack))
	   ((4)   
	    ;; deep local variables
	    (let ((offset (evcode-ref code 0)))
	       (do ((i 4 (+fx i 1))
		    (env (cddddr stack) (cdr env)))
		   ((=fx i offset) (car env)))))
	   ((5)
	    ;; mutable global variable
	    (__evmeaning_address-ref
	     (eval-global-value (evcode-ref code 0))))
	   ((6)
	    ;; global variable
	    (let* ((g (evcode-ref code 0))
		   (val (eval-global-value g)))
	       (if (eq? val #unspecified)
		   (let ((tag (eval-global-tag g)))
		      (if (or (=fx tag 3) (=fx tag 4))
			  (evmeaning-uninitialized
			   (evcode-loc code) (eval-global-name g))
			  val))
		   val)))
	   ((bounce (code stack denv) (7))
	    ;; dynamic global variable
	    (let* ((name (evcode-ref code 0))
		   (mod (evcode-ref code 1))
		   (global (evmodule-find-global mod name)))
	       (if (eval-global? global)
		   (begin
		      ;; we change the value of the byte-code
		      ;; because, now, the variable is bound
		      (evcode-op-set! code 6)
		      (evcode-set! code 0 global)
		      (eval-global-value global))
		   (evmeaning-unbound (evcode-loc code) name mod))))
	   ((8)
	    ;; (set! <global> <value>)
	    (let ((var (evcode-ref code 0))
		  (val (evmeaning (evcode-ref code 1) stack denv)))
	       (update-eval-global! code var val)
	       (unspecified)))
	   ((bounce (code stack denv) (9))
	    ;; (set! <dynamic-global> <value>)
	    (let* ((name (evcode-ref code 0))
		   (value (evcode-ref code 1))
		   (mod (evcode-ref code 2))
		   (global (evmodule-find-global mod name)))
	       (if (eval-global? global)
		   (begin
		      (evcode-op-set! code 8)
		      (evcode-set! code 0 global)
		      (evcode-set! code 1 value)
		      (evmeaning code stack denv))
		   (evmeaning-unbound (evcode-loc code) name mod))))
	   ((10)
	    ;; la mutation de la premiere variable locale
	    (set-car! stack (evmeaning (evcode-ref code 0) stack denv))
	    (unspecified))
	   ((11)
	    ;; la mutation de la deuxieme variable locale
	    (set-car! (cdr stack) (evmeaning (evcode-ref code 0) stack denv))
	    (unspecified))
	   ((12)
	    ;; la mutation de la troisieme variable locale
	    (set-car! (cddr stack) (evmeaning (evcode-ref code 0) stack denv))
	    (unspecified))
	   ((13)
	    ;; la mutation de la quatrieme variable locale
	    (set-car! (cdddr stack) (evmeaning (evcode-ref code 0) stack denv))
	    (unspecified))
	   ((bounce (code stack denv) (14))
	    ;; la mutation des variables locales profondes
	    (let ((offset (evcode-ref code 0))
		  (value  (evmeaning (evcode-ref code 1) stack denv)))
	       (do ((i 4 (+fx i 1))
		    (env (cddddr stack) (cdr env)))
		   ((=fx i offset) (set-car! env value)))
	       (unspecified)))
	   ((15) 
	    ;; conditional
	    (if (evmeaning (evcode-ref code 0) stack denv)
		(evmeaning (evcode-ref code 1) stack denv)
		(evmeaning (evcode-ref code 2) stack denv)))
	   ((16)
	    ;; sequence (by construction, the length is at least 1)
	    (let ((len (-fx (evcode-length code) 1)))
	       (let loop ((i 0))
		  (if (=fx i len)
		      (evmeaning (evcode-ref code i) stack denv)
		      (begin
			 (evmeaning (evcode-ref code i) stack denv)
			 (loop (+fx i 1)))))))
	   ((bounce (code stack denv) (17))
	    ;; (define <global> <var>)
	    (let ((var (evcode-ref code 0))
		  (val (evcode-ref code 1))
		  (mod (evcode-ref code 2)))
	       (let ((v (evmodule-find-global mod var)))
		  (if (eval-global? v)
		      (case (eval-global-tag v)
			 ((0)
			  (everror (evcode-loc code) "eval"
				   "Compiled read-only variable cannot be redefined"
				   var))
			 ((1)
			  (evwarning (evcode-loc code) "eval"
				     "\nRedefinition of compiled variable -- "
				     var)
			  (update-eval-global! code v (evmeaning val '() denv)))
			 ((2)
			  (update-eval-global! code v (evmeaning val '() denv)))
			 ((3)
			  (update-eval-global! code v (evmeaning val '() denv))
			  (eval-global-tag-set! v 2))
			 ((4)
			  (update-eval-global! code v (evmeaning val '() denv))
			  (eval-global-tag-set! v 5))
			 (else
			  (everror (evcode-loc code) "eval"
				   "Read-only variable cannot be redefined"
				   var)))
		      (let* ((loc (evcode-loc code))
			     (g (make-eval-global var (eval-module) loc)))
			 ;; first we bind the variable
			 (evmodule-bind-global! mod var g loc)
			 ;; second we evaluate it's body
			 (let ((value (evmeaning val '() denv)))
			    (set-eval-global-value! g value))))
		  var)))
	   ((bounce (code stack denv) (18))
	    ;; bind-exit
	    (bind-exit (__dummy__)
	       ((evmeaning (evcode-ref code 0) stack denv) __dummy__)))
	   ((bounce (code) (25))
	    ;; l'appel de fonction de compilee anonyme d'arite 0
	    ((evcode-ref code 0)))
	   ((bounce (code stack denv) (26))
	    ;; l'appel de fonction de compilee anonyme d'arite 1
	    (let* ((fun (evcode-ref code 0))
		   (a0 (evmeaning (evcode-ref code 1) stack denv)))
	       (fun a0)))
	   ((bounce (code stack denv) (27))
	    ;; l'appel de fonction de compilee anonyme d'arite 2
	    (let* ((fun (evcode-ref code 0))
		   (a0 (evmeaning (evcode-ref code 1) stack denv))
		   (a1 (evmeaning (evcode-ref code 2) stack denv)))
	       (fun a0 a1)))
	   ((bounce (code stack denv) (28))
	    ;; l'appel de fonction de compilee anonyme d'arite 3
	    (let* ((fun (evcode-ref code 0))
		   (a0 (evmeaning (evcode-ref code 1) stack denv))
		   (a1 (evmeaning (evcode-ref code 2) stack denv))
		   (a2 (evmeaning (evcode-ref code 3) stack denv)))
	       (fun a0 a1 a2)))
	   ((bounce (code stack denv) (29))
	    ;; l'appel de fonction de compilee anonyme d'arite 4
	    (let* ((fun (evcode-ref code 0))
		   (a0 (evmeaning (evcode-ref code 1) stack denv))
		   (a1 (evmeaning (evcode-ref code 2) stack denv))
		   (a2 (evmeaning (evcode-ref code 3) stack denv))
		   (a3 (evmeaning (evcode-ref code 4) stack denv)))
	       (fun a0 a1 a2 a3)))
	   ((bounce (code stack denv) (30))
	    ;; l'appel de fonction de compilee anonyme d'arite plus que 4
	    (let ((eargs (map (lambda (x)
				 (evmeaning x stack denv))
			      (evcode-ref code 1))))
	       (apply (evcode-ref code 0) eargs)))
	   ((31)
	    ;; funcall 0
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (evmeaning-funcall-0 code stack denv fun)))
	   ((32)
	    ;; funcall 1
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (evmeaning-funcall-1 code stack denv fun)))
	   ((33)
	    ;; funcall 2
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (evmeaning-funcall-2 code stack denv fun)))
	   ((34)
	    ;; funcall 3
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (evmeaning-funcall-3 code stack denv fun)))
	   ((35)
	    ;; funcall 4
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (evmeaning-funcall-4 code stack denv fun)))
	   ((bounce (code stack denv) (36))
	    ;; funcall >4
	    (let* ((name (evcode-ref code 0))
		   (fun (evmeaning (evcode-ref code 1) stack denv)))
	       (let loop ((args (evcode-ref code 2))
			  (new '())
			  (len 0))
		  (if (null? args)
		      (begin
			 ($env-set-trace-location denv (evcode-loc code))
			 (eval-apply code name fun len (reverse! new)))
		      (loop (cdr args)
			    (cons (evmeaning (car args) stack denv) new)
			    (+fx 1 len))))))
	   ((bounce (code stack denv) (37))
	    ;; This code is very sensitive to the compiler order. Because
	    ;; of tail recursion, it is extremely important that the capture
	    ;; variable are order 0 for the body (the variable !b), and 1
	    ;; for the stack (the variable !s). To enforce this order, we
	    ;; explicitly use a cascade of "if"s
	    ;; procedure arity 0, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda ()
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning !b !s !denv)))
			    ($env-pop-trace !denv)
			    res))))
		0
		body
		stack)))
	   
	   ((bounce (code stack denv) (42))
	    ;; ...untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda ()
		   (let ((!b body))
		      (evmeaning !b stack denv)))
		0
		body
		stack)))
	   ((bounce (code stack denv) (38))
	    ;; procedure arity 1, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning !b (cons x !s) !denv)))
			    ($env-pop-trace !denv)
			    res))))
		1
		body
		stack)))
	   ((bounce (code stack denv) (43))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x)
		   (let ((!b body))
		      (evmeaning !b (cons x stack) denv)))
		1
		body
		stack)))
	   ((bounce (code stack denv) (39))
	    ;; procedure arity 2, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x y)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((r (evmeaning !b (cons x (cons y !s)) !denv)))
			    ($env-pop-trace !denv)
			    r))))
		2
		body
		stack)))
	   ((bounce (code stack denv) (44))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x y)
		   (let ((!b body))
		      (evmeaning !b (cons x (cons y stack)) denv)))
		2
		body
		stack)))
	   ((bounce (code stack denv) (40))
	    ;; procedure arity 3, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x y z)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning !b
					       (cons x
						     (cons y
							   (cons z !s)))
					       !denv)))
			    ($env-pop-trace !denv)
			    res))))
		3
		body
		stack)))
	   ((bounce (code stack denv) (45))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x y z)
		   (let ((!b body))
		      (evmeaning !b (cons x (cons y (cons z stack))) denv)))
		3
		body
		stack)))
	   ((bounce (code stack denv) (41))
	    ;; procedure arity 4, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x y z t)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)	
			 (let ((res (evmeaning
				     !b
				     (cons x
					   (cons y
						 (cons z
						       (cons t !s))))
				     !denv)))
			    ($env-pop-trace !denv)
			    res))))
		4
		body
		stack)))
	   ((bounce (code stack denv) (46))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x y z t)
		   (let ((!b body))
		      (evmeaning !b
				 (cons x (cons y (cons z (cons t stack))))
				 denv)))
		4
		body
		stack)))
	   ((bounce (code stack denv) (47))
	    ;; procedure arity -1, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda x
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning !b (cons x !s) !denv)))
			    ($env-pop-trace !denv)
			    res))))
		-1
		body
		stack)))
	   ((bounce (code stack denv) (51))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda x
		   (let ((!b body))
		      (evmeaning !b (cons x stack) denv)))
		-1
		body
		stack)))	   
	   ((bounce (code stack denv) (48))
	    ;; procedure arity -2, traced
	    (let ((body  (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x . y)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((r (evmeaning !b (cons x (cons y !s)) !denv)))
			    ($env-pop-trace !denv)
			    r))))
		-2
		body
		stack)))
	   ((bounce (code stack denv) (52))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x . y)
		   (let ((!b body))
		      (evmeaning !b (cons x (cons y stack)) denv)))
		-2
		body
		stack)))
	   ((bounce (code stack denv) (49))
	    ;; procedure arity -3, traced
	    (let ((body (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x y . z)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning !b
					       (cons x
						     (cons y
							   (cons z !s)))
					       !denv)))
			    ($env-pop-trace !denv)
			    res))))
		-3
		body
		stack)))
	   ((bounce (code stack denv) (53))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x y . z)
		   (let ((!b body))
		      (evmeaning !b
				 (cons x (cons y (cons z stack)))
				 denv)))
		-3
		body
		stack)))	   
	   ((bounce (code stack denv) (50))
	    ;; procedure arity -4, traced
	    (let ((body  (evcode-ref code 0))
		  (where (evcode-ref code 1))
		  (loc (evcode-loc code)))
	       (evmeaning-procedure!
		(lambda (x y z . t)
		   (let ((!b body)
			 (!s stack)
			 (!denv::dynamic-env (current-dynamic-env)))
		      (let ()
			 ($env-push-trace !denv where loc)
			 (let ((res (evmeaning
				     !b
				     (cons x
					   (cons y
						 (cons z
						       (cons t
							     !s))))
				     !denv)))
			    ($env-pop-trace !denv)
			    res))))
		-4
		body
		stack)))
	   ((bounce (code stack denv) (54))
	    ;; untraced
	    (let ((body (evcode-ref code 0)))
	       (evmeaning-procedure!
		(lambda (x y z . t)
		   (let ((!b body))
		      (evmeaning
		       !b
		       (cons x (cons y (cons z (cons t stack))))
		       denv)))
		-4
		body
		stack)))
	   ((55)
	    ;; procedure arity > 4 arguments, traced
	    (evmeaning-make-traced-4procedure code stack denv))
	   ;; untraced
	   ((56)
	    (evmeaning-make-4procedure code stack denv))
	   ((bounce (code stack denv) (64))
	    ;; unwind-protect
	    (let ((body (evcode-ref code 0))
		  (protect (evcode-ref code 1)))
	       (unwind-protect (evmeaning body stack denv)
			       (evmeaning protect stack denv))))
	   ((65)
	    ;; let (bindings are stored reversed!)
	    (let loop ((vals (evcode-ref code 1))
		       (env stack))
	       (if (null? vals)
		   (evmeaning (evcode-ref code 0) env denv)
		   (let ((v (evmeaning (car vals) stack denv)))
		      (loop (cdr vals) (cons v env))))))
	   ((66)
	    ;; let*
	    (let loop ((vals (evcode-ref code 1))
		       (stack stack))
	       (if (null? vals)
		   (evmeaning (evcode-ref code 0) stack denv)
		   (loop (cdr vals)
			 (cons (evmeaning (car vals) stack denv) stack)))))
	   ((bounce (code stack denv) (67))
	    ;; or
	    (let ((len (evcode-length code)))
	       (let loop ((i 0))
		  (if (<fx i len)
		      (or (evmeaning (evcode-ref code i) stack denv)
			  (loop (+fx i 1)))
		      #f))))
	   ((bounce (code stack denv) (68))
	    ;; and
	    (let ((len (evcode-length code)))
	       (let loop ((i 0)
			  (l #t))
		  (if (<fx i len)
		      (let ((l (evmeaning (evcode-ref code i) stack denv)))
			 (and l (loop (+fx i 1) l)))
		      l))))
	   ((bounce (code stack denv) (70))
	    ;; letrec
	    (let* ((vals (evcode-ref code 1))
		   (stack2 (append (make-list (length vals)) stack)))
	       (let loop ((vals vals)
			  (stack3 stack2))
		  (if (null? vals)
		      (evmeaning (evcode-ref code 0) stack2 denv)
		      (begin
			 (set-car! stack3 (evmeaning (car vals) stack2 denv))
			 (loop (cdr vals) (cdr stack3)))))))
	   ((bounce (code stack denv) (71))
	    ;; with-handler
	    (let* ((handler (evcode-ref code 0))
		   (body (evcode-ref code 1))
		   (ehandler (evmeaning handler stack denv))
		   (loc (evcode-loc code)))
	       (cond
		  ((not (procedure? ehandler))
		   (evtype-error loc "eval" "procedure" ehandler))
		  ((correct-arity? ehandler 1)
		   (with-handler ehandler (evmeaning body stack denv)))
		  (else
		   (evarity-error loc "with-handler" 1 ($procedure-arity ehandler))))))
	   ((bounce (code stack denv) (175))
	    ;; synchronize
	    (let* ((mutex (evcode-ref code 0))
		   (body (evcode-ref code 1))
		   (m (evmeaning mutex stack denv)))
	       (if (mutex? m)
		   (synchronize m
		      (evmeaning body stack denv))
		   (evtype-error (evcode-loc code) "synchronize" "mutex" m))))
	   ((bounce (code stack denv) (176))
	    ;; synchronize/prelock
	    (let* ((mutex (evcode-ref code 0))
		   (prelock (evcode-ref code 1))
		   (body (evcode-ref code 2))
		   (m (evmeaning mutex stack denv)))
	       (if (mutex? m)
		   (synchronize m
		      :prelock (evmeaning prelock stack denv)
		      (evmeaning body stack denv))
		   (evtype-error (evcode-loc code) "synchronize" "mutex" m))))
	   ((131)
	    ;; tailcall 0
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (if (evmeaning-procedure? fun)
		   (evmeaning (evmeaning-procedure-bcode fun)
			      (evmeaning-tailcall-0-stack code stack denv fun)
			      denv)
		   (evmeaning-funcall-0 code stack denv fun))))
	   ((132)
	    ;; tailcall 1
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (if (evmeaning-procedure? fun)
		   (evmeaning (evmeaning-procedure-bcode fun)
			      (evmeaning-tailcall-1-stack code stack denv fun)
			      denv)
		   (evmeaning-funcall-1 code stack denv fun))))
	   ((133)
	    ;; tailcall 2
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (if (evmeaning-procedure? fun)
		   (evmeaning (evmeaning-procedure-bcode fun)
			      (evmeaning-tailcall-2-stack code stack denv fun)
			      denv)
		   (evmeaning-funcall-2 code stack denv fun))))
	   ((134)
	    ;; tailcall 3
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (if (evmeaning-procedure? fun)
		   (evmeaning (evmeaning-procedure-bcode fun)
			      (evmeaning-tailcall-3-stack code stack denv fun)
			      denv)
		   (evmeaning-funcall-3 code stack denv fun))))
	   ((135)
	    ;; tailcall 4
	    (let ((fun (evmeaning (evcode-ref code 1) stack denv)))
	       (if (evmeaning-procedure? fun)
		   (evmeaning (evmeaning-procedure-bcode fun)
			      (evmeaning-tailcall-4-stack code stack denv fun)
			      denv)
		   (evmeaning-funcall-4 code stack denv fun))))
	   ((136)
	    ;; tailcall >4
	    (let* ((name (evcode-ref code 0))
		   (fun (evmeaning (evcode-ref code 1) stack denv))
		   (loc (evcode-loc code)))
	       (let loop ((args (evcode-ref code 2))
			  (new '())
			  (len 0))
		  (if (null? args)
		      (if (evmeaning-procedure? fun)
			  (let* ((fmls (evmeaning-procedure-args fun))
				 (stack (evmeaning-procedure-stack fun))
				 (wen (reverse! new))
				 (e2 (if (>=fx fmls 0)
					 (evmeaning-push-fxargs name
								loc
								wen
								fmls
								stack)
					 (evmeaning-push-vaargs name
								loc
								wen
								fmls
								stack))))
			     (evmeaning (evmeaning-procedure-bcode fun)
					e2
					denv))
			  (eval-apply code name fun len (reverse! new)))
		      (loop (cdr args)
			    (cons (evmeaning (car args) stack denv) new)
			    (+fx 1 len))))))
	   ((145 146)
	    ;; unlinked global variable
	    (evcode-op-set! code (-fx (evcode-op code) 140))
	    (evcode-set! code 0 (evmodule-find-global
				 (evcode-ref code 1)
				 (evcode-ref code 0)))
	    (evmeaning code stack denv))
	   ;; inline code-op	     
	   ((147)
	    (%inline2 + code stack denv number))
	   ((148)
	    (%inline2 - code stack denv number))
	   ((149)
	    (%inline2 * code stack denv number))
	   ((150)
	    (%inline2 / code stack denv number))
	   ((151)
	    (%inline2 < code stack denv number))
	   ((152)
	    (%inline2 > code stack denv number))
	   ((153)
	    (%inline2 <= code stack denv number))
	   ((154)
	    (%inline2 >= code stack denv number))
	   ((155)
	    (%inline2 = code stack denv number))
	   ((166)
	    (%inline2 +fx code stack denv fixnum))
	   ((167)
	    (%inline2 -fx code stack denv fixnum))
	   ((168)
	    (%inline2 *fx code stack denv fixnum))
	   ((169)
	    (%inline2 /fx code stack denv fixnum))
	   ((170)
	    (%inline2 <fx code stack denv fixnum))
	   ((171)
	    (%inline2 >fx code stack denv fixnum))
	   ((172)
	    (%inline2 <=fx code stack denv fixnum))
	   ((173)
	    (%inline2 >=fx code stack denv fixnum))
	   ((174)
	    (%inline2 =fx code stack denv fixnum))
	   ((156)
	    (%inline2 eq? code stack denv))
	   ((157)
	    (%inline2 cons code stack denv))
	   ((158)
	    (%inline1 car code stack denv pair))
	   ((159)
	    (%inline1 cdr code stack denv pair))
	   ((160)
	    (let ((a0 (evmeaning (evcode-ref code 2) stack denv)))
	       (cond
		  ((and (pair? a0) (pair? (cdr a0)))
		   (cadr a0))
		  ((pair? a0)
		   (evtype-error (evcode-loc code)
				 (evcode-ref code 0)
				 "pair" (cdr a0)))
		  (else
		   (evtype-error (evcode-loc code)
				 (evcode-ref code 0)
				 "pair" a0)))))
	   (else
	    ;; unknown byte code
	    (everror (evcode-loc code) "eval" "unknown byte-code" code))))
       code))

(emit-bounced!)

;*---------------------------------------------------------------------*/
;*    evprocedure ...                                                  */
;*---------------------------------------------------------------------*/
(define-struct evprocedure args bcode stack)

;*---------------------------------------------------------------------*/
;*    evmeaning-procedure? ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (evmeaning-procedure? proc)
   (and (procedure? proc) (evprocedure? (procedure-attr proc))))

;*---------------------------------------------------------------------*/
;*    evmeaning-procedure! ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (evmeaning-procedure! proc args bcode stack)
   (procedure-attr-set! proc (evprocedure args bcode stack))
   proc)

;*---------------------------------------------------------------------*/
;*    evmeaning-procedure-bcode ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (evmeaning-procedure-bcode proc)
   (evprocedure-bcode (procedure-attr proc)))

;*---------------------------------------------------------------------*/
;*    evmeaning-procedure-stack ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (evmeaning-procedure-stack proc)
   (evprocedure-stack (procedure-attr proc)))

;*---------------------------------------------------------------------*/
;*    evmeaning-procedure-args ...                                     */
;*---------------------------------------------------------------------*/
(define-inline (evmeaning-procedure-args proc)
   (evprocedure-args (procedure-attr proc)))

;*---------------------------------------------------------------------*/
;*      update-eval-global! ...                                        */
;*---------------------------------------------------------------------*/
(define (update-eval-global! code var val)
   (case (eval-global-tag var)
      ((0)
       (everror (evcode-loc code) "set!" "read-only variable" (eval-global-name var)))
      ((1)
       (__evmeaning_address-set! (eval-global-value var) val))
      ((2)
       (set-eval-global-value! var val))
      ((3)
       (set-eval-global-value! var val))
      ((4)
       (set-eval-global-value! var val))
      ((5)
       (everror (evcode-loc code) "set!" "read-only variable" (eval-global-name var)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-funcall-0 ...                                          */
;*---------------------------------------------------------------------*/
(define (evmeaning-funcall-0 code stack denv fun)
   (let ((name (evcode-ref code 0))
	 (loc (evcode-loc code)))
      ($env-set-trace-location denv loc)
      (cond
	 ((not (procedure? fun))
	  (everror loc "eval" name "Not a procedure"))
	 ((not (correct-arity? fun 0))
	  (evarity-error loc name 0 ($procedure-arity fun)))
	 (else
	  (%funcall-0 fun)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-funcall-1 ...                                          */
;*---------------------------------------------------------------------*/
(define (evmeaning-funcall-1 code stack denv fun)
   (let* ((name (evcode-ref code 0))
	  (loc (evcode-loc code))
	  (a0 (evmeaning (evcode-ref code 2) stack denv)))
      ($env-set-trace-location denv loc)
      (cond
	 ((not (procedure? fun))
	  (everror loc "eval" "Not a procedure" name))
	 ((not (correct-arity? fun 1))
	  (evarity-error loc name 1 ($procedure-arity fun)))
	 (else
	  (%funcall-1 fun a0)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-funcall-2 ...                                          */
;*---------------------------------------------------------------------*/
(define (evmeaning-funcall-2 code stack denv fun)
   (let* ((name (evcode-ref code 0))
	  (loc (evcode-loc code))
	  (a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv)))
      ($env-set-trace-location denv loc)
      (cond
	 ((not (procedure? fun))
	  (everror loc "eval" "Not a procedure" name))
	 ((not (correct-arity? fun 2))
	  (evarity-error loc name 2 ($procedure-arity fun)))
	 (else
	  (%funcall-2 fun a0 a1)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-funcall-3 ...                                          */
;*---------------------------------------------------------------------*/
(define (evmeaning-funcall-3 code stack denv fun)
   (let* ((name (evcode-ref code 0))
	  (loc (evcode-loc code))
	  (a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv))
	  (a2 (evmeaning (evcode-ref code 4) stack denv)))
      ($env-set-trace-location denv loc)
      (cond
	 ((not (procedure? fun))
	  (everror loc "eval" "Not a procedure" name))
	 ((not (correct-arity? fun 3))
	  (evarity-error loc name 3 ($procedure-arity fun)))
	 (else
	  (%funcall-3 fun a0 a1 a2)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-funcall-4 ...                                          */
;*---------------------------------------------------------------------*/
(define (evmeaning-funcall-4 code stack denv fun)
   (let* ((name (evcode-ref code 0))
	  (loc (evcode-loc code))
	  (a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv))
	  (a2 (evmeaning (evcode-ref code 4) stack denv))
	  (a3 (evmeaning (evcode-ref code 5) stack denv)))
      ($env-set-trace-location denv loc)
      (cond
	 ((not (procedure? fun))
	  (everror loc "eval" "Not a procedure" name))
	 ((not (correct-arity? fun 4))
	  (evarity-error loc name 4 ($procedure-arity fun)))
	 (else
	  (%funcall-4 fun a0 a1 a2 a3)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-tailcall-0-stack ...                                   */
;*---------------------------------------------------------------------*/
(define (evmeaning-tailcall-0-stack code stack denv fun)
   (let* ((envd (evmeaning-procedure-stack fun))
	  (arity (evmeaning-procedure-args fun))
	  (loc (evcode-loc code)))
      (when (symbol? (evcode-ref code 0))
	 ($env-set-trace-name denv (evcode-ref code 0))
	 ($env-set-trace-location denv loc))
      (case arity
	 ((0)
	  envd)
	 ((-1)
	  (cons '() envd))
	 (else
	  (evarity-error loc (evcode-ref code 0) 0 arity)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-tailcall-1-stack ...                                   */
;*---------------------------------------------------------------------*/
(define (evmeaning-tailcall-1-stack code stack denv fun)
   (let ((a0 (evmeaning (evcode-ref code 2) stack denv)))
      (let* ((envd (evmeaning-procedure-stack fun))
	     (arity (evmeaning-procedure-args fun))
	     (loc (evcode-loc code)))
	 (when (symbol? (evcode-ref code 0))
	    ($env-set-trace-name denv (evcode-ref code 0))
	    ($env-set-trace-location denv loc))
	 (case arity
	    ((1)
	     (cons a0 envd))
	    ((-1)
	     (cons (list a0) envd))
	    ((-2)
	     (cons a0 (cons '() envd)))
	    (else
	     (evarity-error loc (evcode-ref code 0) 1 arity))))))

;*---------------------------------------------------------------------*/
;*    evmeaning-tailcall-2-stack ...                                   */
;*---------------------------------------------------------------------*/
(define (evmeaning-tailcall-2-stack code stack denv fun)
   (let* ((a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv)))
      (let* ((envd (evmeaning-procedure-stack fun))
	     (arity (evmeaning-procedure-args fun))
	     (loc (evcode-loc code)))
	 (when (symbol? (evcode-ref code 0))
	    ($env-set-trace-name denv (evcode-ref code 0))
	    ($env-set-trace-location denv loc))
	 (case arity
	    ((2)
	     (cons a0 (cons a1 envd)))
	    ((-1)
	     (cons (list a0 a1) envd))
	    ((-2)
	     (cons a0 (cons (list a1) envd)))
	    ((-3)
	     (cons a0 (cons a1 (cons '() envd))))
	    (else
	     (evarity-error loc (evcode-ref code 0) 2 arity))))))

;*---------------------------------------------------------------------*/
;*    evmeaning-tailcall-3-stack ...                                   */
;*---------------------------------------------------------------------*/
(define (evmeaning-tailcall-3-stack code stack denv fun)
   (let* ((a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv))
	  (a2 (evmeaning (evcode-ref code 4) stack denv)))
      (let* ((envd (evmeaning-procedure-stack fun))
	     (arity (evmeaning-procedure-args fun))
	     (loc (evcode-loc code)))
	 (when (symbol? (evcode-ref code 0))
	    ($env-set-trace-name denv (evcode-ref code 0))
	    ($env-set-trace-location denv loc))
	 (case arity
	    ((3)
	     (cons a0 (cons a1 (cons a2 envd))))
	    ((-1)
	     (cons (list a0 a1 a2) envd))
	    ((-2)
	     (cons a0 (cons (list a1 a2) envd)))
	    ((-3)
	     (cons a0 (cons a1 (cons (list a2) envd))))
	    ((-4)
	     (cons a0 (cons a1 (cons a2 (cons '() envd)))))
	    (else
	     (evarity-error loc (evcode-ref code 0) 3 arity))))))

;*---------------------------------------------------------------------*/
;*    evmeaning-tailcall-4-stack ...                                   */
;*---------------------------------------------------------------------*/
(define (evmeaning-tailcall-4-stack code stack denv fun)
   (let* ((a0 (evmeaning (evcode-ref code 2) stack denv))
	  (a1 (evmeaning (evcode-ref code 3) stack denv))
	  (a2 (evmeaning (evcode-ref code 4) stack denv))
	  (a3 (evmeaning (evcode-ref code 5) stack denv)))
      (let* ((envd (evmeaning-procedure-stack fun))
	     (arity (evmeaning-procedure-args fun))
	     (loc (evcode-loc code)))
	 (when (symbol? (evcode-ref code 0))
	    ($env-set-trace-name denv (evcode-ref code 0))
	    ($env-set-trace-location denv loc))
	 (case arity
	    ((4)
	     (cons a0 (cons a1 (cons a2 (cons a3 envd)))))
	    ((-1)
	     (cons (list a0 a1 a2 a3) envd))
	    ((-2)
	     (cons a0 (cons (list a1 a2 a3) envd)))
	    ((-3)
	     (cons a0 (cons a1 (cons (list a2 a3) envd))))
	    ((-4)
	     (cons a0 (cons a1 (cons a2 (cons (list a3) envd)))))
	    ((-5)
	     (cons a0 (cons a1 (cons a2 (cons a3 (cons '() envd))))))
	    (else
	     (evarity-error loc (evcode-ref code 0) 4 arity))))))

;*---------------------------------------------------------------------*/
;*    eval-apply ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (eval-apply code name fun len args)
   (cond
      ((not (procedure? fun))
       (everror (evcode-loc code) "apply" "Not a procedure" name))
      ((not (correct-arity? fun len))
       (evarity-error (evcode-loc code) name len ($procedure-arity fun)))
      (else
       (%eval-apply fun args))))

;*---------------------------------------------------------------------*/
;*    evmeaning-make-traced-4procedure ...                             */
;*---------------------------------------------------------------------*/
(define (evmeaning-make-traced-4procedure code stack denv)
   (let ((body (evcode-ref code 0))
	 (where (evcode-ref code 1))
	 (formals (evcode-ref code 2))
	 (loc (evcode-loc code)))
      (if (list? formals)
	  (let ((lf (length formals)))
	     (evmeaning-procedure!
	      (lambda x
		 (let ((!b body)
		       (!s stack)
		       (!denv::dynamic-env (current-dynamic-env)))
		    (let ()
		       ($env-push-trace !denv where loc)
		       (let ((e2 (evmeaning-push-fxargs where code x lf !s)))
			  (let ((res (evmeaning !b e2 !denv)))
			     ($env-pop-trace !denv)
			     res)))))
	      lf
	      body
	      stack))
	  (let ((lf (let loop ((formals formals)
			       (num -1))
		       (if (pair? formals)
			   (loop (cdr formals) (-fx num 1))
			   num))))
	     (evmeaning-procedure!
	      (lambda x
		 (let ((!b body)
		       (!s stack)
		       (!denv::dynamic-env (current-dynamic-env)))
		    (let ()
		       ($env-push-trace !denv where loc)
		       (let ((e2 (evmeaning-push-vaargs where code x lf !s)))
			  (let ((res (evmeaning !b e2 !denv)))
			     ($env-pop-trace !denv)
			     res)))))
	      lf
	      body
	      stack)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-make-4procedure ...                                    */
;*---------------------------------------------------------------------*/
(define (evmeaning-make-4procedure code stack denv)
   (let ((body (evcode-ref code 0))
	 (formals (evcode-ref code 1)))
      (if (list? formals)
	  (let ((lf (length formals)))
	     (evmeaning-procedure!
	      (lambda x
		 (let ((!b body)
		       (!s stack))
		    (let ((e2 (evmeaning-push-fxargs x code x lf !s)))
		       (evmeaning !b e2 denv))))
	      lf
	      body
	      stack))
	  (let ((lf (let loop ((formals formals)
			       (num -1))
		       (if (pair? formals)
			   (loop (cdr formals) (-fx num 1))
			   num))))
	     (evmeaning-procedure!
	      (lambda x
		 (let ((!b body)
		       (!s stack))
		    (let ((e2 (evmeaning-push-vaargs x code x lf !s)))
		       (evmeaning !b e2 denv))))
	      lf
	      body
	      stack)))))

;*---------------------------------------------------------------------*/
;*    evmeaning-push-fxargs ...                                        */
;*---------------------------------------------------------------------*/
(define (evmeaning-push-fxargs name loc actuals num stack)
   (let _loop_ ((a actuals)
		(n num))
      (cond
	 ((=fx n 0)
	  (if (not (null? a))
	      (evarity-error loc name (length actuals) num)
	      stack))
	 ((null? a)
	  (evarity-error loc name (length actuals) num))
	 (else
	  (cons (car a) (_loop_ (cdr a) (-fx n 1)))))))

;*---------------------------------------------------------------------*/
;*    evmeaning-push-vaargs ...                                        */
;*---------------------------------------------------------------------*/
(define (evmeaning-push-vaargs name loc actuals num stack)
   (let _loop_ ((a actuals)
		(n num))
      (cond
	 ((=fx n -1)
	  (cons a stack))
	 ((null? a)
	  (evarity-error loc name (length actuals) num))
	 (else
	  (cons (car a) (_loop_ (cdr a) (+fx n 1)))))))
   
;*---------------------------------------------------------------------*/
;*    Les environments ...                                             */
;*---------------------------------------------------------------------*/
(init-the-global-environment!)

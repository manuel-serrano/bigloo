(module saw_jvm_funcall
   (import type_type ast_var ast_node ast_env
	   type_env
	   object_class
	   object_slots
	   backend_backend
	   backend_bvm
	   backend_jvm_class
	   backend_lib
	   backend_cplib
	   saw_procedures
	   saw_jvm_out)
   (export (module-funcall/apply me::jvm)
	   (wide-class indexed::global index::int) ))
   

(define (key-opt? v)
   (let ( (v (global-entry v)) )
      (let ( (val  (global-value v)) )
	 (when (sfun? val)
	    (let ( (clo (sfun-the-closure-global val)) )
	       (let ( (o (global-optional? clo)) (k (global-key? clo)) )
		      (or o k) ))))))
;;
;; Overload funcall<i> and apply methods
;;
(define (module-funcall/apply me)
   (let ( (p (reverse! (get-procedures (jvm-functions me)))) )
      ;; CARE sort p in order to optimize switches
      (let ( (n '0) )
	 (for-each (lambda (var)
		      (widen!::indexed var (index n))
		      (set! n (+fx n 1)) )
		   p ))
      (if (not (null? p))
	  (begin (funcalli me 0 p)
		 (funcalli me 1 p)
		 (funcalli me 2 p)
		 (funcalli me 3 p)
		 (funcalli me 4 p)
		 (compile-apply me p) ))))

(define (exchange l i j)
   (let ( (li (list-tail l i)) (lj (list-tail l j)) )
      (let ( (o (car li)) )
	 (set-car! li (car lj))
	 (set-car! lj o) )))

;;
;; The "funcall"s method
;;
(define (funcalli me i procs)
   (define (needed? p)
      (let ( (arity (global-arity p)) )
	 (if arity
	     (or (and (>=fx arity 0)
		      (if (key-opt? p)
			  (<= arity i)
			  (=fx arity i) ))
		 (and (<fx arity 0) (>= arity (- -1 i))) )
	     (<= i 1) )))
   (define (name n) (if (=fx n 0) '() (cons (gensym) (name (-fx n 1)))))
   (let ( (need (map needed? procs)) )
      (if (not (every not need))
	  (let* ( (fname (string-append "funcall" (integer->string i)))
		  (p (cons 'this (name i))) )
	     (open-lib-method me (string->symbol fname))
	     (declare-locals me p '())
	     (for-each (lambda (v) (code! me `(aload ,v))) p)
	     (code! me '(aload this))
	     (code! me '(getfield procindex))
	     (compile-funi me i need procs fname)
	     (close-method me) ))))

(define (compile-funi me i need procs fname)
   (define (L n) (string->symbol (string-append "L" (integer->string n))))
   (define (get-labs i ns ps)
      (if (null? ns)
	  '()
	  (cons (if (car ns) (L i) 'err)
		(get-labs (+fx i 1) (cdr ns) (cdr ps)) )))
   (let ( (labs (get-labs 0 need procs)) )
      (code! me `(tableswitch err 0 ,@labs))
      (label me 'err)
      (code! me `(invokespecial ,(string->symbol (string-append "p" fname))))
      (code! me '(areturn))
      (for-each (lambda (n? lab p) (if n? (compile-for-funcalli me i lab p)))
		need
		labs
		procs )))


(define (compile-for-funcalli me i lab p)
   (let ( (arity (global-arity p)) )
      (define (make-vect i)
	 (string->symbol (string-append "make_vector" (integer->string i))) )
      (define (make_cons n)
	 (if (= n 0)
	     (call-global me (global-entry p))
	     (begin (code! me '(invokestatic cons))
		    (make_cons (-fx n 1)) )))
      (label me lab)
      (cond
	 ((eq? arity #f)
	  (if (=fx i 0)
	      (begin (code! me '(pop))
		     (code! me `(getstatic ,(declare-global me p))) )
	      (code! me `(putstatic ,(declare-global me p))) ))
	 ((>=fx arity 0)
 	  (if (key-opt? p)
	      (let ( (v (global-entry p)) )
		 (code! me `(invokestatic ,(make-vect i)))
		 (call-global me v) )
	      (call-global me (global-entry p)) ))
	 (else
	  (code! me '(getstatic *nil*))
	  (make_cons (+ i 1 arity)) ))
      (code! me '(areturn)) ))

;;
;; The apply method
;;
(define (compile-apply me procs)
   (let ( (need (map global-arity procs)) )
      (unless (every not need)
	 (open-lib-method me 'apply)
	 (declare-locals me '(this l) '())
	 (code! me '(aload this))
	 (code! me '(aload this))
	 (code! me '(getfield procindex))
	 (compile-dispatch me need procs)
	 (close-method me) )))

(define (compile-dispatch me need procs)
   (define (L n) (string->symbol (string-append "L" (integer->string n))))
   (define (get-labs i ns ps)
      (if (null? ns)
	  '()
	  (cons (if (car ns) (L i) 'err)
		(get-labs (+fx i 1) (cdr ns) (cdr ps)) )))
   (let ( (labs (get-labs 0 need procs)) )
      (code! me `(tableswitch err 0 ,@labs))
      (label me 'err)
      (code! me '(aload l))
      (code! me '(invokespecial papply))
      (code! me '(areturn))
      (for-each (lambda (n? lab p) (if n? (compile-for-apply me lab p)))
		need
		labs
		procs )))

(define (compile-for-apply me lab p)
   (define (push-cars me n fixedarity?)
      (cond
	 ((=fx n 0)
	  (if (not fixedarity?)
	      (code! me '(aload l)) ))
	 ((=fx n 1)
	  (code! me '(aload l))
	  (code! me '(checkcast pair))
	  (code! me '(getfield car))
	  (unless fixedarity?
	     (code! me '(aload l))
	     (code! me '(checkcast pair))
	     (code! me '(getfield cdr)) ))
	 (else
	  (code! me '(aload l))
	  (code! me '(checkcast pair))
	  (code! me '(dup))
	  (code! me '(getfield cdr))
	  (code! me '(astore l))
	  (code! me '(getfield car))
	  (push-cars me (- n 1) fixedarity?) )))
   (let ( (arity (global-arity p)) )
      (label me lab)
      (if (>= arity 0)
	  (if (key-opt? p)
	      (begin (code! me '(aload l))
		     (code! me '(invokestatic list_to_vector)) )
	      (push-cars me arity #t) )
	  (push-cars me (- -1 arity) #f) )
      (call-global me (global-entry p))
      (code! me '(areturn)) ))

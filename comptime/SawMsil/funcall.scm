(module msil_funcall
   (import engine_param
	   type_type ast_var ast_node ast_env
	   type_env
	   object_class
	   object_slots
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   backend_lib
	   backend_cplib
	   saw_procedures
	   msil_out)
   (export (module-funcall/apply me::dotnet)
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
   (let ( (p (get-procedures (dotnet-functions me))) )
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

;;
;; The "funcall"s method
;;
(define (funcalli me i procs)
   (define (needed? p)
      (let ( (arity (global-arity p)) )
	 (if arity
	     ;(or (and (>=fx arity 0) (=fx arity i))
		 ;(and (<fx arity 0) (>= arity (- -1 i))) )
	     (or (and (>=fx arity 0)
		      (if (key-opt? p)
			  (<= arity i)
			  (=fx arity i) ))
		 (and (<fx arity 0) (>= arity (- -1 i))) )
	     (<= i 1) )))
   (let ( (need (map needed? procs)) )
      (if (not (every not need))
	  (let* ( (fname (string-append "funcall" (integer->string i))) )
	     (open-method-virtual me 'obj fname (make-list i 'obj))
	     (declare-maxstack me (+fx i 2))
	     (unless *dotnet-mono-workaround-switch*
		(let push ( (ii 0) )
		   (if (<=fx ii i) (begin (load-par me ii)
					  (push (+fx ii 1)) ))))
	     (load-par me 0)
	     (load-field me 'int "bigloo.procedure" 'index)
	     (compile-funi me i need procs fname)
	     (close-method me) ))))

(define (compile-funi me i need procs fname)
   (define (lab i) (string-append "L" (integer->string i)))
   (define (get-labs i ns ps)
      (if (null? ns)
	  '()
	  (cons (if (car ns) (lab i) "err")
		(get-labs (+fx i 1) (cdr ns) (cdr ps)) )))
   (let ( (labs (get-labs 0 need procs)) )
      (switch me labs)
      (label me "" "err")
      (when *dotnet-mono-workaround-switch*
	 (let push ( (ii 0) )
	    (if (<=fx ii i) (begin (load-par me ii)
				   (push (+fx ii 1)) ))))
      (call-super me 'obj "bigloo.procedure" fname (make-list i 'obj))
      (return me)
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
	     (begin
		(when *dotnet-tail-funcall* (declare-tail me))
		(call-global me (global-entry p)) )
	     (begin (newobj me "bigloo.pair" '(obj obj))
		    (make_cons (-fx n 1)) )))
      (label me "" lab)
      (when *dotnet-mono-workaround-switch*
	 (let push ( (ii 0) )
	    (if (<=fx ii i) (begin (load-par me ii)
				   (push (+fx ii 1)) ))))
      (cond
	 ((eq? arity #f)
	  (if (=fx i 0)
	      (begin (pop me)
		     (load-global me p) )
	      (store-global me p)) )
	 ((>=fx arity 0)
	  (when *dotnet-tail-funcall* (declare-tail me))
	  (if (key-opt? p)
	      (let ( (v (global-entry p)) )
		 (libcall me 'vector 'bigloo.foreign
			  (make-vect i)
			  (make-list i 'obj) )
		 (call-global me v) )
	      (call-global me (global-entry p)) ))
	 (else
	  (push-constant me "nil")
	  (make_cons (+ i 1 arity)) ))
      (return me) ))

;;
;; The apply method
;;
(define (compile-apply me procs)
   (let ( (need (map global-arity procs)) )
      (unless (every not need)
	  (open-method-virtual me 'obj "apply" '(obj))
	  (declare-maxstack me (get-max procs 0))
	  (unless *dotnet-mono-workaround-switch*
	     (load-par me 0))
	  (load-par me 0)
	  (load-field me 'int "bigloo.procedure" 'index)
	  (compile-dispatch me need procs)
	  (close-method me) )))

(define (get-max l max)
   (if (null? l)
       max
       (let ( (nbpush (pushed (car l))) )
	  (get-max (cdr l) (if (>fx nbpush max) nbpush max)) )))

(define (pushed p)
   (let ( (arity (global-arity p)) )
      (cond ((eq? arity #f) 2)
	    ((=fx arity 0) 2)
	    ((>fx arity 0) (+fx arity 1))
	    (else (+fx 2 (-fx -1 arity))) )))

(define (compile-dispatch me need procs)
   (define (lab i) (string-append "L" (integer->string i)))
   (define (get-labs i ns ps)
      (if (null? ns)
	  '()
	  (cons (if (car ns) (lab i) "err")
		(get-labs (+fx i 1) (cdr ns) (cdr ps)) )))
   (let ( (labs (get-labs 0 need procs)) )
      (switch me labs)
      (label me "" "err")
      (when *dotnet-mono-workaround-switch*
	 (load-par me 0))
      (load-par me 1)
      (call-super me 'obj "bigloo.procedure" "apply" '(obj))
      (return me)
      (for-each (lambda (n? lab p) (if n? (compile-for-apply me lab p)))
		need
		labs
		procs )))

(define (compile-for-apply me lab p)
   (let ( (pair (find-type 'pair)) (obj (find-type 'obj)) )
      (define (push-cars me n fixedarity?)
	 (cond
	    ((=fx n 0)
	     (if (not fixedarity?)
		 (load-par me 1) ))
	    ((=fx n 1)
	     (load-par me 1)
	     (castclass me pair)
	     (load-field me obj "bigloo.pair" 'car)
	     (if (not fixedarity?)
		 (begin (load-par me 1)
			(castclass me pair)
			(load-field me obj "bigloo.pair" 'cdr) )))
	    (else
	     (load-par me 1)
	     (castclass me pair)
	     (dup me)
	     (load-field me obj "bigloo.pair" 'cdr)
	     (store-par me 1)
	     (load-field me obj "bigloo.pair" 'car)
	     (push-cars me (- n 1) fixedarity?) )))
      (let ( (arity (global-arity p)) )
	 (label me "" lab)
	 (when *dotnet-mono-workaround-switch*
	    (load-par me 0))
	 (if (>= arity 0)
	     (if (key-opt? p)
		 (begin (load-par me 1)
			(libcall me 'vector 'bigloo.foreign 'list_to_vector
				 '(obj) ))
		 (push-cars me arity #t) )
	     (push-cars me (- -1 arity) #f) )
	 (when *dotnet-tail-funcall* (declare-tail me))
	 (call-global me (global-entry p))
	 (return me) )))

(module saw_procedures
   (import type_type ast_var ast_node ast_env saw_elsewhere)
   (export (get-procedures globals)) )

;;
;; Manu, stp, tu as deja l'info sous la main dans le front-end...
;;

;;
;; Find all globals whicb need a globalref or a loadfun
;;
(define (get-procedures globals)
   (let ( (r '()) )
      (for-each-global! (lambda (global)
			   (if (and (need-function-pointer global)
				    (not (memq global r)) )
			       (set! r (cons global r)) )))
      (for-each (lambda (global)
		   (set! r (get (sfun-body (global-value global)) r)) )
		globals )
      r ))

(define (get* l r)
   (if (null? l)
       r
       (get* (cdr l) (get (car l) r)) ))

;;
;; The generic function.
;;
(define-generic (get e::node r)
   (error 'get "hole" (class-name (object-class e))) )

;;
(define-method (get e::atom r) ; ()
   r )

;;
(define-method (get e::var r) ; ()
   (with-access::var e (variable)
      (if (and (global? variable)
	       (sfun? (variable-value variable))
	       (not (memq variable r)) )
	  (cons variable r)
	  r )))

;;
(define-method (get e::let-var r) ; ()
   (with-access::let-var e (bindings body)
      (get body (get* (map cdr bindings) r)) ))

;;
(define-method (get e::setq r) ; ()
   (with-access::setq e (var value)
      (get value r) ))

;;
(define-method (get e::sequence r) ; ()
   (with-access::sequence e (nodes)
      (get* nodes r) ))

;;
(define-method (get e::conditional r) ; ()
   (with-access::conditional e (test true false)
      (get test (get true (get false r))) ))

;;
(define-method (get e::select r) ; ()
   (with-access::select e (test clauses item-type)
      (get test (get* (map cdr clauses) r)) ))

;;
(define-method (get e::let-fun r) ; ()
   (with-access::let-fun e (locals body)
      (get* (map (lambda (v) (sfun-body (local-value v))) locals)
	    (get body r) )))

;;
(define-method (get e::app r) ; ()
   (with-access::app e (fun args)
      (let ( (r (get* args r)) (v (var-variable fun)) )
	 (if (local? v)
	     r
	     (let ( (id (global-id v)) )
		(if (eq? id '__evmeaning_address)
		    (let ( (g (var-variable (car args))) )
		       (if (not (memq g r))
			   (cons g r)
			   r ))
		    r ))))))

;;
(define-method (get e::app-ly r) ; ()
   (with-access::app-ly e (fun arg)
      (get fun (get arg r)) ))

;;
(define-method (get e::funcall r) ; ()
   (with-access::funcall e (fun args strength)
      (if (eq? strength 'elight)
	  (get* args r)
	  (get fun (get* args r)) )))

;;
(define-method (get e::pragma r) ; ()
   (with-access::pragma e (expr* format)
      (get* expr* r) ))

;;
(define-method (get e::getfield r) ; ()
   (with-access::getfield e (expr* fname ftype otype)
      (get* expr* r) ))

;;
(define-method (get e::setfield r) ; ()
   (with-access::setfield e (expr* fname ftype otype)
      (get* expr* r) ))

;;
(define-method (get e::new r) ; ()
   r )

;;
(define-method (get e::valloc r) ; ()
   (with-access::valloc e (expr* ftype)
      (get* expr* r) ))

;;
(define-method (get e::vref r) ; ()
   (with-access::vref e (expr* ftype vtype)
      (get* expr* r) ))

;;
(define-method (get e::vset! r) ; ()
   (with-access::vset! e (expr* ftype vtype)
      (get* expr* r) ))

;;
(define-method (get e::vlength r) ; ()
   (with-access::vlength e (expr* vtype)
      (get* expr* r) ))

;;
(define-method (get e::isa r) ; ()
   (with-access::isa  e (expr* class)
      (get* expr* r) ))

;;
(define-method (get e::cast-null r) ; ()
   r)

;;
(define-method (get e::cast r) ; ()
   (with-access::cast e (arg)
      (get arg r) ))

;;
(define-method (get e::set-ex-it r) ; ()
   (with-access::set-ex-it e (var body)
      (get var (get body r)) ))

;;
(define-method (get e::jump-ex-it r) ; ()
   (with-access::jump-ex-it e (exit value)
      (get exit (get value r)) ))

;;
(define-method (get e::fail r) ; ()
   (with-access::fail e (proc msg obj)
      (get proc (get msg (get obj r))) ))

;;
(define-method (get e::make-box r) ; ()
   (with-access::make-box e (value)
      (get value r) ))

;;
(define-method (get e::box-ref r) ; ()
   (with-access::box-ref e (var)
      (get var r) ))

;;
(define-method (get e::box-set! r) ; ()
   (with-access::box-set! e (var value)
      (get var (get value r)) ))

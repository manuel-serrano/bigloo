;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/evaluate.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Bernard Serpette                                  */
;*    Creation    :  Fri Jul  2 10:01:28 2010                          */
;*    Last change :  Thu Jan 24 08:52:59 2019 (serrano)                */
;*    Copyright   :  2010-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    New Bigloo interpreter                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate
   
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
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    
	    __pp
	    __reader
	    __progn
	    __expand
	    __evenv
	    __evcompile
	    __everror
	    __evmodule
	    
	    __evaluate_types
	    __evaluate_avar
	    __evaluate_fsize
	    __evaluate_uncomp
	    __evaluate_comp)
   
   (export  (evaluate2 sexp env loc)
	    (get-evaluation-context)
	    (set-evaluation-context! v)
	    (evaluate2-restore-bp! ::int)
	    (evaluate2-restore-state! ::vector)))

;*---------------------------------------------------------------------*/
;*    untype-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define (untype-ident id::symbol)
   (let* ((string (symbol->string id))
	  (len (string-length string)))
      (let loop ((walker  0))
	 (cond
	    ((=fx walker len)
	     (cons id #f))
	    ((and (char=? (string-ref string walker) #\:)
		  (<fx walker (-fx len 1))
		  (char=? (string-ref string (+fx walker 1)) #\:))
	     (cons (string->symbol (substring string 0 walker))
		(string->symbol (substring string (+fx walker 2)))))
	    (else
	     (loop (+fx walker 1)))))))

;*---------------------------------------------------------------------*/
;*    get-evaluation-context ...                                       */
;*---------------------------------------------------------------------*/
(define (get-evaluation-context)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref s 0)) )
	 (let ( (r (make-vector bp "")) )
	    (let rec ( (i 0) )
	       (when (<fx i bp)
		  (vector-set! r i (vector-ref s i))
		  (rec (+fx i 1)) ))
	    r ))))

;*---------------------------------------------------------------------*/
;*    set-evaluation-context! ...                                      */
;*---------------------------------------------------------------------*/
(define (set-evaluation-context! v)
   (let ( (s (find-state)) )
      (let ( (bp (vector-ref v 0)) )
	 (let rec ( (i 0) )
	    (when (<fx i bp)
	       (vector-set! s i (vector-ref v i))
	       (rec (+fx i 1)) )))))

;*---------------------------------------------------------------------*/
;*    evaluate2-restore-bp! ...                                        */
;*---------------------------------------------------------------------*/
(define (evaluate2-restore-bp! bp)
   (let ((s ($evmeaning-evstate (current-dynamic-env))))
      (vector-set! s 0 bp)))

;*---------------------------------------------------------------------*/
;*    evaluate2-restore-state! ...                                     */
;*---------------------------------------------------------------------*/
(define (evaluate2-restore-state! state)
   (let ((env (current-dynamic-env)))
      ($evmeaning-evstate-set! env state)))

;*---------------------------------------------------------------------*/
;*    evaluate2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (evaluate2 sexp env loc)
   (let ( (ast (extract-loops (convert sexp env loc))) )
      (analyse-vars ast)
      (let ( (n (frame-size ast)) )
	 (let ( (f (compile ast)) )
	    (let ( (s (find-state)) )
	       (let ( (bp (vector-ref s 0)) )
		  (unwind-protect
		     (f s)
		     (vector-set! s 0 bp) )))))))

;*---------------------------------------------------------------------*/
;*    get-location ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-location exp loc)
   (or (get-source-location exp) loc))

;*---------------------------------------------------------------------*/
;*    get-location3 ...                                                */
;*---------------------------------------------------------------------*/
(define (get-location3 exp lst loc)
   (or (get-source-location exp)
       (get-source-location lst)
       loc))

;*---------------------------------------------------------------------*/
;*    convert ...                                                      */
;*---------------------------------------------------------------------*/
(define (convert e globals loc)
   (conv e '() globals #f 'toplevel loc #t) )

;*---------------------------------------------------------------------*/
;*    conv-var ...                                                     */
;*---------------------------------------------------------------------*/
(define (conv-var v locals)
   (let rec ( (l locals) )
      (if (null? l)
	  #f
	  (let ( (rv (car l)) )
	     (with-access::ev_var rv (name)
		(if (eq? v name)
		    rv
		    (rec (cdr l)) ))))))

;*---------------------------------------------------------------------*/
;*    conv-begin ...                                                   */
;*---------------------------------------------------------------------*/
(define (conv-begin l locals globals tail? where loc top?)
   (let ( (loc (get-location l loc)) )
      (match-case l
	 (()
	  (instantiate::ev_litt
	     (value #unspecified)))
	 ((?e)
	  (conv e locals globals tail? where (get-location e loc) top?))
	 ((?e1 . ?r)
	  (instantiate::ev_prog2
	     (e1 (conv e1 locals globals #f where (get-location e1 loc) top?))
	     (e2 (conv-begin r locals globals tail? where loc top?)) ))
	 (else
	  (evcompile-error loc "eval" "Bad syntax" l)) )))

;*---------------------------------------------------------------------*/
;*    conv-global ...                                                  */
;*---------------------------------------------------------------------*/
(define (conv-global loc id globals)
   (instantiate::ev_global
      (loc loc)
      (name id)
      (mod (if (evmodule? globals) globals ($eval-module)))))

;*---------------------------------------------------------------------*/
;*    conv-field-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (conv-field-ref e locals globals tail? where loc top?)
   (let* ( (l (cdr e))
	   (v (conv-var (car l) locals)) )
      (if (isa? v ev_var)
	  (with-access::ev_var v (type name)
	     (let loop ( (node v)
			 (klass (class-exists type))
			 (fields (cdr l)) )
		(cond
		   ((null? fields)
		    node)
		   ((class? klass)
		    (let ( (field (find-class-field klass (car fields))) )
		       (if (class-field? field)
			   (let ( (node (make-class-field-ref
					   field node loc tail?)) )
			      (loop node
				 (class-field-type field)
				 (cdr fields)) )
			   (evcompile-error loc type
			      (format "Class \"~a\" has no field \"~a\"" type (car fields))
			      e) )))
		   (else
		    (evcompile-error loc (or type name)
		       "Static type not a class" e) ))))
	  (evcompile-error loc (cadr e) "Variable unbound" e) )))

;*---------------------------------------------------------------------*/
;*    conv-field-set ...                                               */
;*---------------------------------------------------------------------*/
(define (conv-field-set l e2 e locals globals tail? where loc top?)
   (let* ( (v (conv-var (car l) locals))
	   (e2 (conv e2 locals globals #f where loc #f)) )
      (if (isa? v ev_var)
	  (with-access::ev_var v (type name)
	     (let loop ( (node v)
			 (klass (class-exists type))
			 (fields (cdr l)) )
		(cond
		   ((null? fields)
		    node)
		   ((class? klass)
		    (let ( (field (find-class-field klass (car fields))) )
		       (if (class-field? field)
			   (if (null? (cdr fields))
			       (if (class-field-mutable? field)
				   (make-class-field-set
				      field (list node e2) loc tail?)
				   (evcompile-error loc (car fields)
				      "Field read-only"
				      e))
			       (let ( (node (make-class-field-ref
					       field node loc tail?)) )
				  (loop node
				     (class-field-type field)
				     (cdr fields)) ))
			   (evcompile-error loc type
			      (format "Class \"~a\" has no field \"~a\"" type (car fields))
			      e) )))
		   (else
		    (evcompile-error loc
		       (or type name) "Static type not a class" e) ))))
	  (evcompile-error loc (car l) "Variable unbound" e) )))

;*---------------------------------------------------------------------*/
;*    make-class-field-ref ...                                         */
;*---------------------------------------------------------------------*/
(define (make-class-field-ref field arg loc tail?)
   (let ( (get (class-field-accessor field)) )
      (instantiate::ev_app
	 (loc loc)
	 (fun (instantiate::ev_litt (value get)))
	 (args (list arg))
	 (tail? tail?))))

;*---------------------------------------------------------------------*/
;*    make-class-field-set ...                                         */
;*---------------------------------------------------------------------*/
(define (make-class-field-set field args loc tail?)
   (let ( (set (class-field-mutator field)) )
      (instantiate::ev_app
	 (loc loc)
	 (fun (instantiate::ev_litt (value set)))
	 (args args)
	 (tail? tail?))))

;*---------------------------------------------------------------------*/
;*    evepairify ...                                                   */
;*---------------------------------------------------------------------*/
(define (evepairify p loc)
   (econs (car p) (cdr p) loc))

;*---------------------------------------------------------------------*/
;*    type-check ...                                                   */
;*---------------------------------------------------------------------*/
(define (type-check var tname loc procname body)
   
   (if (symbol? tname)
       (let ((pred (case tname
		      ((pair) 'pair?)
		      ((vector) 'vector?)
		      ((symbol) 'symbol?)
		      ((char) 'char?)
		      ((int bint) 'integer?)
		      ((real breal) 'real?)
		      ((bool) 'boolean?)
		      ((struct) 'struct?)
		      ((class) 'class?)
		      ((string bstring) 'string?)
		      (else `(lambda (o)
				(let ((c (class-exists ',tname)))
				   (if c (isa? o c) #t)))))))
	  (evepairify
	     `(if (,pred ,var)
		  ,body
		  ,(match-case loc
		      ((at ?fname ?pos)
		       `(bigloo-type-error/location
			   ,(when (symbol? procname) (symbol->string procname))
			   ,(symbol->string tname) ,var
			   ,fname ,pos))
		      (else
		       `(bigloo-type-error
			   ,(when (symbol? procname) (symbol->string procname))
			   ,(symbol->string tname) ,var))))
	     loc))
       body))

;*---------------------------------------------------------------------*/
;*    type-checks ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-checks vars srcs body loc procname)
   (if (<=fx (bigloo-debug) 0)
       body
       (let loop ((vars vars)
		  (srcs srcs))
	  (if (null? vars)
	      body
	      (let* ((v (car vars))
		     (id (car v))
		     (tname (cdr v)))
		 (if tname
		     (let ((loc (get-location3 (car srcs) srcs loc)))
			(type-check id tname loc procname
			   (loop (cdr vars) (cdr srcs))))
		     (loop (cdr vars) (cdr srcs))))))))

;*---------------------------------------------------------------------*/
;*    type-result ...                                                  */
;*---------------------------------------------------------------------*/
(define (type-result type body loc)
   (if (and type (>=fx (bigloo-debug) 1))
       (let ((tmp (gensym 'tmp)))
	  (evepairify
	     `(let ((,(string->symbol (format "~a::~a" tmp type)) ,body))
		 ,tmp)
	     (get-location body loc)))
       body))

;*---------------------------------------------------------------------*/
;*    conv ...                                                         */
;*---------------------------------------------------------------------*/
(define (conv e locals globals tail? where loc top?)
   
   (define (rconv/loc e loc)
      (conv e locals globals tail? where loc #f))
   
   (define (rconv e)
      (rconv/loc e (get-location e loc)))
   
   (define (uconv/loc+where e loc where)
      (conv e locals globals #f where loc #f))
   
   (define (uconv/loc e loc)
      (conv e locals globals #f where loc #f))
   
   (define (uconv e)
      (uconv/loc e (get-location e loc)))
   
   (define (uconv/where e where)
      (uconv/loc+where e (get-location e loc) where))
   
   (define (uconv* es)
      (let loop ((es es))
	 (if (null? es)
	     '()
	     (let ((e (car es)))
		(cons (conv e locals globals #f where (get-location3 e es loc) #f)
		      (loop (cdr es)))))))
   
   (define (conv-lambda formals body where type)
   
      (define (split-formals l)
	 (let rec ( (r l) (flat '()) (arity 0) )
	    (cond
	       ((null? r)
		(values (reverse! flat) arity))
	       ((not (pair? r))
		(values (reverse! (cons (untype-ident r) flat)) (-fx -1 arity)))
	       (else
		(rec (cdr r)
		     (cons (untype-ident (car r)) flat) (+fx arity 1))) )))
      
      (multiple-value-bind (args arity)
	 (split-formals (dsssl-formals->scheme-typed-formals formals error #t))
	 (let ( (vars (map (lambda (v)
			      (instantiate::ev_var
				 (name (car v))
				 (type (cdr v))) )
			 args ))
		(body (make-dsssl-function-prelude e formals
			 (type-checks args args (type-result type body loc) loc where)
			 error))
		(nloc (get-location body loc)) )
	    (instantiate::ev_abs
	       (loc loc)
	       (where where)
	       (arity arity)
	       (vars vars)
	       (body (conv body (append vars locals) globals #t where nloc #f)) ))))

   (match-case e
      ((atom ?x)
       (if (symbol? x)
	   (or (conv-var x locals) (conv-global loc x globals))
	   (instantiate::ev_litt
	      (value x)) ))
      ((module . ?bah)
       (if top?
	   (let ((forms (evmodule e (get-location e loc))))
	      (conv (expand forms) locals
		 ($eval-module) where #f loc #t))
	   (evcompile-error loc "eval" "Illegal non toplevel module declaration" e) ))
      ((@ (and ?id (? symbol?)) (and ?modname (? symbol?)))
       (instantiate::ev_global
	  (loc loc)
	  (name id)
	  (mod (eval-find-module modname))))
      ((-> . ?l)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (conv-field-ref e locals globals tail? where loc top?)
	   (evcompile-error loc "eval" "Illegal form" e) ))
      (((and (? symbol?)
	     (? (lambda (x) (conv-var x locals)))
	     ?fun)
	. ?args)
       (let ( (fun (uconv fun)) (args (uconv* args)) )
	  (instantiate::ev_app
	     (loc loc)
	     (fun fun)
	     (args args)
	     (tail? tail?)) ))
      ((trap ?e)
       (instantiate::ev_trap
	  (e (uconv e))) )
      ((quote ?v)
       (instantiate::ev_litt
	  (value v)) )
      ((if ?p ?t ?o)
       (instantiate::ev_if
	  (p (uconv/loc p (get-location p loc)))
	  (t (rconv/loc t (get-location t loc)))
	  (e (rconv/loc o (get-location o loc)))) )
      ((if ?p ?t)
       (instantiate::ev_if
	  (p (uconv/loc p (get-location p loc)))
	  (t (rconv/loc t (get-location t loc)))
	  (e (rconv/loc e (get-location #f loc)))) )
      (((kwote or) . ?args)
       (instantiate::ev_or
	  (args (uconv* args))) )
      (((kwote and) . ?args)
       (instantiate::ev_and
	  (args (uconv* args))) )
      ((begin . ?l)
       (conv-begin l locals globals tail? where loc top?) )
      ((let ?binds . ?body)
       (let* ( (ubinds (map (lambda (b) (untype-ident (car b))) binds))
	       (vars (map (lambda (i)
			     (instantiate::ev_var
				(name (car i))
				(type (cdr i)) ))
			ubinds))
	       (body (if (pair? (cdr body)) (econs 'begin body loc) (car body)))
	       (tbody (type-checks ubinds binds body loc where)) )
	  (let ( (bloc (get-location binds loc)) )
	     (instantiate::ev_let
		(vars vars)
		(vals (map (lambda (b)
			      (let ( (loc (get-location b bloc)) )
				 (uconv/loc (cadr b) loc) ))
			 binds))
		(body (conv tbody (append vars locals) globals tail? where loc #f)) ))))
      ((let* ?binds . ?body)
       (define (conv-vals l vars locals loc)
	  (if (null? l)
	      '()
	      (let ( (loc (get-location (car l) loc)) )
		 (cons (conv (cadar l) locals globals #f where loc #f)
		       (conv-vals (cdr l) (cdr vars) (cons (car vars) locals) loc) ))))
       (let ( (vars (map (lambda (b)
			    (let ( (i (untype-ident (car b))) )
			       (instantiate::ev_var
				  (name (car i))
				  (type (cdr i)) )))
			 binds))
	      (bloc (get-location binds loc)) )
	  (instantiate::ev_let*
	     (vars vars)
	     (vals (conv-vals binds vars locals bloc))
	     (body (conv-begin body (append (reverse vars) locals) globals tail? where loc #f)) )))
      ((letrec ?binds . ?body)
       (let* ( (ubinds (map (lambda (b) (untype-ident (car b))) binds))
	       (vars (map (lambda (i)
			     (instantiate::ev_var
				(name (car i))
				(type (cdr i)) ))
			  ubinds))
	       (locals (append vars locals))
	       (body (if (pair? (cdr body)) (econs 'begin body loc) (car body)))
	       (tbody (type-checks ubinds binds body loc where))
	       (bloc (get-location binds loc)) )
	  (instantiate::ev_letrec
	     (vars vars)
	     (vals (map (lambda (b)
			   (conv (cadr b) locals globals #f (symbol-append (car b) '| | where) (get-location b bloc) #f)) binds) )
	     (body (conv tbody locals globals tail? where loc #f) ))))
      ((set! (@ (and ?id (? symbol?)) (and ?modname (? symbol?))) ?e)
       (instantiate::ev_setglobal
	  (loc loc)
	  (name id)
	  (mod (eval-find-module modname))
	  (e (uconv e))))
      ((set! (-> . ?l) ?e2)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (conv-field-set l e2 e locals globals tail? where loc top?)
	   (evcompile-error loc "eval" "Illegal form" e) ))
      ((set! ?v ?e)
       (let ( (cv (conv-var v locals)) (e (uconv e)) )
	  (if cv
	      (instantiate::ev_setlocal
		 (v cv)
		 (e e))
	      (instantiate::ev_setglobal
		 (loc loc)
		 (name v)
		 (mod (if (evmodule? globals) globals ($eval-module)))
		 (e e)) )))
      ((set! . ?-)
       (evcompile-error loc "eval" "Illegal form" e))
      ((define ?gv (lambda ?formals ?body))
       (let ((tid (untype-ident gv)))
	  (instantiate::ev_defglobal
	     (loc loc)
	     (name (car tid))
	     (mod (if (evmodule? globals) globals ($eval-module)))
	     (e (conv-lambda formals body gv (cdr tid))) )))
      ((define ?gv ?ge)
       (let ( (tid (untype-ident gv)) )
	  (instantiate::ev_defglobal
	     (loc loc)
	     (name (car tid))
	     (mod (if (evmodule? globals) globals ($eval-module)))
	     (e (uconv/where
		   (type-result (cdr tid) ge loc)
		   (if top? gv where))) )))
      ((bind-exit (?v) . ?body)
       (let ( (var (instantiate::ev_var (name v) (type #f))) )
	  (instantiate::ev_bind-exit
	     (var var)
	     (body (conv-begin body (cons var locals) globals #f where loc #f)) )))
      ((unwind-protect ?e . ?body)
       (instantiate::ev_unwind-protect
	  (e (uconv e))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((with-handler ?h . ?body)
       (instantiate::ev_with-handler
	  (handler (uconv h))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((synchronize ?m :prelock ?p . ?body)
       (instantiate::ev_synchronize
	  (loc loc)
	  (mutex (uconv m))
	  (prelock (uconv p))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((synchronize ?m . ?body)
       (instantiate::ev_synchronize
	  (loc loc)
	  (mutex (uconv m))
	  (prelock (uconv '()))
	  (body (conv-begin body locals globals #f where loc #f)) ))
      ((lambda ?formals ?body)
       (conv-lambda formals body (symbol-append '\@ where) #f) )
      ((free-pragma::obj . ?-)
       (error "free-pragma" "not supported in eval" e))
      ((?f . ?args)
       (let ( (fun (uconv f)) (args (uconv* args)) )
	  (instantiate::ev_app
	     (loc loc)
	     (fun fun)
	     (args args)
	     (tail? tail?)) ))
      (else (evcompile-error loc "eval" "Bad syntax" e)) ))


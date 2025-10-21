;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Ast/toplevel.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 20 15:11:28 2025                          */
;*    Last change :  Mon Oct 20 17:50:17 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    AST construction of the toplevel forms                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_toplevel
   
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Tools/trace.sch")
   
   (import  ast_find-gdefs
	    ast_glo-def
	    ast_ident
	    ast_env
	    ast_local
	    ast_sexp
	    ast_let
	    ast_unit
	    backend_backend
	    object_class
	    object_generic
	    object_method
	    tools_progn
	    tools_args
	    tools_misc
	    tools_speek
	    tools_shape
	    tools_location
	    tools_error
	    tools_dsssl
	    engine_param
	    module_module
	    module_class
	    module_include
	    module_impuse
	    type_cache
	    type_env
	    expand_eps)
   
  (export  (toplevel*->ast::pair-nil ::pair-nil ::pair-nil ::symbol ::obj)
	   (toplevel->ast ::obj ::pair-nil ::symbol ::obj)))

;*---------------------------------------------------------------------*/
;*    toplevel*->ast ...                                               */
;*---------------------------------------------------------------------*/
(define (toplevel*->ast::pair-nil sexp*::pair-nil gdefs module genv)
   (let loop ((sexp* sexp*)
	      (aexp* '()))
      (if (null? sexp*)
	  (reverse! aexp*)
	  (loop (cdr sexp*)
	     (append (toplevel->ast (car sexp*) gdefs module genv) aexp*)))))

;*---------------------------------------------------------------------*/
;*    toplevel->ast ...                                                */
;*    -------------------------------------------------------------    */
;*    !!! WARNING: this function must build a reversed list !!!        */
;*---------------------------------------------------------------------*/
(define (toplevel->ast sexp gdefs module genv)
   (match-case sexp
      ((begin)
       (list sexp))
      ((begin . ?nsexp*)
       (reverse! (toplevel*->ast nsexp* gdefs module genv)))
      ((define (?var . ?args) . ?exp)
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module genv id module)))
	  ;; exported variable are set to be written hence, we don't
	  ;; have to check here if the variable has been declared has
	  ;; exported (as a variable vs a function). We just have to
	  ;; check if it is written.
	  ;; We may not find global in def because it can has been
	  ;; introduced after the computation of gdefs (for instance
	  ;; if it is a default method of a generic).
	  (when (and (type-exists? var) (isa? (find-type var) tclass))
	     (error-class-shadow var module sexp))
	  (if (or (not (pair? def))
		  (and (eq? (car (cdr def)) 'read)
		       (or (not (global? global))
			   (eq? (global-access global) 'read))))
	      (make-sfun-definition var
		 module args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
		 sexp 'sfun genv)
	      (let ((new-sexp `(set! ,var (lambda ,args ,@exp))))
		 (replace! sexp new-sexp)
		 (make-svar-definition var sexp genv)))))
      ((define ?var (lambda ?args . ?exp))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module genv id module)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read)))
	      (make-sfun-definition var
		 module args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
		 sexp 'sfun genv)
	      (make-svar-definition var sexp genv))))
      ((define ?var (labels ((?f ?args . ?exp)) ?f))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module genv id module)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read))
		   (eq? id f))
	      (make-sfun-definition var
		 module args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
		 sexp 'sfun genv)
	      (make-svar-definition var sexp genv))))
      ((define ?var ((and ?lam (? lambda?)) ?args . ?exp))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module genv id module))
	      (tlam (type-of-id lam #f)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read)))
	      (make-sfun-definition (make-typed-ident id (type-id tlam))
		 module args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
		 sexp 'sfun genv)
	      (make-svar-definition var sexp genv))))
      ((define ?var (begin ?1-exp))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (set-car! (cddr sexp) 1-exp)
       (toplevel->ast sexp gdefs module genv))
      ((define ?var (and (? symbol?) ?var2))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let* ((g (find-global genv var2))
		     (arity (and (global? g)
				 (fun? (global-value g))
				 (fun-arity (global-value g)))))
		 (if (fixnum? arity)
		     (cond
			((global-optional? g)
			 (user-warning var
			    "Unable to eta-expand #!optional alias"
			    sexp)
			 (make-svar-definition var sexp genv))
			((global-key? g)
			 (user-warning var
			    "Unable to eta-expand #!key alias"
			    sexp)
			 (make-svar-definition var sexp genv))
			(else
			 (let ((def (eta-expanse sexp arity)))
			    (toplevel->ast def gdefs module genv))))
		     (make-svar-definition var sexp genv)))
	      (make-svar-definition var sexp genv))))
      ((define ?var (@ (and (? symbol?) ?var2) (and (? symbol?) ?module)))
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let* ((g (find-global/module genv var2 module))
		     (arity (and (global? g)
				 (fun? (global-value g))
				 (global-arity g))))
		 (if (fixnum? arity)
		     (cond
			((global-optional? g)
			 (user-warning var
			    "Unable to eta-expand #!optional alias"
			    sexp)
			 (make-svar-definition var sexp genv))
			((global-key? g)
			 (user-warning var
			    "Unable to eta-expand #!key alias"
			    sexp)
			 (make-svar-definition var sexp genv))
			(else
			 (let ((def (eta-expanse sexp arity)))
			    (toplevel->ast def gdefs module genv))))
		     (make-svar-definition var sexp genv)))
	      (make-svar-definition var sexp genv))))
      ((define ?var . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (unless (match-case exp
		     ((((@ register-class! __object) . ?-)) #t)
		     (else #f))
	     (error-class-shadow var module sexp)))
       (make-svar-definition var sexp genv))
      ((define-inline ((@ ?var ?module) . ?args) . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (make-sfun-definition var
	  module args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
	  sexp 'sifun genv))
      ((define-inline (?var . ?args) . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (make-sfun-definition var
	  module args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
	  sexp 'sifun genv))
      ((define-generic ((@ ?var ?module) . ?args) . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (warning "define-generic" "form no longer supported" sexp)
       (make-generic-definition var module args exp sexp genv))
      ((define-generic (?var . ?args) . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (make-generic-definition var module args exp sexp genv))
      ((define-method (?var . ?args) . ?exp)
       (when (and (type-exists? var) (isa? (find-type var) tclass))
	  (error-class-shadow var module sexp))
       (make-method-definition genv var
	  args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)) genv)
	  sexp genv))
      (else
       (list sexp))))

;*---------------------------------------------------------------------*/
;*    error-class-shadow ...                                           */
;*---------------------------------------------------------------------*/
(define (error-class-shadow id module src)
   (let ((loc (find-location src))
	 (msg "Illegal class shadowing"))
      (if loc
	  (user-error/location loc module msg id)
	  (user-error module msg id))))

;*---------------------------------------------------------------------*/
;*    lambda? ...                                                      */
;*---------------------------------------------------------------------*/
(define (lambda? sym)
   (and (symbol? sym) (eq? (fast-id-of-id sym #f) 'lambda)))
	
;*---------------------------------------------------------------------*/
;*    eta-expanse ...                                                  */
;*---------------------------------------------------------------------*/
(define (eta-expanse sexp arity)
   (let ((args (make-n-proto arity)))
      (define (do-eta-expanse/module var id2 module)
	 (cond
	    ((>=fx arity 0)
	     `(define ,(cons var args)
		 ((@ ,id2 ,module) ,@args)))
	    ((=fx arity -1)
	     `(define ,(cons var args)
		 (apply (@ ,id2 ,module) ,args)))
	    (else
	     `(define ,(cons var args)
		 (apply (@ ,id2 ,module)
			(cons* ,@(args*->args-list args)))))))
      (define (do-eta-expanse var id2)
	 (cond
	    ((>=fx arity 0)
	     `(define ,(cons var args)
		 (,id2 ,@args)))
	    ((=fx arity -1)
	     `(define ,(cons var args)
		 (apply ,id2 ,args)))
	    (else
	     `(define ,(cons var args)
		 (apply ,id2 (cons* ,@(args*->args-list args)))))))
      (match-case sexp
	 ((define ?var1 (@ ?var2 ?module2))
	  (let* ((id2 (id-of-id var2 (find-location sexp))))
	     (replace! sexp (do-eta-expanse/module var2 id2 module2))))
	 ((define ?var1 ?var2)
	  (let ((id2 (id-of-id var2 (find-location sexp))))
	     (replace! sexp (do-eta-expanse var1 id2)))))))
	  
;*---------------------------------------------------------------------*/
;*    make-sfun-definition ...                                         */
;*---------------------------------------------------------------------*/
(define (make-sfun-definition id::symbol module::symbol args body src class genv)
   (let ((loc (find-location src))
	 (opts (dsssl-optionals args))
	 (keys (dsssl-keys args)))
      (cond
	 ((pair? opts)
	  (make-sfun-opt-definition opts id module args body src class loc genv))
	 ((pair? keys)
	  (make-sfun-key-definition keys id module args body src class loc genv))
	 (else
	  (make-sfun-noopt-definition id module args body src class loc genv)))))

;*---------------------------------------------------------------------*/
;*    make-sfun-opt-definition ...                                     */
;*---------------------------------------------------------------------*/
(define (make-sfun-opt-definition optionals id module args body src class loc genv)
   (let* ((locals (parse-fun-opt-args args args loc))
	  (glo (def-global-sfun! genv id args locals module class src 'now body))
	  (clo (make-sfun-opt-closure glo optionals id module args body src class loc genv)))
      (list glo clo)))

;*---------------------------------------------------------------------*/
;*    make-sfun-opt-closure ...                                        */
;*    -------------------------------------------------------------    */
;*    This function is actually the closure associated with the        */
;*    global. Until the closure allocation pass it is handled as a     */
;*    regular function. Then the closure allocation sets it            */
;*    as the closure of the global function.                           */
;*    -------------------------------------------------------------    */
;*    See Globalize/gloclo.scm:make-opt-global-closure for             */
;*    more details.                                                    */
;*---------------------------------------------------------------------*/
(define (make-sfun-opt-closure glo optionals id module args body src class loc genv)
   
   (define arity (sfun-arity (global-value glo)))

   (define (funcall-vector opt)
      (let ((lopt (length optionals))
	    (forms (map local-id (sfun-args (global-value glo))))
	    (opts (map (lambda (o)
			  (fast-id-of-id (car o) loc))
		     (sfun-optionals (global-value glo)))))
	 `(,(let-sym) ,(map (lambda (v i)
			       `(,v ($vector-ref-ur ,opt ,i)))
			  (take (sfun-args-name (global-value glo)) arity)
			  (iota arity))
		      (case ($vector-length ,opt)
			 ,@(map (lambda (i)
				   `((,(+fx arity i))
				     (let* (,@(map (lambda (v j)
						      `(,v ($vector-ref-ur ,opt ,j)))
						 (take (drop forms arity) i)
						 (iota i arity))
					      ,@(if (<=fx i lopt)
						    (drop optionals i)
						    '()))
					(,glo
					   ;; required unbound parameters
					   ,@(take (sfun-args-name (global-value glo)) arity)
					   ;; optional parameters
					   ,@opts))))
			    (iota (+fx lopt 1)))
			 (else
			  ,(if *unsafe-arity*
			       #unspecified
			       `((@ error __error)
				 ',id
				 ,(string-append
				     "wrong number of arguments: ["
				     (integer->string arity)
				     ".." (integer->string (+ arity lopt))
				     "] expected, provided")
				 ($vector-length ,opt))))))))

   (define (funcall-pair opt)
      (let ((tmp (gensym 'vec)))
	 `(let ((,tmp (list->vector ,opt)))
	     ,(funcall-vector tmp))))

   (if (backend-varargs (the-backend))
       (let* ((id (symbol-append '_ id))
	      (optid (gensym 'opt))
	      (envid (gensym 'env))
	      (opt (make-local-svar optid *vector*))
	      (env (make-local-svar envid *procedure*))
	      (g (def-global-sfun! genv id (list envid optid)
		    (list env opt) module class
		    src 'globalization
		    (compile-expand (comptime-expand (funcall-vector optid))))))
	  (global-evaluable?-set! g #f)
	  (global-type-set! g *obj*)
	  g)
       (let* ((id (symbol-append '_ id))
	      (optid (gensym 'opt))
	      (envid (gensym 'env))
	      (opt (make-local-svar optid *pair*))
	      (env (make-local-svar envid *procedure*))
	      (g (def-global-sfun! genv id (list envid optid)
		    (list env opt) module class
		    src 'globalization
		    (compile-expand (comptime-expand (funcall-pair optid))))))
	  (global-evaluable?-set! g #f)
	  (global-type-set! g *obj*)
	  g)))

;*---------------------------------------------------------------------*/
;*    make-sfun-key-definition ...                                     */
;*---------------------------------------------------------------------*/
(define (make-sfun-key-definition keys id module args body src class loc genv)
   (let* ((locals (append (map (lambda (a)
				  (let* ((pid (check-id (parse-id a loc) src))
					 (id (car pid))
					 (type (cdr pid)))
				     (if (user-symbol? id)
					 (make-user-local-svar id type)
					 (make-local-svar id type))))
			       (dsssl-before-dsssl args))
			  (map (lambda (o)
				  (let* ((a (car o))
					 (pid (check-id (parse-id a loc) src))
					 (id (car pid))
					 (type (cdr pid)))
				     (make-user-local-svar id type)))
			       keys)))
	  (glo (def-global-sfun! genv id args locals module class src 'now body))
	  (clo (make-sfun-key-closure glo keys id module args body src class loc genv)))
      (list glo clo)))

;*---------------------------------------------------------------------*/
;*    make-sfun-key-closure ...                                        */
;*    -------------------------------------------------------------    */
;*    See make-sfun-opt-closure.                                       */
;*---------------------------------------------------------------------*/
(define (make-sfun-key-closure glo keys id module args body src class loc genv)
   
   (define arity (sfun-arity (global-value glo)))
   (define iopt (gensym 'opt))
   (define ienv (gensym 'env))
   (define lopt (length keys))
   (define l (gensym 'l))
   (define search (gensym 'search))
   (define check (gensym 'check))
   (define var (gensym 'var))
   
   (define (all-keys keys)
      ;; compute the whole keywords set from the declaration
      (map (lambda (k)
	      (symbol->keyword (fast-id-of-id (car k) loc)))
	 keys))
   
   (define (funcall-vector iopt)
      `(,(let-sym) ((,l (vector-length ,iopt)))
		   (labels ((,search (k1 i)
			       (if (=fx i ,l)
				   -1
				   ,(if (and *unsafe-arity*
					     (or (not (global-evaluable? glo))
						 *unsafe-eval*))
					`(,(let-sym) ((v ($vector-ref-ur ,iopt i)))
						     (if (eq? v k1)
							 (+fx i 1)
							 (,search k1 (+fx i 2))))
					`(if (=fx i (-fx ,l 1))
					     ((@ error __error)
					      ',id
					      ,(string-append
						  "wrong number of arguments: ["
						  (integer->string arity)
						  ".." (integer->string (+ arity lopt))
						  "] expected, provided")
					      ($vector-length ,iopt))
					     (let ((v ($vector-ref-ur ,iopt i)))
						(if (eq? v k1)
						    (+fx i 1)
						    (,search k1 (+fx i 2)))))))))
		      (let ,(map (lambda (v i)
				    `(,v ($vector-ref-ur ,iopt ,i)))
			       (take (sfun-args-name (global-value glo)) arity)
			       (iota arity))
			 (let* ,(map (lambda (p) (list (car p) (cadr p))) keys)
			    ;; arity check in safe mode
			    ,(if (and *unsafe-arity*
				      (or (not (global-evaluable? glo))
					  *unsafe-eval*))
				 #unspecified
				 `(labels ((,check (i)
					      (if (=fx i ,l)
						  '()
						  (if (memq (vector-ref ,iopt i)
							 ',(all-keys keys))
						      (,check (+fx i 2))
						      ((@ error __error)
						       ',id
						       "Illegal keyword argument"
						       (vector-ref ,iopt i))))))
				     (,check ,arity)))
			    ,@(map (lambda (p)
				      (let* ((i (fast-id-of-id (car p) loc))
					     (k1 (symbol->keyword i))
					     (ind (gensym 'index)))
					 `(,(let-sym) ((,ind (,search ,k1 ,arity)))
						      (when (>=fx ,ind 0)
							 (set! ,i ($vector-ref-ur ,iopt ,ind))))))
				 keys)
			    (,glo ,@(map (lambda (j)
					    `($vector-ref-ur ,iopt ,j))
				       (iota arity))
			       ,@(append-map (lambda (p)
						(let ((id (fast-id-of-id
							     (car p) loc)))
						   (list
						      (symbol->keyword id)
						      id)))
				    keys)))))))
   
   (define (funcall-pair iopt)
      (let ((tmp (gensym 'vec)))
	 `(let ((,tmp (list->vector ,iopt)))
	     ,(funcall-vector tmp))))
   
   (if (backend-varargs (the-backend))
       (let* ((id (symbol-append '_ id))
	      (opt (make-local-svar iopt *vector*))
	      (env (make-local-svar ienv *procedure*))
	      (g (def-global-sfun! genv id (list ienv iopt) (list env opt) module class
		    src 'globalization
		    (compile-expand
		       (comptime-expand (funcall-vector iopt))))))
	  (global-type-set! g *obj*)
	  (global-evaluable?-set! g #f)
	  g)
       (let* ((id (symbol-append '_ id))
	      (opt (make-local-svar iopt *pair*))
	      (env (make-local-svar ienv *procedure*))
	      (g (def-global-sfun! genv id (list ienv iopt) (list env opt) module class
		    src 'globalization
		    (compile-expand
		       (comptime-expand (funcall-pair iopt))))))
	  (global-type-set! g *obj*)
	  (global-evaluable?-set! g #f)
	  g)))

;*---------------------------------------------------------------------*/
;*    parse-fun-args ...                                               */
;*---------------------------------------------------------------------*/
(define (parse-fun-args args src loc)
   (let loop ((args args)
	      (res '()))
      (cond
	 ((null? args)
	  (reverse! res))
	 ((not (pair? args)) 
	  (let* ((pid (check-id (parse-id args loc) src))
		 (id (car pid))
		 (type (cdr pid)))
	     ;; there is no need to check the last
	     ;; n-ary formal argument because it will
	     ;; be checked when defining the global variable
	     (reverse! (cons (make-user-local-svar id type) res))))
	 ((dsssl-named-constant? (car args))
	  (let ((arg (dsssl-find-first-formal args)))
	     (if arg
		 (reverse! (cons (make-user-local-svar arg *obj*) res))
		 (reverse! res))))
	 (else
	  (let* ((pid (check-id (parse-id (car args) loc) src))
		 (id (car pid))
		 (type (cdr pid)))
	     (loop (cdr args)
		(cons (if (user-symbol? id)
			  (make-user-local-svar id type)
			  (make-local-svar id type))
		   res)))))))

;*---------------------------------------------------------------------*/
;*    parse-fun-opt-args ...                                           */
;*---------------------------------------------------------------------*/
(define (parse-fun-opt-args args src loc)
   (append (map (lambda (a)
		   (let* ((pid (check-id (parse-id a loc) src))
			  (id (car pid))
			  (type (cdr pid)))
		      (if (user-symbol? id)
			  (make-user-local-svar id type)
			  (make-local-svar id type))))
	      (dsssl-before-dsssl args))
      (map (lambda (o)
	      (let* ((a (car o))
		     (pid (check-id (parse-id a loc) src))
		     (id (car pid))
		     (type (cdr pid)))
		 (make-user-local-svar id type)))
	 (dsssl-optionals args))))
   
;*---------------------------------------------------------------------*/
;*    make-sfun-noopt-definition ...                                   */
;*---------------------------------------------------------------------*/
(define (make-sfun-noopt-definition id module args body src class loc genv)
   (let ((locals (parse-fun-args args src loc))
	 (body (make-dsssl-function-prelude id args body user-error)))
      (list (def-global-sfun! genv id args locals module class src 'now body))))

;*---------------------------------------------------------------------*/
;*    make-svar-definition ...                                         */
;*---------------------------------------------------------------------*/
(define (make-svar-definition id src genv)
   (def-global-svar! genv id *module* src 'now)
   ;; without the Inline global variable optimization once should except
   ;; to find here `(set-car! src 'set!)'
   (set-car! (cdr src) (car (check-id (parse-id id (find-location src)) src)))
   (list src))
   
;*---------------------------------------------------------------------*/
;*    make-generic-definition ...                                      */
;*---------------------------------------------------------------------*/
(define (make-generic-definition id module args body src genv)
   (trace ast "make-generic-definition: " id " " module " " args " " body
      #\Newline)
   (let ((opts (dsssl-optionals args))
	 (keys (dsssl-keys args)))
      (cond
	 ((pair? opts)
	  (if (pair? (cdr (filter dsssl-named-constant? args)))
	      (error-sexp->node "generics can only use on DSSSL keyword" src
		 (find-location src) genv)
	      (make-generic-opt-definition opts id module args body src genv)))
	 ((pair? keys)
	  (make-generic-key-definition keys id module args body src genv))
	 (else
	  (make-generic-noopt-definition id module args body src genv)))))

;*---------------------------------------------------------------------*/
;*    make-generic-opt-definition ...                                  */
;*---------------------------------------------------------------------*/
(define (make-generic-opt-definition optionals id module args body src genv)
   (let* ((loc (find-location src))
	  (gen (make-generic-noopt-definition id module args body src genv))
	  (glo (find-global genv (fast-id-of-id id #f) module))
	  (clo (make-sfun-opt-closure glo optionals id module args body src 'sfun loc genv)))
      (cons clo gen)))

;*---------------------------------------------------------------------*/
;*    make-generic-key-definition ...                                  */
;*---------------------------------------------------------------------*/
(define (make-generic-key-definition keys id module args body src genv)
   (let* ((loc (find-location src))
	  (gen (make-generic-noopt-definition id module args body src genv))
	  (glo (find-global genv (fast-id-of-id id #f) module))
	  (clo (make-sfun-key-closure glo keys id module args body src 'sfun loc genv)))
      (cons clo gen)))

;*---------------------------------------------------------------------*/
;*    make-generic-noopt-definition ...                                */
;*---------------------------------------------------------------------*/
(define (make-generic-noopt-definition id module args body src genv)

   (define (typed-ident? id)
      (string-contains (symbol->string! id) "::"))
   
   (define (typed-args args generic)
      (with-access::variable generic (value)
	 (with-access::sfun value ((formals args))
	    (let loop ((args args)
		       (formals formals))
	       (if (not (pair? args))
		   args
		   (with-access::local (car formals) (type)
		      (cond
			 ((dsssl-named-constant? (car args))
			  args)
			 ((typed-ident? (car args))
			  (cons (car args)
			     (loop (cdr args) (cdr formals))))
			 (else
			  (cons (make-typed-ident (car args) (type-id type))
			     (loop (cdr args) (cdr formals)))))))))))
   
   (if (not (and (pair? args) (symbol? (car args))))
       (begin
	  (error-sexp->node "Bad generic formal argument" src
	     (find-location src) genv)
	  (list #unspecified))
       (let* ((loc (find-location src))
	      (locals (if (and (dsssl-prototype? args)
			       (pair? (dsssl-optionals args)))
			  (parse-fun-opt-args args src loc)
			  (parse-fun-args args src loc)))
	      (pid (check-id (parse-id id loc) src))
	      (name (gensym (car pid)))
	      (type (cdr pid))
	      (gbody (make-generic-body id locals args src))
	      (generic (def-global-sfun! genv id args locals module 'sgfun src 'now gbody))
	      ;;(def `(labels ((,name ,(typed-args args generic)
	      (def `(labels ((,name ,(if (and (dsssl-prototype? args)
					      (pair? (dsssl-optionals args)))
					 (map local-id locals)
					 (typed-args args generic))
				;; use labels instead of a plain lambda expr
				;; in order to give that default function
				;; a pleasant debug identifier
				,@(if (pair? body)
				      body
				      `(((@ error __error)
					 ',name "No method for this object"
					 ,(id-of-id (car args) (find-location src)))))))
		       ,name)))
	  (trace (ast 2) " generic " (shape id) #\newline)
	  (trace (ast 2) "       def: " (shape generic)  " " (typeof generic) #\Newline)
	  (trace (ast 2) "      body: " (shape body) #\Newline)
	  (trace (ast 2) "      args: " (shape args) #\Newline)
;* 	  (trace (ast 2) "   formals: " (shape (                       */
	  (trace (ast 2) "    locals: " (shape locals) #\Newline)
	  (mark-method! name)
	  (let* ((o-unit (get-generic-unit))
		 (type (local-type (car locals)))
		 (gen `(register-generic!
			  (@ ,(global-id generic) ,module)
			  ,(epairify-propagate-loc def loc)
			  ,(if (tclass? type) (tclass-holder type) #f)
			  ,(symbol->string name)))
		 (sexp* (list generic gen)))
	     (if (not (unit? o-unit))
		 sexp*
		 (begin
		    (unit-sexp*-add! o-unit sexp*)
		    (list #unspecified)))))))
	 	
;*---------------------------------------------------------------------*/
;*    make-method-definition ...                                       */
;*---------------------------------------------------------------------*/
(define (make-method-definition env id args body src genv)
   (if (not (and (pair? args) (symbol? (car args))))
       (begin
	  (error-sexp->node "Bad method formal argument" src
	     (find-location src) genv)
	  (list #unspecified))
       (let* ((loc (find-location src))
	      (locals (parse-fun-args args src loc)))
	  (if (not (check-method-definition env id args locals src))
	      (list #unspecified)
	      (let* ((o-unit (get-method-unit))
		     (sexp* (make-method-body id args locals body src loc)))
		 (if (not (unit? o-unit))
		     sexp*
		     (begin
			(unit-sexp*-add! o-unit sexp*)
			(list #unspecified))))))))
	     
;*---------------------------------------------------------------------*/
;*    make-method-body ...                                             */
;*---------------------------------------------------------------------*/
(define (make-method-body ident args locals body src loc)
   (cond
      ((not (dsssl-prototype? args))
       (make-method-no-dsssl-body ident args locals body src))
      ((dsssl-optional-only-prototype? args)
       (let ((locals (parse-fun-opt-args args src loc)))
	  (make-method-no-dsssl-body ident (map local-id locals) locals body src)))
      (else
       (make-method-dsssl-body ident args locals body src))))

;*---------------------------------------------------------------------*/
;*    normalize-progn/error ...                                        */
;*---------------------------------------------------------------------*/
(define (normalize-progn/error exp src loc genv)
   (if (null? exp)
       (error-sexp->node "Illegal '() expression" src (find-location src) genv)
       (let ((exp (normalize-progn exp)))
	  (cond
	     ((not loc) exp)
	     ((epair? exp) exp)
	     ((pair? exp) (econs (car exp) (cdr exp) loc))
	     (else (econs 'begin (list exp) loc))))))

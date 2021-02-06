;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Ast/unit.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 08:35:53 1996                          */
;*    Last change :  Wed Jun 20 13:23:03 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A module is composed of several unit (for instance, the user     */
;*    unit (also called the toplevel unit), the foreign unit, the      */
;*    constant unit, ...). This module takes in charge the production  */
;*    of an ast for a unit.                                            */
;*    -------------------------------------------------------------    */
;*    This module does not build node. Nodes can't be until, all       */
;*    body has been scanned and all global definitions seens. This     */
;*    is done only when all unit have been converted.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_unit
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
	    expand_eps)
   (export  (unit-sexp*-add! <unit> ::obj)
	    (unit-sexp*-add-head! <unit> ::obj)
	    (unit->defs <unit>)
	    (unit-initializers)
	    (unit-initializer-id id)
	    (unit-init-calls)))

;*---------------------------------------------------------------------*/
;*    unit-sexp*-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-sexp*-add! unit sexp)
   (if (null? (unit-sexp* unit))
       (unit-sexp*-set! unit sexp)
       (set-cdr! (last-pair (unit-sexp* unit)) sexp)))

;*---------------------------------------------------------------------*/
;*    unit-sexp*-add-head! ...                                         */
;*---------------------------------------------------------------------*/
(define (unit-sexp*-add-head! unit sexp)
   (unit-sexp*-set! unit (append sexp (unit-sexp* unit))))

;*---------------------------------------------------------------------*/
;*    unit->defs ...                                                   */
;*---------------------------------------------------------------------*/
(define (unit->defs unit)
   (verbose 2
	    "      [" (string-downcase (symbol->string (unit-id unit)))
	    "]" #\Newline)
   (when (pair? (unit-sexp* unit))
      (trace (ast 2) "  sexp*=" (map shape (unit-sexp* unit)) #\Newline))
   (let* ((id (unit-id unit))
	  (weight (unit-weight unit))
	  (sexp* (unit-sexp* unit))
	  (gdefs (find-global-defs sexp*)))
      ;; we now compute the global definitions
      (let loop ((aexp* (if (procedure? sexp*)
			    (force sexp*)
			    (toplevel*->ast sexp* gdefs)))
		 (init* (if (unit-exported? unit) (list #unspecified) '()))
		 (def*  '()))
	 (if (null? aexp*)
	     (if (pair? init*)
		 (let* ((body (if (unit-exported? unit)
				  `(if (@ ,(unit-require-init-id id) ,*module*)
				       (begin
					  (set! (@ ,(unit-require-init-id id) ,*module*) #f)
					  ,@(initialize-imported-modules
					     (lambda (m)
						(unit-initializer-id id)))
					  ,@(reverse! init*)))
				  (normalize-progn (reverse! init*))))
			(init (def-global-sfun-no-warning!
				 (make-typed-ident
				  (unit-initializer-id id) 'obj)
				 '()
				 '()
				 *module*
				 'snifun
				 *module-clause*
				 'cgen
				 body)))
		    ;; exported units must be declared export
		    (when (unit-exported? unit)
		       (global-import-set! init 'export))
		    ;; all init functions but the toplevel ones are
		    ;; compiler functions.
		    (if (not (eq? unit (get-toplevel-unit)))
			(global-user?-set! init #f))
		    ;; we declare the unit for the late module
		    ;; initialization (unit initializer are called
		    ;; in order they are declared).
		    (declare-unit! id weight)
		    ;; init functions cannot be sent to eval
		    (global-evaluable?-set! init #f)
		    ;; we are done, we return now a list of
		    ;; global function definitions.
		    (when (unit-exported? unit)
		       (let* ((id (make-typed-ident (unit-require-init-id id)
						    'obj))
			      (glo (def-global-svar! id *module*
				      *module-clause* 'now)))
			  (global-user?-set! glo #f)
			  (global-evaluable?-set! glo #f)))
		    (cons init (reverse! def*)))
		 (reverse! def*))
	     (if (global? (car aexp*))
		 (loop (cdr aexp*)
		       init*
		       (cons (car aexp*) def*))
		 (loop (cdr aexp*)
		       (cons (car aexp*) init*)
		       def*))))))
 
;*---------------------------------------------------------------------*/
;*    *unit-list* ...                                                  */
;*---------------------------------------------------------------------*/
(define *unit-list* '())

;*---------------------------------------------------------------------*/
;*    declare-unit! ...                                                */
;*---------------------------------------------------------------------*/
(define (declare-unit! id::symbol weight::long)
   (if (or (null? *unit-list*) (<fx weight (cdr (car *unit-list*))))
       (set! *unit-list* (cons (cons id weight) *unit-list*))
       (let loop ((ulist *unit-list*))
	  (cond
	     ((<fx weight (cdr (car ulist)))
	      (set-cdr! ulist (cons (car ulist) (cdr ulist)))
	      (set-car! ulist (cons id weight)))
	     ((null? (cdr ulist))
	      (set-cdr! ulist (list (cons id weight))))
	     (else
	      (loop (cdr ulist)))))))

;*---------------------------------------------------------------------*/
;*    unit-initializer-id ...                                          */
;*---------------------------------------------------------------------*/
(define (unit-initializer-id id)
   (symbol-append id '-init))

;*---------------------------------------------------------------------*/
;*    unit-require-init-id ...                                         */
;*---------------------------------------------------------------------*/
(define (unit-require-init-id id)
   (symbol-append id '-require-initialization))

;*---------------------------------------------------------------------*/
;*    unit-initializers ...                                            */
;*---------------------------------------------------------------------*/
(define (unit-initializers)
   (map (lambda (unit)
	   (find-global/module (unit-initializer-id (car unit)) *module*))
	*unit-list*))

;*---------------------------------------------------------------------*/
;*    unit-init-calls ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-init-calls)
   (map (lambda (unit) `((@ ,(unit-initializer-id (car unit)) ,*module*)))
	*unit-list*))

;*---------------------------------------------------------------------*/
;*    toplevel*->ast ...                                               */
;*---------------------------------------------------------------------*/
(define (toplevel*->ast::pair-nil sexp*::pair-nil gdefs)
   (let loop ((sexp* sexp*)
	      (aexp* '()))
      (if (null? sexp*)
	  (reverse! aexp*)
	  (loop (cdr sexp*)
		(append (toplevel->ast (car sexp*) gdefs) aexp*)))))

;*---------------------------------------------------------------------*/
;*    lambda? ...                                                      */
;*---------------------------------------------------------------------*/
(define (lambda? sym)
   (and (symbol? sym) (eq? (fast-id-of-id sym #f) 'lambda)))
	
;*---------------------------------------------------------------------*/
;*    toplevel->ast ...                                                */
;*    -------------------------------------------------------------    */
;*    !!! WARNING: this function must build a reversed list !!!        */
;*---------------------------------------------------------------------*/
(define (toplevel->ast sexp gdefs)
   (match-case sexp
      ((begin)
       (list sexp))
      ((begin . ?nsexp*)
       (reverse! (toplevel*->ast nsexp* gdefs)))
      ((define (?var . ?args) . ?exp)
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module id *module*)))
	  ;; exported variable are set to be written hence, we don't
	  ;; have to check here if the variable has been declared has
	  ;; exported (as a variable vs a function). We just have to
	  ;; check if it is written.
	  ;; We may not find global in def because it can has been
	  ;; introduced after the computation of gdefs (for instance
	  ;; if it is a default method of a generic).
	  (if (or (not (pair? def))
		  (and (eq? (car (cdr def)) 'read)
		       (or (not (global? global))
			   (eq? (global-access global) 'read))))
	      (make-sfun-definition var
		 *module* args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)))
		 sexp 'sfun)
	      (let ((new-sexp `(set! ,var (lambda ,args ,@exp))))
		 (replace! sexp new-sexp)
		 (make-svar-definition var sexp)))))
      ((define ?var (lambda ?args . ?exp))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module id *module*)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read)))
	      (make-sfun-definition var
		 *module* args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)))
		 sexp 'sfun)
	      (make-svar-definition var sexp))))
      ((define ?var (labels ((?f ?args . ?exp)) ?f))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module id *module*)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read))
		   (eq? id f))
	      (make-sfun-definition var
		 *module* args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)))
		 sexp 'sfun)
	      (make-svar-definition var sexp))))
      ((define ?var ((and ?lam (? lambda?)) ?args . ?exp))
       (let* ((id (id-of-id var (find-location sexp)))
	      (def (assq id gdefs))
	      (global (find-global/module id *module*))
	      (tlam (type-of-id lam #f)))
	  ;; same remark as in the previous match (variables vs functions)
	  (if (and (eq? (car (cdr def)) 'read)
		   (or (not (global? global))
		       (eq? (global-access global) 'read)))
	      (make-sfun-definition (make-typed-ident id (type-id tlam))
		 *module* args
		 (normalize-progn/error exp sexp (find-location (cddr sexp)))
		 sexp 'sfun)
	      (make-svar-definition var sexp))))
      ((define ?var (begin ?1-exp))
       (set-car! (cddr sexp) 1-exp)
       (toplevel->ast sexp gdefs))
      ((define ?var (and (? symbol?) ?var2))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let* ((g (find-global var2))
		     (arity (and (global? g)
				 (fun? (global-value g))
				 (fun-arity (global-value g)))))
		 (if (fixnum? arity)
		     (cond
			((global-optional? g)
			 (user-warning var
			    "Unable to eta-expand #!optional alias"
			    sexp)
			 (make-svar-definition var sexp))
			((global-key? g)
			 (user-warning var
			    "Unable to eta-expand #!key alias"
			    sexp)
			 (make-svar-definition var sexp))
			(else
			 (let ((def (eta-expanse sexp arity)))
			    (toplevel->ast def gdefs))))
		     (make-svar-definition var sexp)))
	      (make-svar-definition var sexp))))
      ((define ?var (@ (and (? symbol?) ?var2) (and (? symbol?) ?module)))
       (let ((def (assq (id-of-id var (find-location sexp)) gdefs)))
	  (if (eq? (car (cdr def)) 'read)
	      (let* ((g (find-global/module var2 module))
		     (arity (and (global? g)
				 (fun? (global-value g))
				 (global-arity g))))
		 (if (fixnum? arity)
		     (cond
			((global-optional? g)
			 (user-warning var
			    "Unable to eta-expand #!optional alias"
			    sexp)
			 (make-svar-definition var sexp))
			((global-key? g)
			 (user-warning var
			    "Unable to eta-expand #!key alias"
			    sexp)
			 (make-svar-definition var sexp))
			(else
			 (let ((def (eta-expanse sexp arity)))
			    (toplevel->ast def gdefs))))
		     (make-svar-definition var sexp)))
	      (make-svar-definition var sexp))))
      ((define ?var . ?exp)
       (make-svar-definition var sexp))
      ((define-inline ((@ ?var ?module) . ?args) . ?exp)
       (make-sfun-definition var
	  module args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)))
	  sexp 'sifun))
      ((define-inline (?var . ?args) . ?exp)
       (make-sfun-definition var
	  *module* args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)))
	  sexp 'sifun))
      ((define-generic ((@ ?var ?module) . ?args) . ?exp)
       (warning "define-generic" "form no longer supported" sexp)
       (make-generic-definition var module args exp sexp))
      ((define-generic (?var . ?args) . ?exp)
       (make-generic-definition var *module* args exp sexp))
      ((define-method (?var . ?args) . ?exp)
       (make-method-definition var
	  args
	  (normalize-progn/error exp sexp (find-location (cddr sexp)))
	  sexp))
      (else
       (list sexp))))

;*---------------------------------------------------------------------*/
;*    normalize-progn/error ...                                        */
;*---------------------------------------------------------------------*/
(define (normalize-progn/error exp src loc)
   (if (null? exp)
       (error-sexp->node "Illegal '() expression" src (find-location src))
       (let ((exp (normalize-progn exp)))
	  (cond
	     ((not loc)
	      exp)
	     ((epair? exp)
	      exp)
	     ((pair? exp)
	      (econs (car exp) (cdr exp) loc))
	     (else
	      (econs 'begin (list exp) loc))))))

;*---------------------------------------------------------------------*/
;*    get-global-arity ...                                             */
;*---------------------------------------------------------------------*/
(define (get-global-arity id module gdefs)
   (let ((global (if (symbol? module)
		     (find-global/module id module)
		     (find-global id))))
      (if (not (global? global))
	  #f
	  (if (fun? (global-value global))
	      (fun-arity (global-value global))
	      #f))))

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
(define (make-sfun-definition id::symbol module::symbol args body src class)
   (let ((loc (find-location src))
	 (opts (dsssl-optionals args))
	 (keys (dsssl-keys args)))
      (cond
	 ((pair? opts)
	  (make-sfun-opt-definition opts id module args body src class loc))
	 ((pair? keys)
	  (make-sfun-key-definition keys id module args body src class loc))
	 (else
	  (make-sfun-noopt-definition id module args body src class loc)))))

;*---------------------------------------------------------------------*/
;*    make-sfun-opt-definition ...                                     */
;*---------------------------------------------------------------------*/
(define (make-sfun-opt-definition optionals id module args body src class loc)
   (let* ((locals (parse-fun-opt-args args args loc))
	  (glo (def-global-sfun! id args locals module class src 'now body))
	  (clo (make-sfun-opt-closure glo optionals id module args body src class loc)))
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
(define (make-sfun-opt-closure glo optionals id module args body src class loc)
   (let ((arity (sfun-arity (global-value glo))))
      (define (funcall opt)
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
      (let* ((id (symbol-append '_ id))
	     (optid (gensym 'opt))
	     (envid (gensym 'env))
	     (opt (make-local-svar optid *vector*))
	     (env (make-local-svar envid *procedure*))
	     (g (def-global-sfun! id (list envid optid)
		   (list env opt) module class
		   src 'globalization
		   (compile-expand (comptime-expand (funcall optid))))))
	 (global-evaluable?-set! g #f)
	 (global-type-set! g *obj*)
	 g)))

;*---------------------------------------------------------------------*/
;*    make-sfun-key-definition ...                                     */
;*---------------------------------------------------------------------*/
(define (make-sfun-key-definition keys id module args body src class loc)
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
	  (glo (def-global-sfun! id args locals module class src 'now body))
	  (clo (make-sfun-key-closure glo keys id module args body src class loc)))
      (list glo clo)))

;*---------------------------------------------------------------------*/
;*    make-sfun-key-closure ...                                        */
;*    -------------------------------------------------------------    */
;*    See make-sfun-opt-closure.                                       */
;*---------------------------------------------------------------------*/
(define (make-sfun-key-closure glo keys id module args body src class loc)
   (let ((arity (sfun-arity (global-value glo)))
	 (iopt (gensym 'opt))
	 (ienv (gensym 'env))
	 (lopt (length keys))
	 (l (gensym 'l))
	 (search (gensym 'search))
	 (check (gensym 'check))
	 (var (gensym 'var)))
      (define (all-keys keys)
	 ;; compute the whole keywords set from the declaration
	 (map (lambda (k)
		 (symbol->keyword (fast-id-of-id (car k) loc)))
	      keys))
      (define (funcall)
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
      (let* ((id (symbol-append '_ id))
	     (opt (make-local-svar iopt *vector*))
	     (env (make-local-svar ienv *procedure*))
	     (g (def-global-sfun! id (list ienv iopt) (list env opt) module class
		   src 'globalization
		   (compile-expand
		    (comptime-expand (funcall))))))
	 (global-type-set! g *obj*)
	 (global-evaluable?-set! g #f)
	 g)))

;*---------------------------------------------------------------------*/
;*    parse-fun-args ...                                               */
;*---------------------------------------------------------------------*/
(define (parse-fun-args args src loc)
   (let loop ((args  args)
	      (res   '()))
      (cond
	 ((null? args)
	  (reverse! res))
	 ((not (pair? args)) 
	  (let* ((pid  (check-id (parse-id args loc) src))
		 (id   (car pid))
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
	  (let* ((pid  (check-id (parse-id (car args) loc) src))
		 (id   (car pid))
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
(define (make-sfun-noopt-definition id module args body src class loc)
   (let ((locals (parse-fun-args args src loc))
	 (body (make-dsssl-function-prelude id args body user-error)))
      (list (def-global-sfun! id args locals module class src 'now body))))

;*---------------------------------------------------------------------*/
;*    make-svar-definition ...                                         */
;*---------------------------------------------------------------------*/
(define (make-svar-definition id src)
   (def-global-svar! id *module* src 'now)
   ;; without the Inline global variable optimization once should except
   ;; to find here `(set-car! src 'set!)'
   (set-car! (cdr src) (car (check-id (parse-id id (find-location src)) src)))
   (list src))
   
;*---------------------------------------------------------------------*/
;*    make-sgfun-default ...                                           */
;*---------------------------------------------------------------------*/
(define (make-sgfun-default name type args body src gdefs)
   (trace ast "make-sgfun-default: " name " " args " " body #\Newline)
   (let* ((default-id   name)
	  (default-tid  (if (eq? type *_*)
			    default-id
			    (make-typed-ident default-id (type-id type))))
	  (default-body (if (pair? body)
			    (normalize-progn body)
			    `((@ error __error)
			      ',name
			      "No method for this object"
			      ,(id-of-id (car args)
					 (find-location src)))))
	  (form (let ((tmp `(define ,(cons default-tid args) ,default-body)))
		   (if (epair? *module-clause*)
		       (econs (car tmp) (cdr tmp) (cer *module-clause*))
		       tmp))))
      (trace (ast 2) "  le body: " default-body #\Newline)
      (let ((ast (toplevel->ast form gdefs)))
	 (trace (ast 2) "  l'ast: " ast #\Newline)
	 (when (and (pair? ast) (global? (car ast)))
	    ;; we mark the function as a user one so the default
	    ;; function will appears as is in the profiler or the
	    ;; debugger.
	    (global-user?-set! (car ast) #t))
	 ast)))

;*---------------------------------------------------------------------*/
;*    make-generic-definition ...                                      */
;*---------------------------------------------------------------------*/
(define (make-generic-definition id module args body src)
   (trace ast "make-generic-definition: " id " " module " " args " " body
      #\Newline)
   (let ((opts (dsssl-optionals args))
	 (keys (dsssl-keys args)))
      (cond
	 ((pair? opts)
	  (if (pair? (cdr (filter dsssl-named-constant? args)))
	      (error-sexp->node "generics can only use on DSSSL keyword" src
		 (find-location src))
	      (make-generic-opt-definition opts id module args body src)))
	 ((pair? keys)
	  (make-generic-key-definition keys id module args body src))
	 (else
	  (make-generic-noopt-definition id module args body src)))))

;*---------------------------------------------------------------------*/
;*    make-generic-opt-definition ...                                  */
;*---------------------------------------------------------------------*/
(define (make-generic-opt-definition optionals id module args body src)
   (let* ((loc (find-location src))
	  (gen (make-generic-noopt-definition id module args body src))
	  (glo (find-global (fast-id-of-id id #f) module))
	  (clo (make-sfun-opt-closure glo optionals id module args body src 'sfun loc)))
      (cons clo gen)))

;*---------------------------------------------------------------------*/
;*    make-generic-key-definition ...                                  */
;*---------------------------------------------------------------------*/
(define (make-generic-key-definition keys id module args body src)
   (let* ((loc (find-location src))
	  (gen (make-generic-noopt-definition id module args body src))
	  (glo (find-global (fast-id-of-id id #f) module))
	  (clo (make-sfun-key-closure glo keys id module args body src 'sfun loc)))
      (cons clo gen)))

;*---------------------------------------------------------------------*/
;*    make-generic-noopt-definition ...                                */
;*---------------------------------------------------------------------*/
(define (make-generic-noopt-definition id module args body src)

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
	     (find-location src))
	  (list #unspecified))
       (let* ((loc (find-location src))
	      (locals (if (and (dsssl-prototype? args)
			       (pair? (dsssl-optionals args)))
			  (parse-fun-opt-args args src loc)
			  (parse-fun-args args src loc)))
	      (pid (check-id (parse-id id loc) src))
	      (name (gensym (car pid)))
	      (type (cdr pid))
	      (gbody   (make-generic-body id locals args src))
	      (generic (def-global-sfun! id args locals module 'sgfun src 'now gbody))
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
(define (make-method-definition id args body src)
   (if (not (and (pair? args) (symbol? (car args))))
       (begin
	  (error-sexp->node "Bad method formal argument" src
	     (find-location src))
	  (list #unspecified))
       (let* ((loc (find-location src))
	      (locals (parse-fun-args args src loc)))
	  (if (not (check-method-definition id args locals src))
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

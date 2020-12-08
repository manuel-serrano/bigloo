;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/node.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 14 17:30:55 1995                          */
;*    Last change :  Fri Apr 21 18:44:42 2017 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of K and K* properties.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_node
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_typeof
	    type_cache
	    ast_var
	    ast_node
	    ast_local
	    integrate_info
	    integrate_local->global)
   (export  (integrate-globalize! ::node ::variable <what/by>*)
	    (integrate-celled?::bool ::local)))
   
;*---------------------------------------------------------------------*/
;*    integrate-globalize! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function makes many transformation on the Ast *and*         */
;*    returns a free variables list.                                   */
;*---------------------------------------------------------------------*/
(define (integrate-globalize! ast integrator what/by*)
   (trace integrate "globalize!: " (shape integrator) " " (shape what/by*)
	  #\Newline "  " (shape ast)
	  #\Newline)
   ;; for each celled variable, we declare a new local
   ;; variable
   (let* ((fun (variable-value integrator))
	  (celled (celled-bindings (sfun-args fun)))
	  (what/by* (append celled what/by*)))
      ;; we set alpha-fast slot 
      (for-each (lambda (w-b)
		   (local-fast-alpha-set! (car w-b) (cdr w-b)))
		what/by*)
      (let ((res (cell-formals celled (glo! ast integrator))))
	 ;; we remove alpha-fast slots
	 (for-each (lambda (w-b)
		      (local-fast-alpha-set! (car w-b) #unspecified))
		   what/by*)
	 res)))

;*---------------------------------------------------------------------*/
;*    celled-bindings ...                                              */
;*---------------------------------------------------------------------*/
(define (celled-bindings formals)
   (let loop ((celled   '())
	      (formals  formals))
      (cond
	 ((null? formals)
	  celled)
	 ((not (integrate-celled? (car formals)))
	  (loop celled (cdr formals)))
	 (else
	  (let* ((ovar (car formals))
		 (nvar (cell-variable ovar)))
	     (loop (cons (cons ovar nvar) celled) (cdr formals)))))))

;*---------------------------------------------------------------------*/
;*    cell-variable ...                                                */
;*---------------------------------------------------------------------*/
(define (cell-variable::local local::local)
   (let ((var (make-local-svar (local-id local) *cell*)))
      (local-access-set! var 'cell-integrate)
      (widen!::svar/Iinfo (local-value var)
	 (celled? #t)
	 (kaptured? #t))
      var))
   
;*---------------------------------------------------------------------*/
;*    cell-formals ...                                                 */
;*---------------------------------------------------------------------*/
(define (cell-formals celled body)
   (if (null? celled)
       body
       (let ((loc (node-loc body)))
	  (instantiate::let-var
	     (loc loc)
	     (body body)
	     (type (node-type body))
	     (bindings (map (lambda (o-n)
			       (cons (cdr o-n)
				  (a-make-cell (instantiate::var
						  (type (variable-type (car o-n)))
						  (loc loc)
						  (variable (car o-n)))
				     (car o-n))))
			  celled))))))

;*---------------------------------------------------------------------*/
;*    a-make-cell ...                                                  */
;*---------------------------------------------------------------------*/
(define (a-make-cell::make-box node::node variable::variable)
   (trace (integrate 5) "*** a-make-cell: " (shape node) " " (shape variable)
	  #\Newline)
   (with-access::node node (loc)
      (local-access-set! variable 'cell-integrate)
      (svar/Iinfo-celled?-set! (variable-value variable) #t)
      (instantiate::make-box
	 (type *cell*)
	 (vtype (get-bigloo-type (variable-type variable)))
	 (loc loc)
	 (value node)
	 (stackable #t))))
    
;*---------------------------------------------------------------------*/
;*    integrate-celled? ...                                            */
;*---------------------------------------------------------------------*/
(define (integrate-celled?::bool variable::local)
   (trace (integrate 5) "*** celled?: "
	  (shape variable) " Iinfo?:"
	  (svar/Iinfo? (variable-value variable)) " Iinfo-celled?:"
	  (if (svar/Iinfo? (variable-value variable))
	      (svar/Iinfo-celled? (variable-value variable)) " not Iinfo")
	  " "
	  (variable-access variable) " kaptured?:"
	  (if (svar/Iinfo? (variable-value variable))
	      (svar/Iinfo-kaptured? (variable-value variable)) " Not Iinfo")
	  #\Newline) 
   (and (svar/Iinfo? (variable-value variable))
	(or (svar/Iinfo-celled? (variable-value variable))
	    (and (memq (variable-access variable) '(write cell-integrate))
		 (svar/Iinfo-kaptured? (variable-value variable))))))

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (glo!::node node::node integrator::variable))

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::atom integrator)
   node)
 
;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::kwote integrator)
   node)

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::var integrator)
   (let* ((variable (var-variable node))
	  (vtype (variable-type variable)))
      (let loop ((variable variable))
	 (let ((alpha (variable-fast-alpha variable)))
	    (cond
	       ((local? alpha)
		(var-variable-set! node alpha)
		(node-type-set! node (variable-type alpha))
		(loop alpha))
	       ((global? variable)
		node)
	       ((integrate-celled? variable)
		;(local-access-set! variable 'cell-integrate)
		(let ((vtype (get-bigloo-type vtype)))
		   ;; (node-type-set! node (get-bigloo-type vtype))
		   (node-type-set! node *cell*)
		   (instantiate::box-ref
		      (loc (node-loc node))
		      ;;(type (get-bigloo-type (node-type node)))
		      (type vtype)
		      (vtype vtype)
		      (var node))))
	       (else
		node))))))

;*---------------------------------------------------------------------*/
;*    glo! ::closure ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::closure integrator)
   (internal-error "node-free" "Unexepected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    glo! ::sequence ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::sequence integrator)
   (with-access::sequence node (nodes)
      (glo*! nodes integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::sync ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::sync integrator)
   (with-access::sync node (body mutex prelock)
      (set! mutex (glo! mutex integrator))
      (set! prelock (glo! prelock integrator))
      (set! body (glo! body integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (glo! node::app integrator)
   (with-access::app node (loc args type)
      (let* ((fun  (var-variable (app-fun node)))
	     (info (variable-value fun)))
	 ;; we change the called function if globalized
	 (when (and (local? fun) (sfun/Iinfo-G? info))
	    (app-fun-set! node
			  (instantiate::var
			     (loc loc)
			     (type (variable-type (the-global fun)))
			     (variable (the-global fun)))))
	 ;; we globalize the actuals before adding new ones
	 ;; otherwise, we could produce illegal `cell-ref'
	 (let liip ((nodes args))
	    (if (null? nodes)
		'done
		(begin
		   (set-car! nodes (glo! (car nodes) integrator))
		   (liip (cdr nodes)))))
	 (unless (or (global? fun) (not (sfun/Iinfo-G? info)))
	    ;; this is a call to globalized but non escaping
	    ;; function. We add its kaptured variables
	    (let loop ((new-actuals args)
		       (kaptured (sfun/Iinfo-kaptured info)))
	       (if (null? kaptured)
		   (set! args new-actuals)
		   (let* ((kap   (car kaptured))
			  (alpha (local-fast-alpha kap))
			  (var   (if (local? alpha) alpha kap)))
		      (loop (cons (instantiate::var
				     (loc loc)
				     (type (variable-type var))
				     (variable var))
				  new-actuals)
			    (cdr kaptured))))))
	 node)))
	  
;*---------------------------------------------------------------------*/
;*    glo! ::app-ly ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::app-ly integrator)
   (with-access::app-ly node (fun arg)
      (set! fun (glo! fun integrator))
      (set! arg (glo! arg integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::funcall ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::funcall integrator)
   (with-access::funcall node (fun args)
      (set! fun (glo! fun integrator))
      (glo*! args integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::extern ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::extern integrator)
   (with-access::extern node (expr*)
      (glo*! expr* integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::cast ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::cast integrator)
   (with-access::cast node (arg)
      (glo! arg integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::setq ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::setq integrator)
   (with-access::setq node (value)
      (set! value (glo! value integrator))
      (let* ((var (var-variable (setq-var node)))
	     (vtype (variable-type var)))
	 (let loop ((var   var)
		    (alpha (variable-fast-alpha var)))
	    (if (local? alpha)
		(begin
		   (var-variable-set! (setq-var node) alpha)
		   (loop alpha (variable-fast-alpha alpha)))
		(let ((var (var-variable (setq-var node))))
		   (if (and (local? var) (integrate-celled? var))
		       (let ((a-var (make-local-svar 'aux *obj*))
			     (loc   (node-loc node)))
			  (local-access-set! var 'cell-integrate)
			  (node-type-set! (setq-var node) *obj*)
			  (widen!::svar/Iinfo (local-value a-var)
			     (kaptured? #f))
			  (instantiate::let-var
			     (loc loc)
			     (type *unspec*)
			     (bindings (list (cons a-var value)))
			     (body (instantiate::box-set!
				      (loc loc)
				      (type *unspec*)
				      (var (setq-var node))
				      (vtype (get-bigloo-type vtype))
				      (value (instantiate::var
						(loc loc)
						(type (variable-type a-var))
						(variable a-var)))))))
		       node)))))))

;*---------------------------------------------------------------------*/
;*    glo! ::conditional ...                                           */
;*---------------------------------------------------------------------*/
(define-method (glo! node::conditional integrator)
   (with-access::conditional node (test true false)
      (set! test (glo! test integrator))
      (set! true (glo! true integrator))
      (set! false (glo! false integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::fail ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::fail integrator)
   (with-access::fail node (proc msg obj)
      (set! proc (glo! proc integrator))
      (set! msg (glo! msg integrator))
      (set! obj (glo! obj integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::switch ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::switch integrator)
   (with-access::switch node (clauses test)
      (set! test (glo! test integrator))
      (for-each (lambda (clause)
		   (set-cdr! clause (glo! (cdr clause) integrator)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::let-fun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::let-fun integrator)
   (with-access::let-fun node (body locals)
      (set! body (glo! body integrator))
      (for-each (lambda (local)
		   (globalize-local-fun! local integrator))
		locals)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::let-var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::let-var integrator)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (set-cdr! binding (glo! val integrator))
		      (when (integrate-celled? var)
			 (let ((nvar (cell-variable var)))
			    (local-fast-alpha-set! var nvar)
			    (set-cdr! binding
			       (a-make-cell (cdr binding) var))))))
	 bindings)
      (set! body (glo! body integrator))
      (for-each (lambda (binding)
		   (let ((var (car binding)))
		      (when (integrate-celled? var)
			 (let ((nvar (cell-variable var)))
			    (set-car! binding (variable-fast-alpha var))
			    (local-fast-alpha-set! var #unspecified)))))
	 bindings)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::set-ex-it ...                                             */
;*---------------------------------------------------------------------*/
(define-method (glo! node::set-ex-it integrator)
   (with-access::set-ex-it node (var body)
      (set! body (glo! body integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::jump-ex-it ...                                            */
;*---------------------------------------------------------------------*/
(define-method (glo! node::jump-ex-it integrator)
   (with-access::jump-ex-it node (exit value)
      (set! exit (glo! exit integrator))
      (set! value (glo! value integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::make-box ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::make-box integrator)
   (with-access::make-box node (value)
      (set! value (glo! value integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::box-ref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::box-ref integrator)
   (trace (integrate 3) "box-ref: " (shape node) #\Newline)
   (with-access::box-ref node (var)
      (when (integrate-celled? (var-variable var))
	 (let ((v (var-variable var)))
	    (tprint "box-ref=" (shape node))
	    (tprint "access=" (variable-access v))
	    (error "integration" "box-ref" (shape node))))
      (set! var (glo! var integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::box-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::box-set! integrator)
   (trace (integrate 3) "box-set!: " (shape node) #\Newline)
   (with-access::box-set! node (var value)
      (set! var (glo! var integrator))
      (set! value (glo! value integrator))
      node))
	    
;*---------------------------------------------------------------------*/
;*    glo*! ...                                                        */
;*---------------------------------------------------------------------*/
(define (glo*! node* integrator)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (glo! (car node*) integrator))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    globalize-local-fun! ...                                         */
;*---------------------------------------------------------------------*/
(define (globalize-local-fun! local::local integrator::variable)
   (trace (integrate 2) "globalize-local-fun!: local: " (shape local)
	  " integrator: " (shape integrator) #\Newline)
   (let* ((fun   (local-value local))
	  (obody (sfun-body fun)))
      (cond
	 ((eq? local integrator)
	  (sfun-body-set! fun (glo! obody integrator)))
	 ((and (sfun/Iinfo? fun) (sfun/Iinfo-G? fun))
	  #unspecified)
	 (else
	  (let ((celled (celled-bindings (sfun-args fun))))
	     (for-each (lambda (w-b)
			  (variable-fast-alpha-set! (car w-b) (cdr w-b)))
		       celled)
	     (let* ((nbody1 (glo! obody integrator))
		    (nbody2 (cell-formals celled nbody1)))
		(for-each (lambda (w-b)
			     (variable-fast-alpha-set! (car w-b) #unspecified))
			  celled)
		(sfun-body-set! fun nbody2)))))))


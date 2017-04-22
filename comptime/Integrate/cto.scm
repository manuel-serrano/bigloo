;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/cto.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Apr 25 15:52:39 1995                          */
;*    Last change :  Fri Apr 21 18:48:28 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of the `cto' property.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_cto
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    ast_var
	    ast_node
	    integrate_info)
   (export  (generic set-cto! ::node ::local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (set-cto! node::node local::local))

;*---------------------------------------------------------------------*/
;*    set-cto! ::atom ...                                              */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::atom local)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    set-cto! ::kwote ...                                             */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::kwote local)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    set-cto! ::var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::var local)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    set-cto! ::closure ...                                           */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::closure local)
   (internal-error "set-cto!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::sequence ...                                          */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::sequence local)
   (with-access::sequence node (nodes)
      (for-each (lambda (node) (set-cto! node local)) nodes)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::sync ...                                              */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::sync local)
   (with-access::sync node (body mutex prelock)
      (set-cto! mutex local)
      (set-cto! prelock local)
      (set-cto! body local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::app ...                                               */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::app local)
   (with-access::app node (args)
      ;; if the called function is a globalized local one, and
      ;; is not local, we add it to the cto list.
      (let ((fun (var-variable (app-fun node))))
	 (if (and (local? fun) (not (eq? local fun)))
	     (trace (integrate 4) ">>> set-cto!(" (shape local) "): "
		    (shape node) #\Newline
		    "         "
		    "  Iinfo?: " (sfun/Iinfo-G? (local-value fun)) " "
		    "  memq: " (memq fun (sfun/Iinfo-cto (local-value local)))
		    #\Newline))
	 (if (and (local? fun)
		  (sfun/Iinfo-G? (local-value fun))
		  (not (eq? local fun))
		  (not (memq fun (sfun/Iinfo-cto (local-value local)))))
	     (sfun/Iinfo-cto-set! (local-value local)
				  (cons fun
					(sfun/Iinfo-cto
					 (local-value local))))))
      (let liip ((asts args))
	 (if (null? asts)
	     #unspecified
	     (begin
		(set-cto! (car asts) local)
		(liip (cdr asts)))))))

;*---------------------------------------------------------------------*/
;*    set-cto! ::app-ly ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::app-ly local)
   (with-access::app-ly node (fun arg)
      (set-cto! fun local)
      (set-cto! arg local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::funcall ...                                           */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::funcall local)
   (with-access::funcall node (fun args)
      (let liip ((asts args))
	 (if (null? asts)
	     (set-cto! fun local)
	     (begin
		(set-cto! (car asts) local)
		(liip (cdr asts)))))))

;*---------------------------------------------------------------------*/
;*    set-cto! ::extern ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::extern local)
   (with-access::extern node (expr*)
      (for-each (lambda (node) (set-cto! node local)) expr*)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::cast ...                                              */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::cast local)
   (with-access::cast node (arg)
      (set-cto! arg local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::setq ...                                              */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::setq local)
   (with-access::setq node (var value)
      (set-cto! var local)
      (set-cto! value local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::conditional ...                                       */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::conditional local)
   (with-access::conditional node (test true false)
      (set-cto! test local)
      (set-cto! true local)
      (set-cto! false local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::fail ...                                              */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::fail local)
   (with-access::fail node (proc msg obj)
      (set-cto! proc local)
      (set-cto! msg local)
      (set-cto! obj local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::switch ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::switch local)
   (with-access::switch node (test)
      (let liip ((clauses (switch-clauses node)))
	 (if (null? clauses)
	     (set-cto! test local)
	     (begin
		(set-cto! (cdr (car clauses)) local)
		(liip (cdr clauses)))))))

;*---------------------------------------------------------------------*/
;*    set-cto! ::let-fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::let-fun local)
   (with-access::let-fun node (body)
      (let liip ((locals (let-fun-locals node)))
	 (if (null? locals)
	     (set-cto! body local)
	     (let* ((llocal (car locals))
		    (fun    (local-value llocal))
		    (info   (local-value llocal)))
		(begin
		   (set-cto! (sfun-body fun) local)
		   (liip (cdr locals))))))))

;*---------------------------------------------------------------------*/
;*    set-cto! ::let-var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::let-var local)
   (with-access::let-var node (body)
      (let liip ((bindings  (let-var-bindings node)))
	 (if (null? bindings)
	     (set-cto! body local)
	     (begin
		(set-cto! (cdr (car bindings)) local)
		(liip (cdr bindings)))))))

;*---------------------------------------------------------------------*/
;*    set-cto! ::set-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::set-ex-it local)
   (with-access::set-ex-it node (var body)
      (set-cto! body local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::jump-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::jump-ex-it local)
   (with-access::jump-ex-it node (exit value)
      (set-cto! exit local)
      (set-cto! value local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::make-box ...                                          */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::make-box local)
   (with-access::make-box node (value)
      (set-cto! value local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::box-set! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::box-set! local)
   (with-access::box-set! node (var value)
      (set-cto! var local)
      (set-cto! value local)))

;*---------------------------------------------------------------------*/
;*    set-cto! ::box-ref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (set-cto! node::box-ref local)
   (with-access::box-ref node (var)
      (set-cto! var local)))

		
	    



   



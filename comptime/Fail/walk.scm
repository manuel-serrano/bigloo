;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Fail/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 13 13:53:58 1995                          */
;*    Last change :  Fri Nov  5 13:26:10 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The introduction of fail in debugging mode.                      */
;*    -------------------------------------------------------------    */
;*    This stage replaces FAIL node and calls to ERROR with            */
;*    ERROR/LOCATION calls.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module fail_walk
   (include "Engine/pass.sch"
	    "Tools/location.sch"
	    "Ast/node.sch")
   (import  tools_shape
	    tools_error
	    tools_location
	    ast_sexp
	    ast_env
	    ast_env)
   (export  (fail-walk! tree)))

;*---------------------------------------------------------------------*/
;*    *error* ...                                                      */
;*---------------------------------------------------------------------*/
(define *error* #unspecified)

;*---------------------------------------------------------------------*/
;*    fail-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (fail-walk! globals)
   (pass-prelude "Fail"
		 (lambda ()
		    (set! *error* (find-global 'error '__error))))
   (for-each fail-fun! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    fail-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (fail-fun! var)
   (let* ((fun  (variable-value var))
	  (body (sfun-body fun))
	  (type (variable-type var)))
      (enter-function (variable-id var))
      (sfun-body-set! fun (fail-node! body))
      (leave-function)))
   
;*---------------------------------------------------------------------*/
;*    fail-node! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (fail-node!::node node::node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::sequence)
   (fail-node*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::app)
   (with-access::app node (fun args loc)
      (fail-node*! args)
      (if (and (location? loc)
	       (eq? (var-variable fun) *error*))
	  (sexp->node `(error/location ,(car args)
				       ,(cadr args)
				       ,(caddr args)
				       ,(location-full-fname loc)
				       ,(location-pos loc))
		      '()
		      loc
		      'value)
	  node)))
 
;*---------------------------------------------------------------------*/
;*    fail-node! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (fail-node! fun))
      (set! arg (fail-node! arg))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (fail-node! fun))
      (fail-node*! args)
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::extern)
   (fail-node*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::setq)
   (setq-value-set! node (fail-node! (setq-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (fail-node! test))
       (set! true (fail-node! true))
       (set! false (fail-node! false))
       node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::fail)
   (with-access::fail node (loc proc msg obj)
      (set! proc (fail-node! proc))
      (set! msg (fail-node! msg))
      (set! obj (fail-node! obj))
      (if (location? loc)
	  (sexp->node `(error/location ,proc
				       ,msg
				       ,obj
				       ,(location-full-fname loc)
				       ,(location-pos loc))
		      '()
		      loc
		      'value)
	  node)))

;*---------------------------------------------------------------------*/
;*    fail-node! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::select)
   (with-access::select node (clauses test)
      (set! test (fail-node! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (fail-node! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each fail-fun! locals)
      (set! body (fail-node! body))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (fail-node! (cdr binding))))
		bindings)
      (set! body (fail-node! body))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::cast)
   (with-access::cast node (arg)
      (set! arg (fail-node! arg))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::set-ex-it)
   (set-ex-it-body-set! node (fail-node! (set-ex-it-body node)))
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (fail-node! exit)) 
      (set! value (fail-node! value))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::make-box)
   (make-box-value-set! node (fail-node! (make-box-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::box-ref)
   node)

;*---------------------------------------------------------------------*/
;*    fail-node! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (fail-node! node::box-set!)
   (with-access::box-set! node (value)
      (set! value (fail-node! value))
      node))

;*---------------------------------------------------------------------*/
;*    fail-node*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (fail-node*! node*)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (fail-node! (car node*)))
	  (fail-node*! (cdr node*)))))
   

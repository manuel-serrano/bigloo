;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Return/walk.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Mon Mar  9 07:46:48 2020 (serrano)                */
;*    Copyright   :  2010-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Replace set-exit/unwind-until with return. Currently this pass   */
;*    is only executed when generating plain C code.                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module return_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    engine_param
	    backend_backend)
   (export  (return-walk! globals)
	    (init-return-cache!)
	    (function-exit-node ::node)
	    (is-unwind-until?::bool ::variable)
	    (is-get-exitd-top?::bool ::variable)
	    (is-exit-return?::bool ::node ::local)))

;*---------------------------------------------------------------------*/
;*    return-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (return-walk! globals)
   (pass-prelude "Return" init-return-cache!) 
   (for-each return-fun! globals)
   (pass-postlude globals clear-return-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *get-exitd-top* #unspecified)
(define *unwind-until!* #unspecified)
(define *push-exit!* #unspecified)
(define *pop-exit!* #unspecified)

;*---------------------------------------------------------------------*/
;*    init-return-cache! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-return-cache!)
   (unless (global? *get-exitd-top*)
      (set! *get-exitd-top* (find-global '$get-exitd-top 'foreign))
      (set! *unwind-until!* (find-global 'unwind-until! '__bexit))
      (set! *pop-exit!* (find-global 'pop-exit! 'foreign))
      (set! *push-exit!* (find-global 'push-exit! 'foreign)))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-return-cache! ...                                          */
;*---------------------------------------------------------------------*/
(define (clear-return-cache!)
   (set! *get-exitd-top* #f)
   (set! *unwind-until!* #f)
   (set! *push-exit!* #f)
   (set! *pop-exit!* #f))

;*---------------------------------------------------------------------*/
;*    return-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (return-fun! var)
   (enter-function (variable-id var))
   (let* ((fun (variable-value var))
	  (body (sfun-body fun))
	  (exit (function-exit-node body)))
      (when (pair? exit)
	 (let ((exitvar (car exit))
	       (exitnode (cdr exit)))
	    (when (is-exit-return? exitnode exitvar)
	       (let ((rblock (instantiate::retblock
				(loc (node-loc body))
				(type (node-type exitnode))
				(body body))))
		  (with-access::retblock rblock (body)
		     (set! body (return! exitnode exitvar rblock)))
		  (sfun-class-set! fun 'sfun)
		  (sfun-body-set! fun rblock)))))
      (when *optim-return-goto?*
	 (return-goto-funs! body))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    function-exit-node ...                                           */
;*    -------------------------------------------------------------    */
;*    This checks if NODE is a function set-exit body function. That   */
;*    is a node of the form:                                           */
;*                                                                     */
;*    (set-exit an_exit1012                           ;; step0         */
;*       (let ()                                      ;; step1         */
;*          (begin                                    ;; step2         */
;*    	       (push-exit! an_exit1012 1)             ;; step3         */
;*    	       (let ((EXITVAR ($get-exitd-top)))      ;; step4         */
;*    	          (let ((res1015 EXITNODE))           ;; step5         */
;*    	             (begin                           ;; step6         */
;*                      (pop-exit!)                   ;; step7         */
;*                      (f res1015)))))))             ;; step8 (*)     */
;*                                                                     */
;*    (*) the expression might also be (f (cast res1015 ...))          */
;*                                                                     */
;*    It returns <EXITVAR x EXITNODE> or #f                            */
;*---------------------------------------------------------------------*/
(define (function-exit-node node::node)
   
   (define (step8 node::node res::local)
      (cond
	 ((isa? node var)
	  (with-access::var node (variable)
	     (when (eq? variable res)
		node)))
	 ((isa? node app)
	  (with-access::app node (args)
	     (when (any (lambda (n) (step8 n res)) args)
		node)))
	 ((isa? node cast)
	  (with-access::cast node (arg)
	     (when (step8 arg res)
		node)))
	 (else
	  #f)))
   
   (define (step7 node::node)
      (when (isa? node app)
	 (with-access::app node (fun args)
	    (when (null? args)
	       (with-access::var fun (variable)
		  (eq? variable *pop-exit!*))))))
   
   (define (step6 node::node var::local)
      (when (isa? node sequence)
	 (with-access::sequence node (nodes)
	    (when (=fx (length nodes) 2)
	       (when (step7 (car nodes))
		  (step8 (cadr nodes) var))))))
   
   (define (step5 node::node var::local)
      (when (isa? node let-var)
	 (with-access::let-var node (bindings body)
	    (when (and (pair? bindings) (null? (cdr bindings)))
	       (let* ((binding (car bindings))
		      (nbody (step6 body (car binding))))
		  (when nbody
		     (duplicate::let-var node
			(body nbody))))))))
   
   (define (step4 node::node)
      (when (isa? node let-var)
	 (with-access::let-var node (bindings body)
	    (when (and (pair? bindings) (null? (cdr bindings)))
	       (let* ((binding (car bindings))
		      (exitvar (car binding))
		      (expr (cdr binding)))
		  (when (isa? expr app)
		     (with-access::app expr (fun args)
			(when (null? args)
			   (with-access::var fun (variable)
			      (when (eq? variable *get-exitd-top*)
				 binding))))))))))
   
   (define (step3 node::node exitvar::local)
      (when (isa? node app)
	 (with-access::app node (fun args)
	    (with-access::var fun (variable)
	       ;; step 3b
	       (when (eq? variable *push-exit!*)
		  ;; step 3c
		  (when (=fx (length args) 2)
		     ;; step 3d
		     (when (isa? (car args) var)
			(with-access::var (car args) (variable)
			   ;; step 3e
			   (eq? variable exitvar)))))))))
   
   (define (step2 nodes::pair-nil var::local)
      (when (and (=fx (length nodes) 2) (step3 (car nodes) var))
	 (let ((binding (step4 (cadr nodes))))
	    (when binding
	       (with-access::let-var (cadr nodes) (body)
		  (let ((exitnode (step5 body (car binding))))
		     (when exitnode
			(cons (car binding) exitnode))))))))
   
   (define (step1 body::node var::var)
      (when (isa? body let-var)
	 (with-access::let-var body (bindings body)
	    (when (and (null? bindings) (isa? body sequence))
	       (with-access::sequence body (nodes)
		  (with-access::var var (variable)
		     (step2 nodes variable)))))))
   
   (when (isa? node set-ex-it)
      (with-access::set-ex-it node (body var)
	 (step1 body var))))

;*---------------------------------------------------------------------*/
;*    is-unwind-until? ...                                             */
;*---------------------------------------------------------------------*/
(define (is-unwind-until? v::variable)
   (eq? v *unwind-until!*))

;*---------------------------------------------------------------------*/
;*    is-get-exitd-top? ...                                            */
;*---------------------------------------------------------------------*/
(define (is-get-exitd-top? v::variable)
   (eq? v *get-exitd-top*))

;*---------------------------------------------------------------------*/
;*    is-exit-return? ...                                              */
;*    -------------------------------------------------------------    */
;*    Is the exit binding only used as a mere return?                  */
;*---------------------------------------------------------------------*/
(define (is-exit-return? node::node exitvar::local)
   (bind-exit (abort)
      (is-return? node (list exitvar) abort)
      #t))

;*---------------------------------------------------------------------*/
;*    is-return? ...                                                   */
;*---------------------------------------------------------------------*/
(define-walk-method (is-return? node::node exitvar::pair-nil abort)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    is-exit-return? ::var ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (is-return? node::var exitvar abort)
   (with-access::var node (variable)
      (when (memq variable exitvar)
	 (abort #f))))

;*---------------------------------------------------------------------*/
;*    is-exit-return? ::let-var ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (is-return? node::let-var exitvar abort)
   (with-access::let-var node (bindings body)
      (cond
	 ((not *optim-return-goto?*)
	  (call-default-walker))
	 ((and (pair? bindings) (null? (cdr bindings)))
	  (let ((bnode (cdar bindings)))
	     (if (isa? bnode app)
		 (with-access::app bnode (fun)
		    (with-access::var fun (variable)
		       (if (eq? variable *get-exitd-top*)
			   (is-return? body (cons (caar bindings) exitvar) abort)
			   (call-default-walker))))
		 (call-default-walker))))
	 (else
	  (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    is-exit-return? ::app ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (is-return? node::app exitvar abort)
   (with-access::app node (fun args)
      (with-access::var fun (variable)
	 (cond
	    ((eq? variable *unwind-until!*)
	     (if (isa? (car args) var)
		 (with-access::var (car args) (variable)
		    (or (and (pair? exitvar) (memq variable exitvar))
			(is-return? (car args) exitvar abort)))
		 (call-default-walker)))
	    ((eq? variable *get-exitd-top*)
	     (or *optim-return-goto?* (abort #f)))
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    return! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return! node::node exitvar::local rblock::retblock)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    return! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (return! node::app exitvar::local rblock::retblock)
   (with-access::app node (fun args loc)
      (with-access::var fun (variable)
	 (if (eq? variable *unwind-until!*)
	     (begin
		(if (isa? (car args) var)
		    (with-access::var (car args) (variable)
		       (if (eq? variable exitvar)
			   (instantiate::return
			      (loc loc)
			      (type (node-type rblock))
			      (block rblock)
			      (value (cadr args)))
			   (call-default-walker)))
		    (call-default-walker)))
	     (call-default-walker)))))

;*---------------------------------------------------------------------*/
;*    return-goto-funs! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (return-goto-funs! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    return-goto-funs! ::let-fun ...                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (return-goto-funs! node::let-fun)
   (with-access::let-fun node (locals body)
      (for-each return-fun! locals)
      (set! body (return-goto-funs! body))
      node))
		  

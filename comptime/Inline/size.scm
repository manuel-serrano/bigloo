;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Inline/size.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 17 12:06:16 1996                          */
;*    Last change :  Sat Apr 21 18:06:03 2018 (serrano)                */
;*    Copyright   :  1996-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The size an ast node.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_size
   
   (include "Ast/node.sch"
	    "Inline/size.sch")

   (import  tools_shape)
   
   (static  (wide-class sized-sequence::sequence (size::long read-only))
	    (wide-class sized-sync::sync (size::long read-only))
	    (wide-class sized-switch::switch (size::long read-only))
	    (wide-class sized-let-fun::let-fun (size::long read-only))
	    (wide-class sized-let-var::let-var (size::long read-only)))
	      
   (export  (generic node-size::long ::node)))

;*---------------------------------------------------------------------*/
;*    node-size ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node-size::long node::node))

;*---------------------------------------------------------------------*/
;*    node-size ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-size node::atom)
   1)

;*---------------------------------------------------------------------*/
;*    node-size ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-size node::var)
   1)

;*---------------------------------------------------------------------*/
;*    node-size ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-size node::kwote)
   1)
       
;*---------------------------------------------------------------------*/
;*    node-size ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sequence)
   (let loop ((nodes (sequence-nodes node))
	      (size  0))
      (if (null? nodes)
	  (begin
	     (widen!::sized-sequence node (size size))
	     size)
	  (loop (cdr nodes) (+fx size (node-size (car nodes)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::sized-sequence ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sized-sequence)
   (sized-sequence-size node))

;*---------------------------------------------------------------------*/
;*    node-size ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sync)
   (let ((size (+ 1
		  (node-size (sync-mutex node))
		  (node-size (sync-prelock node))
		  (node-size (sync-body node)))))
      (widen!::sized-sync node (size size))
      size))

;*---------------------------------------------------------------------*/
;*    node-size ::sized-sync ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sized-sync)
   (sized-sync-size node))
(define m 0)
;*---------------------------------------------------------------------*/
;*    node-size ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-size node::app)
   (when (getenv "INLINESZ")
      (set! m (+fx m 1))
      (fprint (current-error-port) (make-string m #\space) ">>> inline-size node=" (shape node)))
   (let loop ((args  (app-args node))
	      (size  (node-size (app-fun node))))
      (when (getenv "INLINESZ")
	 (fprint (current-error-port) (make-string m #\space) "--- inline-size node=" (map shape args) " size=" size " null=" (null? args)))
      (if (null? args)
	  (begin
	     (when (getenv "INLINESZ")
		(fprint (current-error-port)
		   (make-string m #\space) "<<< inline-size node=" size)
		(set! m (-fx m 1)))
	     size)
	  (loop (cdr args) (+fx size (node-size (car args)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::app-ly ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size node::app-ly)
   (+fx 1 (+fx (node-size (app-ly-fun node)) (node-size (app-ly-arg node)))))

;*---------------------------------------------------------------------*/
;*    node-size ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size node::funcall)
   (let loop ((args  (funcall-args node))
	      (size  (node-size (funcall-fun node))))
      (if (null? args)
	  size
	  (loop (cdr args) (+fx size (node-size (car args)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-size node::cast)
   (with-access::cast node (arg)
      (node-size arg)))

;*---------------------------------------------------------------------*/
;*    node-size ::extern ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size node::extern)
   (let loop ((args (extern-expr* node))
	      (size 1))
      (if (null? args)
	  size
	  (loop (cdr args) (+fx size (node-size (car args)))))))

;*---------------------------------------------------------------------*/
;*    node-size ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-size node::setq)
   (+fx 2 (node-size (setq-value node))))

;*---------------------------------------------------------------------*/
;*    node-size ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-size node::conditional)
   (let* ((test-size  (node-size (conditional-test node)))
	  (true-size  (node-size (conditional-true node)))
	  (false-size (node-size (conditional-false node))))
      (+fx 1 (+fx test-size (+fx true-size false-size)))))

;*---------------------------------------------------------------------*/
;*    node-size ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-size node::fail)
   (let* ((proc-size (node-size (fail-proc node)))
	  (msg-size  (node-size (fail-msg node)))
	  (obj-size  (node-size (fail-obj node))))
      (+fx 2 (+fx proc-size (+fx msg-size obj-size)))))

;*---------------------------------------------------------------------*/
;*    node-size ::switch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size node::switch)
   (let loop ((clauses (switch-clauses node))
	      (size    (+fx 1 (node-size (switch-test node)))))
      (if (null? clauses)
	  (begin
	     (widen!::sized-switch node (size size))
	     size)
	  (loop (cdr clauses)
		(+fx size (+fx (if (pair? (car (car clauses)))
				   (length (car (car clauses)))
				   1)
			       (node-size (cdr (car clauses)))))))))

;*---------------------------------------------------------------------*/
;*    node-size ::sized-switch ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sized-switch)
   (sized-switch-size node))

;*---------------------------------------------------------------------*/
;*    node-size ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size node::let-fun)
   (let loop ((locals (let-fun-locals node))
	      (size   (+fx (length (let-fun-locals node))
			   (node-size (let-fun-body node))))) 
      (if (null? locals)
	  (begin
	     (widen!::sized-let-fun node (size size))
	     (+fx 1 size))
	  (loop (cdr locals)
		(+fx size
		     (+fx 1 (node-size (sfun-body (local-value (car locals))))))))))

;*---------------------------------------------------------------------*/
;*    node-size ::sized-let-fun ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sized-let-fun)
   (sized-let-fun-size node))

;*---------------------------------------------------------------------*/
;*    node-size ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size node::let-var)
   (let loop ((bindings (let-var-bindings node))
	      (size     (node-size (let-var-body node))))
      (if (null? bindings)
	  (begin
	     (widen!::sized-let-var node (size size))
	     size)
	  (loop (cdr bindings)
		(+fx size (node-size (cdr (car bindings))))))))

;*---------------------------------------------------------------------*/
;*    node-size ::sized-let-var ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-size node::sized-let-var)
   (sized-let-var-size node))

;*---------------------------------------------------------------------*/
;*    node-size ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-size node::set-ex-it)
   (+fx 2 (node-size (set-ex-it-body node))))

;*---------------------------------------------------------------------*/
;*    node-size ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-size node::jump-ex-it)
   (+fx 1
	(+fx (node-size (jump-ex-it-exit node))
	     (node-size (jump-ex-it-value node)))))

;*---------------------------------------------------------------------*/
;*    node-size ::retblock ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-size node::retblock)
   (with-access::retblock node (body)
      (node-size body)))

;*---------------------------------------------------------------------*/
;*    node-size ::return ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-size node::return)
   (with-access::return node (value)
      (node-size value)))

;*---------------------------------------------------------------------*/
;*    node-size ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-size node::make-box)
   (+fx 1 (node-size (make-box-value node))))

;*---------------------------------------------------------------------*/
;*    node-size ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-size node::box-ref)
   2)

;*---------------------------------------------------------------------*/
;*    node-size ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-size node::box-set!)
   (+fx 2 (node-size (box-set!-value node))))




   

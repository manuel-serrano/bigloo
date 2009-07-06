;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Inline/loop.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 11 09:27:29 1996                          */
;*    Last change :  Thu Jul 13 11:12:05 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The loop unrolling module.                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_loop
   (include "Tools/trace.sch")
   (import  engine_param
	    type_type
	    ast_var
	    ast_node
	    ast_local
	    ast_sexp
	    tools_speek
	    tools_shape
	    tools_error
	    inline_inline
	    inline_variant
	    inline_recursion
	    effect_effect)
   (export  (is-loop?::bool ::variable)
	    (inner-loop?::bool ::variable)
	    (generic nest-loop!::node ::node ::local ::procedure)))

;*---------------------------------------------------------------------*/
;*    is-loop? ...                                                     */
;*    -------------------------------------------------------------    */
;*    A loop is a recursion with exactly one recursive call.           */
;*---------------------------------------------------------------------*/
(define (is-loop? variable)
   ;; firstly we call `is-recursive?' to ensure that variable is widened.
   (and (is-recursive? variable)
	(=fx (length (isfun-recursive-calls (variable-value variable))) 1)))

;*---------------------------------------------------------------------*/
;*    inner-loop? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Does a loop nests inner loops ?                                  */
;*---------------------------------------------------------------------*/
(define (inner-loop? variable)
   (find-let-fun? (sfun-body (variable-value variable))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (find-let-fun? node::node)
   #f)

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::sequence)
   (find-let-fun?* (sequence-nodes node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::app)
   (with-access::app node (args)
      (find-let-fun?* args)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::app-ly ...                                       */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::app-ly)
   (with-access::app-ly node (fun arg)
      (or (find-let-fun? fun) (find-let-fun? arg))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::funcall ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::funcall)
   (with-access::funcall node (fun args)
      (or (find-let-fun? fun)
	  (find-let-fun?* args))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::extern ...                                       */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::extern)
   (find-let-fun?* (extern-expr* node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::cast ...                                         */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::cast)
   (find-let-fun? (cast-arg node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::setq ...                                         */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::setq)
   (find-let-fun? (setq-value node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::conditional)
   (with-access::conditional node (test true false)
       (or (find-let-fun? test)
	   (find-let-fun? true)
	   (find-let-fun? false))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::fail ...                                         */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::fail)
   (with-access::fail node (proc msg obj)
      (or (find-let-fun? proc)
	  (find-let-fun? msg)
	  (find-let-fun? obj))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::select ...                                       */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::select)
   (let loop ((clauses (select-clauses node)))
      (cond
	 ((null? clauses)
	  (find-let-fun? (select-test node)))
	 ((find-let-fun? (cdr (car clauses)))
	  #t)
	 (else
	  (loop (cdr clauses))))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::let-fun)
   #t)

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::let-var)
   (let loop ((bindings (let-var-bindings node)))
      (cond
	 ((null? bindings)
	  (find-let-fun? (let-var-body node)))
	 ((find-let-fun? (cdr (car bindings)))
	  #t)
	 (else
	  (loop (cdr bindings))))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::set-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::set-ex-it)
   (find-let-fun? (set-ex-it-body node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::jump-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (or (find-let-fun? exit) (find-let-fun? value))))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::make-box ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::make-box)
   (find-let-fun? (make-box-value node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::box-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::box-ref)
   (find-let-fun? (box-ref-var node)))

;*---------------------------------------------------------------------*/
;*    find-let-fun? ::box-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-method (find-let-fun? node::box-set!)
   (with-access::box-set! node (var value)
      (or (find-let-fun? var) (find-let-fun? value))))

;*---------------------------------------------------------------------*/
;*    find-let-fun?* ...                                               */
;*---------------------------------------------------------------------*/
(define (find-let-fun?* node*)
   (let loop ((node* node*))
      (cond
	 ((null? node*)
	  #f)
	 ((find-let-fun? (car node*))
	  #t)
	 (else
	  (loop (cdr node*))))))
   
;*---------------------------------------------------------------------*/
;*    nest-loop! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (nest-loop!::node node::node local::local nester::procedure))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::atom var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::kwote var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::var var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::sequence var nester)
   (with-access::sequence node (nodes)
      (nest-loop!* nodes var nester)
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::app var nester)
   (with-access::app node (fun args)
      (nest-loop!* args var nester)
      (if (and (var? fun) (eq? (var-variable fun) var))
	  (nester node)
	  node)))
 
;*---------------------------------------------------------------------*/
;*    nest-loop! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::app-ly var nester)
   (with-access::app-ly node (fun arg)
      (set! fun (nest-loop! fun var nester))
      (set! arg (nest-loop! arg var nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::funcall var nester)
   (with-access::funcall node (fun args)
      (set! fun (nest-loop! fun var nester))
      (nest-loop!* args var nester)
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::extern var nester)
   (nest-loop!* (extern-expr* node) var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::cast var nester)
   (nest-loop! (cast-arg node) var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::setq var nester)
   (nest-loop! (setq-value node) var nester)
   node)

;*---------------------------------------------------------------------*/
;*    nest-loop! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::conditional var nester)
   (with-access::conditional node (test true false)
       (set! test (nest-loop! test var nester))
       (set! true (nest-loop! true var nester))
       (set! false (nest-loop! false var nester))
       node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::fail var nester)
   (with-access::fail node (proc msg obj)
      (set! proc (nest-loop! proc var nester))
      (set! msg (nest-loop! msg var nester))
      (set! obj (nest-loop! obj var nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::select var nester)
   (with-access::select node (test clauses)
      (set! test (nest-loop! test var nester))
      (for-each (lambda (clause)
		   (set-cdr! clause (nest-loop! (cdr clause) var nester)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::let-fun var nester)
   (with-access::let-fun node (body locals)
      (set! body (nest-loop! body var nester))
      (for-each (lambda (local)
		   (let ((sfun (local-value local)))
		      (sfun-body-set! sfun
				      (nest-loop! (sfun-body sfun)
						  var
						  nester))))
		locals)
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::let-var var nester)
   (with-access::let-var node (body bindings)
      (set! body (nest-loop! body var nester))
      (for-each (lambda (binding)
		   (set-cdr! binding (nest-loop! (cdr binding) var nester)))
		bindings)
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::set-ex-it var nester)
   (with-access::set-ex-it node (body)
      (set! body (nest-loop! body var nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::jump-ex-it v nester)
   (with-access::jump-ex-it node (exit value)
      (set! exit (nest-loop! exit v nester))
      (set! value (nest-loop! value v nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::make-box var nester)
   (with-access::make-box node (value)
      (set! value (nest-loop! value var nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::box-ref v nester)
   (with-access::box-ref node (var)
      (set! var (nest-loop! var v nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (nest-loop! node::box-set! v nester)
   (with-access::box-set! node (var value)
      (set! var (nest-loop! var v nester))
      (set! value (nest-loop! value v nester))
      node))

;*---------------------------------------------------------------------*/
;*    nest-loop!* ...                                                  */
;*---------------------------------------------------------------------*/
(define (nest-loop!* node* var nester)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (nest-loop! (car node*) var nester))
	     (loop (cdr node*))))))
   

   
   
   
		
	    

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/escape.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 21 09:02:16 1996                          */
;*    Last change :  Tue May 20 16:51:39 2003 (serrano)                */
;*    Copyright   :  1996-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The escape property computation                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_escape
   (import  tools_shape
	    type_type
	    engine_param
	    ast_var 
	    ast_node
	    globalize_ginfo)
   (export  (generic escape-fun! ::variable)))

;*---------------------------------------------------------------------*/
;*    define-generic ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (escape-fun! variable::variable)
   (let ((fun (variable-value variable)))
      (for-each (lambda (local)
		   (widen!::svar/Ginfo (local-value local)))
		(sfun-args fun))
      (widen!::sfun/Ginfo fun)
      (escape! (sfun-body fun) variable)))

;*---------------------------------------------------------------------*/
;*    escape-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (escape-fun! variable::global)
   (if (not (global/Ginfo? variable))
       (widen!::global/Ginfo variable
	  (escape? (eq? (global-import variable) 'export))))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    escape-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (escape-fun! variable::local)
   (if (not (local/Ginfo? variable))
       (widen!::local/Ginfo variable))
   (call-next-method))

;*---------------------------------------------------------------------*/
;*    set-escaping-fun! ::variable ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (set-escaping-fun! variable::variable)
   (error "set-escaping-fun!"
	  "Illegal variable"
	  (cons variable (shape variable))))

;*---------------------------------------------------------------------*/
;*    set-escaping-fun! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-escaping-fun! variable::global)
   (widen!::global/Ginfo variable (escape? #t)))

;*---------------------------------------------------------------------*/
;*    set-escaping-fun! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-escaping-fun! variable::global/Ginfo)
   (global/Ginfo-escape?-set! variable #t))

;*---------------------------------------------------------------------*/
;*    set-escaping-fun! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (set-escaping-fun! variable::local/Ginfo)
   (local/Ginfo-escape?-set! variable #t))

;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (escape! node::node o::variable))

;*---------------------------------------------------------------------*/
;*    escape! ::atom ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape! node::atom o)
   'done)

;*---------------------------------------------------------------------*/
;*    escape! ::kwote ...                                              */
;*---------------------------------------------------------------------*/
(define-method (escape! node::kwote o)
   'done)

;*---------------------------------------------------------------------*/
;*    escape! ::var ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape! node::var o)
   'done)

;*---------------------------------------------------------------------*/
;*    escape! ::closure ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! node::closure o)
   (set-escaping-fun! (closure-variable node)))

;*---------------------------------------------------------------------*/
;*    escape! ::sequence ...                                           */
;*---------------------------------------------------------------------*/
(define-method (escape! node::sequence o)
   (escape*! (sequence-nodes node) o))

;*---------------------------------------------------------------------*/
;*    escape! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape! node::app o)
   (with-access::app node (args)
      (escape*! args o)))
 
;*---------------------------------------------------------------------*/
;*    escape! ::app-ly ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape! node::app-ly o)
   (with-access::app-ly node (fun arg)
      (escape! fun o)
      (escape! arg o)))

;*---------------------------------------------------------------------*/
;*    escape! ::funcall ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! node::funcall o)
   (with-access::funcall node (fun args)
      (escape! fun o)
      (escape*! args o)))

;*---------------------------------------------------------------------*/
;*    escape! ::extern ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape! node::extern o)
   (escape*! (extern-expr* node) o))

;*---------------------------------------------------------------------*/
;*    escape! ::cast ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape! node::cast o)
   (escape! (cast-arg node) o))

;*---------------------------------------------------------------------*/
;*    escape! ::setq ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape! node::setq o)
   (with-access::setq node (value)
      (escape! value o)))

;*---------------------------------------------------------------------*/
;*    escape! ::conditional ...                                        */
;*---------------------------------------------------------------------*/
(define-method (escape! node::conditional o)
   (with-access::conditional node (test true false)
       (escape! test o)
       (escape! true o)
       (escape! false o)))

;*---------------------------------------------------------------------*/
;*    escape! ::fail ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape! node::fail o)
   (with-access::fail node (proc msg obj)
      (escape! proc o)
      (escape! msg o)
      (escape! obj o)))

;*---------------------------------------------------------------------*/
;*    escape! ::select ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape! node::select o)
   (with-access::select node (clauses test)
      (escape! test o)
      (for-each (lambda (clause)
		   (escape! (cdr clause) o))
		clauses)))

;*---------------------------------------------------------------------*/
;*    escape! ::let-fun ...                                            */
;*    -------------------------------------------------------------    */
;*    We cannot use the `escape-fun!' function because all the         */
;*    functions need to be widened before we scan the bodies. Hence,   */
;*    a fully ad-hoc function is preferable.                           */
;*---------------------------------------------------------------------*/
(define-method (escape! node::let-fun o)
   (with-access::let-fun node (body locals)
      ;; first, we enlarge all defined functions
      (for-each (lambda (local)
		   (widen!::local/Ginfo local)
		   (let ((fun (local-value local)))
		      (widen!::sfun/Ginfo fun
			 (owner o))
		      (for-each (lambda (local)
				   (widen!::svar/Ginfo (local-value local)))
				(sfun-args fun))))
		locals)
      ;; then, we scan the bodies
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (escape! (sfun-body fun) local)))
		locals)
      ;; and we scan the body of the labels.
      (escape! body o)))

;*---------------------------------------------------------------------*/
;*    escape! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! node::let-var o)
   (with-access::let-var node (body bindings)
      (escape! body o)
      (for-each (lambda (binding)
		   (widen!::svar/Ginfo (local-value (car binding)))
		   (escape! (cdr binding) o))
		bindings)))

;*---------------------------------------------------------------------*/
;*    escape! ::set-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (escape! node::set-ex-it o)
   (with-access::set-ex-it node (var body)
      (widen!::sexit/Ginfo (local-value (var-variable var)))
      (escape! body o)))

;*---------------------------------------------------------------------*/
;*    escape! ::jump-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (escape! node::jump-ex-it o)
   (with-access::jump-ex-it node (exit value)
      (escape! exit o) 
      (escape! value o)))

;*---------------------------------------------------------------------*/
;*    escape! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (escape! node::make-box o)
   (escape! (make-box-value node) o))

;*---------------------------------------------------------------------*/
;*    escape! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (escape! node::box-set! o)
   (with-access::box-set! node (value)
      (escape! value o)))

;*---------------------------------------------------------------------*/
;*    escape! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (escape! node::box-ref o)
   (with-access::box-ref node (var)
      (escape! var o)))

;*---------------------------------------------------------------------*/
;*    escape*! ...                                                     */
;*---------------------------------------------------------------------*/
(define (escape*! node* o)
   (for-each (lambda (n) (escape! n o)) node*))



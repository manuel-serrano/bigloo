;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Effect/cgraph.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 15 10:55:41 1995                          */
;*    Last change :  Sun Dec 24 14:30:52 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of the call-graph                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module effect_cgraph
   (include "Ast/node.sch"
	    "Tools/trace.sch")
   (import  tools_shape
	    tools_error)
   (export  (wide-class local/from::local from)
	    (wide-class global/from::global from)
	    (fun-call-graph! ::variable)
	    (get-var/all)
	    (get-var/side-effect)
	    (reset-effect-tables!)))

;*---------------------------------------------------------------------*/
;*    fun-call-graph! ...                                              */
;*---------------------------------------------------------------------*/
(define (fun-call-graph! variable)
   (set! *var/all* (cons variable *var/all*))
   (call-graph! (sfun-body (variable-value variable)) variable))

;*---------------------------------------------------------------------*/
;*    call-graph! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (call-graph! node::node owner::variable)
   'done)

;*---------------------------------------------------------------------*/
;*    call-graph! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::sequence owner)
   (call-graph*! (sequence-nodes node) owner))

;*---------------------------------------------------------------------*/
;*    call-graph! ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::app owner)
   (with-access::app node (fun args)
      ;; the save-call mark the side effect when
      ;; the callee is imported and has no pragma.
      (save-call! (var-variable fun) owner)
      (call-graph*! args owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::app-ly owner)
   (with-access::app-ly node (fun arg)
      (mark-side-effect! owner)
      (call-graph! fun owner)
      (call-graph! arg owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::funcall owner)
   (with-access::funcall node (fun args)
      (mark-side-effect! owner)
      (call-graph! fun owner)
      (call-graph*! args owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::extern owner)
   (with-access::extern node (side-effect? expr*)
      (if side-effect?
	  (mark-side-effect! owner))
      (call-graph*! expr* owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::cast owner)
   (with-access::cast node (arg)
      (call-graph! arg owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::setq ...                                           */
;*    -------------------------------------------------------------    */
;*    A `set!' is a side effect only when:                             */
;*      - the concerned variable is global                             */
;*      - the owner is local.                                          */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::setq owner)
   (with-access::setq node (var value)
      (if (or (local? owner) (global? (var-variable var)))
	  (mark-side-effect! owner))
      (call-graph! var owner)
      (call-graph! value owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::conditional owner)
   (with-access::conditional node (test true false)
      (call-graph! test owner)
      (call-graph! true owner)
      (call-graph! false owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::fail owner)
   (with-access::fail node (proc msg obj)
      (mark-side-effect! owner)
      (call-graph! proc owner)
      (call-graph! msg owner)
      (call-graph! obj owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::select ...                                         */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::select owner)
   (with-access::select node (test clauses)
      (call-graph! test owner)
      (for-each (lambda (clause)
		   (call-graph! (cdr clause) owner))
		clauses)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::let-fun owner)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (fun-call-graph! local))
		locals)
      (call-graph! body owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::let-var owner)
   (with-access::let-var node (body bindings)
      (call-graph! body owner)
      (for-each (lambda (binding)
		   (call-graph! (cdr binding) owner))
		bindings)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::set-ex-it owner)
   (with-access::set-ex-it node (var)
      (mark-side-effect! owner)
      (call-graph! (set-ex-it-body node) owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::jump-ex-it owner)
   (with-access::jump-ex-it node (exit value)
      (mark-side-effect! owner)
      (call-graph! exit owner)
      (call-graph! value owner)))

;*---------------------------------------------------------------------*/
;*    call-graph! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::make-box owner)
   (call-graph! (make-box-value node) owner))

;*---------------------------------------------------------------------*/
;*    call-graph! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::box-ref owner)
   (call-graph! (box-ref-var node) owner))

;*---------------------------------------------------------------------*/
;*    call-graph! ::box-set! ...                                       */
;*    -------------------------------------------------------------    */
;*    Same remark as for `set!'.                                       */
;*---------------------------------------------------------------------*/
(define-method (call-graph! node::box-set! owner)
   (with-access::box-set! node (var value)
      (if (or (local? owner) (global? (var-variable var)))
	  (mark-side-effect! owner))
      (call-graph! var owner)
      (call-graph! value owner)))

;*---------------------------------------------------------------------*/
;*    call-graph*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (call-graph*! node* owner)
   (for-each (lambda (node) (call-graph! node owner)) node*))

;*---------------------------------------------------------------------*/
;*    save-call!                                                       */
;*---------------------------------------------------------------------*/
(define-generic (save-call! callee::variable owner::variable))

;*---------------------------------------------------------------------*/
;*    save-call!                                                       */
;*---------------------------------------------------------------------*/
(define-method (save-call! callee::global owner)
   (let ((fun (global-value callee)))
      (if (or (cfun? fun)
	      (and (sfun? fun)
		   (eq? (global-import callee) 'import)))
	  (if (fun-side-effect? fun)
	      (mark-side-effect! owner))
	  (save-call! (widen!::global/from callee (from '())) owner))))

;*---------------------------------------------------------------------*/
;*    save-call! ::global/from ...                                     */
;*---------------------------------------------------------------------*/
(define-method (save-call! callee::global/from owner)
   (with-access::global/from callee (from)
      (if (not (memq owner from))
	  (set! from (cons owner from)))))

;*---------------------------------------------------------------------*/
;*    save-call! ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (save-call! callee::local owner)
   (save-call! (widen!::local/from callee (from '())) owner))

;*---------------------------------------------------------------------*/
;*    save-call! ::local/from ...                                      */
;*---------------------------------------------------------------------*/
(define-method (save-call! callee::local/from owner)
   (with-access::local/from callee (from)
      (if (not (memq owner from))
	  (set! from (cons owner from)))))

;*---------------------------------------------------------------------*/
;*    get-var/side-effect ...                                          */
;*---------------------------------------------------------------------*/
(define (get-var/side-effect)
   *var/side-effect*)

;*---------------------------------------------------------------------*/
;*    *var/side-effect* ...                                            */
;*---------------------------------------------------------------------*/
(define *var/side-effect* '())

;*---------------------------------------------------------------------*/
;*    get-var/all ...                                                  */
;*---------------------------------------------------------------------*/
(define (get-var/all)
   *var/all*)

;*---------------------------------------------------------------------*/
;*    *var/all* ...                                                    */
;*---------------------------------------------------------------------*/
(define *var/all* '())

;*---------------------------------------------------------------------*/
;*    reset-effect-tables! ...                                         */
;*---------------------------------------------------------------------*/
(define (reset-effect-tables!)
   (set! *var/side-effect* '())
   (set! *var/all* '()))

;*---------------------------------------------------------------------*/
;*    mark-side-effect! ...                                            */
;*---------------------------------------------------------------------*/
(define (mark-side-effect! v::variable)
   (trace (effect 2) "!!! mark-side-effect!(" (shape v) ")" #\Newline)
   (let ((fun (variable-value v)))
      (cond
	 ((not (fun-side-effect? fun))
	  (set! *var/side-effect* (cons v *var/side-effect*))
	  (fun-side-effect?-set! fun #t))
	 ((not (memq v *var/side-effect*))
	  (set! *var/side-effect* (cons v *var/side-effect*)))
	 (else
	  'nothing))))
	  

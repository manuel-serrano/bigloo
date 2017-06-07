;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Effect/spread.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 20 11:36:01 1996                          */
;*    Last change :  Wed Jun  7 06:04:31 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We spread the computed side-effect properties                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module effect_spread
   (include "Ast/node.sch")
   (use     tools_shape)
   (export  (generic spread-side-effect!::bool ::node)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (spread-side-effect!::bool node::node)
   #f)

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::var ...                                    */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::var)
   (not (eq? (variable-access (var-variable node)) 'read)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::patch ...                                  */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::patch)
   (spread-side-effect! (patch-value node))
   #t)

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::sequence ...                               */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::sequence)
   (let ((res (spread-side-effect*! (sequence-nodes node))))
      (sequence-side-effect-set! node res)
      res))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::sync ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::sync)
   (spread-side-effect! (sync-mutex node))
   (spread-side-effect! (sync-prelock node))
   (spread-side-effect! (sync-body node))
   #t)

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::app ...                                    */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::app)
   (with-access::app node (side-effect fun args)
      (let ((res (or (spread-side-effect*! args)
		     (fun-side-effect (variable-value (var-variable fun))))))
	 (set! side-effect res)
	 res)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::app-ly ...                                 */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::app-ly)
   (with-access::app-ly node (fun arg)
      (spread-side-effect! fun)
      (spread-side-effect! arg)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::funcall ...                                */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::funcall)
   (with-access::funcall node (fun args)
      (spread-side-effect! fun)
      (spread-side-effect*! args)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::extern ...                                 */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::extern)
   (with-access::extern node (side-effect expr*)
      (let ((res (or (spread-side-effect*! expr*) side-effect)))
	 (set! side-effect res)
	 res)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::cast ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::cast)
   (with-access::cast node (arg)
      (spread-side-effect! arg)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::setq ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::setq)
   (with-access::setq node (value)
      (spread-side-effect! value)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::conditional ...                            */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::conditional)
   (with-access::conditional node (side-effect test true false)
      (let* ((res-test  (spread-side-effect! test))
	     (res-true  (spread-side-effect! true))
	     (res-false (spread-side-effect! false))
	     (res       (or res-test res-true res-false)))
	 (set! side-effect res)
	 res)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::fail ...                                   */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::fail)
   (with-access::fail node (proc msg obj)
      (let* ((res-proc (spread-side-effect! proc))
	     (res-msg  (spread-side-effect! msg))
	     (res-obj  (spread-side-effect! obj)))
	 #t)))
   
;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::switch ...                                 */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::switch)
   (with-access::switch node (side-effect test clauses)
      (let loop ((clauses clauses)
		 (res     (spread-side-effect! test)))
	 (if (null? clauses)
	     (begin
		(set! side-effect res)
		res)
	     (loop (cdr clauses)
		   (or (spread-side-effect! (cdr (car clauses))) res))))))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::let-fun ...                                */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::let-fun)
   (with-access::let-fun node (side-effect body locals)
      (for-each (lambda (local)
		   (spread-side-effect! (sfun-body (local-value local))))
		locals)
      (let ((res (spread-side-effect! body)))
	 (set! side-effect res)
	 res)))
 
;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::let-var ...                                */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::let-var)
   (with-access::let-var node (side-effect body bindings)
      (let loop ((bdgs bindings)
		 (res  (spread-side-effect! body)))
	 (if (null? bdgs)
	     (begin
		(set! side-effect res)
		res)
	     (loop (cdr bdgs)
		   (or (spread-side-effect! (cdr (car bdgs))) res))))))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::set-ex-it ...                              */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::set-ex-it)
   (with-access::set-ex-it node (body)
      (spread-side-effect! body)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::jump-ex-it ...                             */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (spread-side-effect! exit)
      (spread-side-effect! value)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::make-box ...                               */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::make-box)
   (with-access::make-box node (side-effect value)
      (let ((res (spread-side-effect! value)))
	 (set! side-effect res)
	 res)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::box-set! ...                               */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::box-set!)
   (with-access::box-set! node (value)
      (spread-side-effect! value)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::return ...                                 */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::return)
   (with-access::return node (value)
      (spread-side-effect! value)
      #t))

;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::retblock ...                               */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::retblock)
   (with-access::retblock node (body)
      (spread-side-effect! body)))
   
;*---------------------------------------------------------------------*/
;*    spread-side-effect! ::box-ref ...                                */
;*    -------------------------------------------------------------    */
;*    Many thanks to Dominique Boucher (dboucher@locus.ca) who         */
;*    pointed me a bug. Box-ref where omitted from the Effect          */
;*    property computation!                                            */
;*---------------------------------------------------------------------*/
(define-method (spread-side-effect! node::box-ref)
   (with-access::box-ref node (var)
      (spread-side-effect! var)))

;*---------------------------------------------------------------------*/
;*    spread-side-effect*! ...                                         */
;*---------------------------------------------------------------------*/
(define (spread-side-effect*! node*)
   (let loop ((node* node*)
	      (res   #f))
      (if (null? node*)
	  res
	  (loop (cdr node*) (or (spread-side-effect! (car node*)) res)))))


   

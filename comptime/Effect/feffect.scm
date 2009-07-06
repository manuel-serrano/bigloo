;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Effect/feffect.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Sep 26 08:48:52 2003                          */
;*    Last change :  Fri Dec  5 10:12:31 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The effect of the functions (i.e. does a function read a pair,   */
;*    does it set a vector or a global variable, ...).                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module effect_feffect
   (include "Tools/trace.sch")
   (import  type_type
	    tools_shape
	    tools_error
	    ast_node
	    ast_var
	    effect_cgraph)
   (export  (parse-effect::feffect ::obj)
	    (feffect! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    feffect! ...                                                     */
;*    -------------------------------------------------------------    */
;*    Computes the fix point of the feffect property.                  */
;*---------------------------------------------------------------------*/
(define (feffect! globals)
   (for-each fun-effect-module! globals)
   (feffect-fix-point! globals))

;*---------------------------------------------------------------------*/
;*    feffect-fix-point! ...                                           */
;*---------------------------------------------------------------------*/
(define (feffect-fix-point! globals)
   (let ((changed #t))
      (define (iterate-function! var)
	 (define (merge-caller! f)
	    (let ((ef (fun-effect (variable-value f)))
		  (et (fun-effect (variable-value var))))
	       (unless (feffect? ef) (set! ef *effect-top*))
	       (set! changed (or (merge-effects! et ef) changed))))
	 (let* ((fun (variable-value var)))
	    (cond
	       ((local/from? var)
		(for-each merge-caller! (local/from-from var)))
	       ((global/from? var)
		(for-each merge-caller! (global/from-from var))))))
      (let loop ()
	 (if changed
	     (begin
		(set! changed #f)
		(for-each iterate-function! globals)
		(loop))))))

;*---------------------------------------------------------------------*/
;*    object-display ::feffect ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display f::feffect . p)
   (with-output-to-port (if (pair? p) (car p) (current-output-port))
      (lambda ()
	 (with-access::feffect f (read write)
	    (printf "#|feffect: ~a ~a|" read write)))))

;*---------------------------------------------------------------------*/
;*    pre-created effects                                              */
;*---------------------------------------------------------------------*/
(define *effect-top* (instantiate::feffect (read 'top) (write 'top)))
(define *effect-read-mem* (instantiate::feffect (read '(memory))))
(define *effect-write-mem* (instantiate::feffect (write '(memory))))

;*---------------------------------------------------------------------*/
;*    parse-effect ...                                                 */
;*---------------------------------------------------------------------*/
(define (parse-effect prop)
   (let ((e (instantiate::feffect)))
      (let loop ((v (cdr prop)))
	 (if (null? v)
	     e
	     (match-case (car v)
		((read ?r)
		 (feffect-read-set! e r)
		 (loop (cdr v)))
		((write ?w)
		 (feffect-write-set! e w)
		 (loop (cdr v)))
		(else
		 (user-error "Parse error" "Illegal `effect' pragma" v)))))))

;*---------------------------------------------------------------------*/
;*    merge-effects! ...                                               */
;*    -------------------------------------------------------------    */
;*    Incorporates the feffect F2 into F1. Returns a boolean if        */
;*    something has been added to F1.                                  */
;*---------------------------------------------------------------------*/
(define (merge-effects!::bool f1 f2)
   (define changed #f)
   (define (union! l1 l2)
      (cond
	 ((null? l1)
	  (if (pair? l2)
	      (set! changed #t))
	  l2)
	 ((null? l2) 
	  l1)
	 (else
	  (let ((k (gensym)))
	     (for-each (lambda (s)
			  (putprop! s k #t))
		       l1)
	     (for-each (lambda (s)
			  (if (not (getprop s k))
			      (begin
				 (set! changed #t)
				 (set! l1 (cons s l1)))))
		       l2)
	     (for-each (lambda (s)
			  (remprop! s k))
		       l1)
	     l1))))
   (define (merge! e1 e2)
      (cond
	 ((eq? e1 'top)
	  e1)
	 ((null? e2)
	  e1)
	 ((eq? e2 'top)
	  (set! changed #t)
	  'top)
	 (else
	  (union! e1 e2))))
   (feffect-read-set! f1 (merge! (feffect-read f1) (feffect-read f2)))
   (feffect-write-set! f1 (merge! (feffect-write f1) (feffect-write f2)))
   changed)

;*---------------------------------------------------------------------*/
;*    fun-effect! ...                                                  */
;*---------------------------------------------------------------------*/
(define (fun-effect!::feffect v)
   (if (global? v)
       (fun-effect-global! v)
       (fun-effect-local! v)))

;*---------------------------------------------------------------------*/
;*    fun-effect-global! ...                                           */
;*---------------------------------------------------------------------*/
(define (fun-effect-global!::feffect v::global)
   (if (or (not (memq (global-import v) '(static export)))
	   (not (sfun? (global-value v))))
       (fun-effect-alien! v)
       (fun-effect-module! v)))

;*---------------------------------------------------------------------*/
;*    fun-effect-alien! ...                                            */
;*    -------------------------------------------------------------    */
;*    Returns the effect of an imported or extern function.            */
;*---------------------------------------------------------------------*/
(define (fun-effect-alien!::feffect g::global)
   (with-access::fun (variable-value g) (effect)
      (if (not (feffect? effect))
	  (set! effect *effect-top*))
      effect))

;*---------------------------------------------------------------------*/
;*    fun-effect-module! ...                                           */
;*---------------------------------------------------------------------*/
(define (fun-effect-module!::feffect v::global)
   (let* ((fun (variable-value v))
	  (ef (fun-effect fun)))
      (trace egen "fun-effect-module!: " (shape v) " ef="
	     (with-output-to-string (lambda () (display ef))) #\Newline)
      (if (feffect? fun)
	  ef
	  (do-fun-effect! v))))

;*---------------------------------------------------------------------*/
;*    fun-effect-local! ...                                            */
;*---------------------------------------------------------------------*/
(define (fun-effect-local!::feffect v::local)
   (do-fun-effect! v))

;*---------------------------------------------------------------------*/
;*    do-fun-effect! ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is incomplete, it does not compute the small       */
;*    fix point effect property. It does iterate to the fix point.     */
;*    It traverses the functions only once. The effect property        */
;*    has to be complete with an ad-hoc fix point iteration            */
;*    (see FEFFECT! and FEFFECT-FIX-POINT!).                           */
;*---------------------------------------------------------------------*/
(define (do-fun-effect!::feffect v::variable)
   (with-access::sfun (variable-value v) (effect body)
      (if (feffect? effect)
	  effect
	  (begin
	     (set! effect (instantiate::feffect))
	     (body-effect! body effect)))))

;*---------------------------------------------------------------------*/
;*    body-effect! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Walk thru the body of the functions without traversing the       */
;*    application nodes. These nodes are handled separately during     */
;*    a fix point iteration.                                           */
;*---------------------------------------------------------------------*/
(define-generic (body-effect!::feffect node::node effect::feffect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::atom effect)
   effect)

;*---------------------------------------------------------------------*/
;*    body-effect! ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::kwote effect)
   effect)

;*---------------------------------------------------------------------*/
;*    body-effect! ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::var effect::feffect)
   (if (global? (var-variable node))
       (merge-effects! effect *effect-read-mem*))
   effect)
 
;*---------------------------------------------------------------------*/
;*    body-effect! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::setq effect::feffect)
   (with-access::setq node (var value)
      (if (global? (var-variable var))
	  (merge-effects! effect *effect-write-mem*))
      (body-effect! value effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::conditional effect::feffect)
   (with-access::conditional node (test true false)
      (body-effect! test effect)
      (body-effect! true effect)
      (body-effect! false effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::select effect::feffect)
   (with-access::select node (test clauses)
      (body-effect! test effect)
      (for-each (lambda (clause)
		   (body-effect! (cdr clause) effect))
		clauses)
      effect))
   
;*---------------------------------------------------------------------*/
;*    body-effect! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::fail effect::feffect)
   (with-access::fail node (proc msg obj)
      (body-effect! proc effect)
      (body-effect! msg effect)
      (body-effect! obj effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::set-ex-it effect::feffect)
   (with-access::set-ex-it node (var)
      (body-effect! (set-ex-it-body node) effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::jump-ex-it effect::feffect)
   (with-access::jump-ex-it node (exit value)
      (body-effect! exit effect)
      (body-effect! value effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::make-box effect::feffect)
   (body-effect! (make-box-value node) effect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::box-ref effect::feffect)
   (body-effect! (box-ref-var node) effect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::box-set! effect::feffect)
   (with-access::box-set! node (var value)
      (body-effect! var effect)
      (body-effect! value effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::cast effect::feffect)
   (with-access::cast node (arg)
      (body-effect! arg effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::sequence effect::feffect)
   (body-effect*! (sequence-nodes node) effect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::funcall effect::feffect)
   (merge-effects! effect *effect-top*)
   effect)

;*---------------------------------------------------------------------*/
;*    body-effect! ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::app-ly effect::feffect)
   (merge-effects! effect *effect-top*)
   effect)

;*---------------------------------------------------------------------*/
;*    body-effect! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::app effect)
   (with-access::app node (fun args)
      (body-effect*! args effect)
      (let ((ef (fun-effect! (var-variable fun))))
      (merge-effects! effect ef))
      effect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::extern effect)
   (let ((ef (if (feffect? (extern-effect node))
		 (extern-effect node)
		 *effect-top*)))
      (merge-effects! effect ef)
      effect))

;*---------------------------------------------------------------------*/
;*    body-effect! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::let-fun effect::feffect)
   (with-access::let-fun node (body locals)
      (for-each fun-effect! locals)
      (body-effect! body effect)))

;*---------------------------------------------------------------------*/
;*    body-effect! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (body-effect! node::let-var effect::feffect)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (body-effect! (cdr binding) effect))
		bindings)
      (body-effect! body effect)))

;*---------------------------------------------------------------------*/
;*    body-effect*! ...                                                */
;*---------------------------------------------------------------------*/
(define (body-effect*! node* effect)
   (for-each (lambda (n) (body-effect! n effect)) node*)
   effect)

;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/pair.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 30 08:11:10 2011                          */
;*    Last change :  Sat Aug 25 06:33:19 2012 (serrano)                */
;*    Copyright   :  2011-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The pair approximation manager                                   */
;*    -------------------------------------------------------------    */
;*    Pair are approximated by two values, the car and the cdr.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_pair
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_env
	    engine_param
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_loose
	    cfa_iterate
	    cfa_cfa
	    cfa_setup
	    cfa_approx
	    cfa_tvector)
   (export  (patch-pair-set!)
	    (unpatch-pair-set!)
	    (pair-optim?::bool)
	    (pair-optim-quote-maxlen)))

;*---------------------------------------------------------------------*/
;*    pair-optim? ...                                                  */
;*---------------------------------------------------------------------*/
(define (pair-optim?)
   (and (>=fx *optim* 2) *optim-cfa-pair?*))

;*---------------------------------------------------------------------*/
;*    pair-optim-quote-maxlen ...                                      */
;*---------------------------------------------------------------------*/
(define (pair-optim-quote-maxlen)
   *optim-cfa-pair-quote-max-length*)

;*---------------------------------------------------------------------*/
;*    patch-pair-set! ...                                              */
;*    -------------------------------------------------------------    */
;*    This function is called by (@ compiler engine) at the very       */
;*    beginning of the compilation (just after the heap restoration).  */
;*---------------------------------------------------------------------*/
(define (patch-pair-set!)
   ;; add some extra information about the pair library functions
   ;; when the cfa traces values inside pairs
   (when (pair-optim?)
      (for-each (lambda (id)
		   (let* ((g (find-global/module (cadr id) (caddr id)))
			  (fun (global-value g)))
		      (fun-top?-set! (global-value g) #f)
		      (if (cfun? fun)
			  (let ((args (cfun-args-type fun)))
			     (set-car! args (get-default-type))
			     (set-car! (cdr args) (get-default-type)))
			  (let ((args (sfun-args fun)))
			     (local-type-set! (car args) (get-default-type))
			     (local-type-set! (cadr args) (get-default-type))))))
	 '((@ cons __r4_pairs_and_lists_6_3)
	   (@ set-car! __r4_pairs_and_lists_6_3)
	   (@ set-cdr! __r4_pairs_and_lists_6_3)
	   (@ $cons foreign)
	   (@ $set-car! foreign)
	   (@ $set-cdr! foreign)))
      (let ((g (find-global '$pair?)))
	 (if (global? g)
	     (let ((f (global-value g)))
		(set-car! (cfun-args-type f) (get-default-type)))))))
	
;*---------------------------------------------------------------------*/
;*    unpatch-pair-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (unpatch-pair-set!)
   (when (pair-optim?)
      (for-each (lambda (id)
		   (let* ((g (find-global/module (cadr id) (caddr id)))
			  (fun (global-value g)))
		      (if (cfun? fun)
			  (let ((args (cfun-args-type fun)))
			     (set-car! args *obj*)
			     (set-car! (cdr args) *obj*))
			  (let ((args (sfun-args fun)))
			     (local-type-set! (car args) *obj*)
			     (local-type-set! (cadr args) *obj*)))))
	 '((@ cons __r4_pairs_and_lists_6_3)
	   (@ set-car! __r4_pairs_and_lists_6_3)
	   (@ set-cdr! __r4_pairs_and_lists_6_3)
	   (@ $cons foreign)
	   (@ $set-car! foreign)
	   (@ $set-cdr! foreign)))
      (let ((g (find-global '$pair?)))
	 (if (global? g)
	     (let ((f (global-value g)))
		(set-car! (cfun-args-type f) *obj*))))
      (let ((g (find-global 'pair?)))
	 (if (global? g)
	     (let ((f (global-value g)))
		(local-type-set! (car (sfun-args f)) *obj*)))))
   #unspecified)
    
;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-app ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-app)
   (with-access::pre-cons-app node (fun args)
      (node-setup*! args)
      (let* ((owner (pre-cons-app-owner node))
	     (node  (shrink! node))
	     (wnode (widen!::cons-app node
		       (owner owner)
		       (approxes (cons (make-empty-approx) (make-empty-approx)))
		       (approx (make-empty-approx)))))
	 (trace (cfa 3) "    setup cons-app: " (shape node) #\Newline)
	 (cons-app-approx-set! wnode (make-type-alloc-approx *pair* node)))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-ref-app ...                               */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-ref-app)
   (trace (cfa 3) "    setup cons-ref: " (shape node) #\Newline)
   (with-access::pre-cons-ref-app node (fun args get)
      (node-setup*! args)
      (let* ((cget get)
	     (node (shrink! node)))
	 (widen!::cons-ref-app node
	    (get cget)
	    (approx (make-empty-approx))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-cons-set!-app ...                              */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-cons-set!-app)
   (with-access::pre-cons-set!-app node (fun args get)
      (node-setup*! args)
      (let* ((cget get)
	     (node (shrink! node)))
	 (widen!::cons-set!-app node
	    (get cget)
	    (approx (make-type-approx *unspec*))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-app)
   (with-access::cons-app node (args approxes approx)
      (trace (cfa 4) (cfa-current) ": >>> cons " (shape node) #\Newline)
      (let ((cara (cfa! (car args)))
	    (cdra (cfa! (cadr args))))
	 (union-approx! (car approxes) cara)
	 (union-approx! (cdr approxes) cdra)
	 (trace (cfa 4) (cfa-current) ": ~~~ cons, car="
	    (shape (car approxes))
	    " cdr=" (shape (cdr approxes)) "\n")
	 (trace (cfa 4) (cfa-current) ": ~~~ cons, -> car-approx="
	    (shape (car approxes))
	    " -> cdr-approx=" (shape (cdr approxes)) "\n")
	 (approx-set-type! (car approxes)
	    (get-bigloo-type (approx-type (car approxes))))
	 (approx-set-type! (cdr approxes)
	    (get-bigloo-type (approx-type (cdr approxes))))
	 (trace (cfa 4) (cfa-current) ": <<< cons " (shape node)
	    #\Newline)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-ref-app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-ref-app)
   (with-access::cons-ref-app node (args approx get)
      (let ((cons-approx (cfa! (car args))))
	 (trace (cfa 4) (cfa-current) ": >>> cons-ref: " (shape node)
	    " cons-approx=" (shape cons-approx)
	    " current=" (shape approx)
	    #\Newline)
	 ;; we check for top
	 (when (approx-top? cons-approx)
	    (approx-set-type! approx *obj*)
	    (approx-set-top! approx))
	 ;; then we scan the allocations.
	 (for-each-approx-alloc
	    (lambda (app)
	       (when (cons-app? app)
		  (with-access::cons-app app (approxes seen?)
		     (set! seen? #t)
		     (trace (cfa 4) (cfa-current) 
			": ~~~ cons-app=" (shape (get approxes)) #\Newline)
		     (union-approx! approx (get approxes))
		     (approx-set-type! (get approxes) (approx-type approx)))))
	    cons-approx))
      (trace (cfa 4) (cfa-current) ": <<< cons-ref: " (shape node) " "
	 " <- " (shape approx) #\Newline)
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::cons-set!-app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::cons-set!-app)
   (with-access::cons-set!-app node (args approx get)
      (let ((cons-approx (cfa! (car args)))
	    (val-approx (cfa! (cadr args))))
	 (trace (cfa 4) (cfa-current) ": >>> cons-set!: "
	    (shape node) " "
	    (shape cons-approx) #\Newline)
	 ;; we check the type...
	 (unless (eq? (approx-type cons-approx) *pair*)
	    (approx-set-type! val-approx *obj*))
	 ;; we check if we have top on the pair
	 (if (approx-top? cons-approx)
	     ;; yes, we have, hence we loose every thing.
	     (loose! val-approx 'all)
	     ;; no, then we scan the allocations.
	     (for-each-approx-alloc
		(lambda (app)
		   (when (cons-app? app)
		      (with-access::cons-app app (approxes seen?)
			 (set! seen? #t)
			 (union-approx! (get approxes) val-approx))))
		cons-approx)))
      (trace (cfa 4) (cfa-current) ": <<< cons-set!: " (shape node) " "
	 (shape approx) #\Newline)
      approx))
      
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    Loosing a pair means that cons-ref now returns `obj' and         */
;*    `top' and that all contained allocation are also lost but        */
;*    it does not change anything on the allocation itself.            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::cons-app)
   (with-access::cons-app alloc (lost-stamp approxes)
      (unless (=fx lost-stamp *cfa-stamp*)
	 (trace (cfa 2) (cfa-current) ": *** loose(cons): "
	    (shape alloc) #\Newline)
	 (set! lost-stamp *cfa-stamp*)
	 (for-each-approx-alloc loose-alloc! (car approxes))
	 (for-each-approx-alloc loose-alloc! (cdr approxes))
	 (approx-set-type! (car approxes) *obj*)
	 (approx-set-type! (cdr approxes) *obj*)
	 (approx-set-top! (car approxes))
	 (approx-set-top! (cdr approxes)))))

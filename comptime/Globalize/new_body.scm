;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/new_body.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 30 09:25:08 1995                          */
;*    Last change :  Fri Apr 21 18:45:33 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    For each globalized function, we set its new body.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_new-body
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_sexp
	    ast_dump
	    globalize_ginfo
	    globalize_node)
   (export  (set-globalized-new-bodies! ::global local*)))

;*---------------------------------------------------------------------*/
;*    set-globalized-new-bodies! ...                                   */
;*---------------------------------------------------------------------*/
(define (set-globalized-new-bodies! global locals)
   (trace (globalize 2) #\Newline "set-globalized-new-bodies!: " (shape global)
      #\Newline)
   (set! *round* (+fx *round* 1))
   ;; remove globalized or integrated functions from global
   (let ((fun (global-value global)))
      (sfun-body-set! fun (rem! (sfun-body fun) global global)))
   ;; then compute the globalized functions bodies
   (for-each set-globalized-new-body! locals))

;*---------------------------------------------------------------------*/
;*    set-globalized-new-body! ...                                     */
;*---------------------------------------------------------------------*/
(define (set-globalized-new-body! local)
   (trace (globalize 2) "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	  #\Newline 
	  "set-globalized-new-body!: " (shape local) " integrated="
	  (shape (sfun/Ginfo-integrated (local-value local)))
	  #\Newline)
   (set! *round* (+fx *round* 1))
   ;; we remove globalized or integrated functions from local
   (let* ((fun       (local-value local))
	  (info      (local-value local))
	  (old-body  (sfun-body fun))
	  (obindings (filter (lambda (g)
				(not (eq? (sfun/Ginfo-owner (local-value g))
					  local)))
			     (sfun/Ginfo-integrated info)))
	  (new-body  (rem! old-body local local)))
      ;; then, we scan all the integrated functions in order to
      ;; remove nested functions in added functions
      ;; (same problem as in integrate_let-fun)
      (for-each (lambda (f)
		   (trace (globalize 2) "scanning: " (shape f) #\Newline)
		   (if (not (=fx (sfun/Ginfo-bmark (local-value f)) *round*))
		       (let* ((fun  (local-value f))
			      (body (sfun-body fun)))
			  (sfun-body-set! fun (rem! body local f)))))
		obindings)
      ;; and we peek really integrated functions.
      (let ((nbindings (let loop ((nbdings '())
				  (obdings obindings))
			  (cond
			     ((null? obdings)
			      nbdings)
			     ((=fx (sfun/Ginfo-bmark (local-value
						      (car obdings)))
				   *round*)
			      ;; this function _is_ already in local
			      (loop nbdings (cdr obdings)))
			     (else
			      (loop (cons (car obdings) nbdings)
				    (cdr obdings)))))))
	 (let ((new2-body  (if (null? nbindings)
			       new-body
			       (instantiate::let-fun
				  (loc (node-loc old-body))
				  (type (node-type old-body))
				  (locals nbindings)
				  (body new-body)))))
	    ;; during the computation of the captured variable, we will compute
	    ;; the set of free variables from the new-body but, we have to
	    ;; ajust the field `cto' of local.
	    (let loop ((ncto (sfun/Ginfo-cto info))
		       (nbindings nbindings))
	       (if (null? nbindings)
		   (sfun/Ginfo-cto-set! info ncto)
		   (let liip ((ncto ncto)
			      (lcto (sfun/Ginfo-cto (local-value
						    (car nbindings)))))
		      (cond
			 ((null? lcto)
			  (loop ncto (cdr nbindings)))
			 ((memq (car lcto) ncto)
			  (liip ncto (cdr lcto)))
			 (else
			  (liip (cons (car lcto) ncto) (cdr lcto)))))))
	    (trace (globalize 2) "new-body( " (shape local) " ): "
		   (shape new2-body)
		   #\Newline
		   "     obindings: " (shape obindings)
		   #\Newline
		   "     nbindings: " (shape nbindings)
		   #\Newline
		   "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		   #\Newline
		   #\Newline)
	  (sfun/Ginfo-new-body-set! info (rem! new2-body local local))))))

;*---------------------------------------------------------------------*/
;*    is-in? ...                                                       */
;*    -------------------------------------------------------------    */
;*    Is f1 in the body of f2 ?                                        */
;*---------------------------------------------------------------------*/
(define (is-in? f1 f2)
   (let ((info (local-value f1)))
      (cond
	 ((sfun/Ginfo-G? info)
	  #f)
	 ((not (local? (sfun/Ginfo-integrator info)))
	  #t)
	 ((eq? (sfun/Ginfo-integrator info) f2)
	  #t)
	 ((is-in? (sfun/Ginfo-integrator info) f2)
	  #t)
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    *round* ...                                                      */
;*---------------------------------------------------------------------*/
(define *round* 0)

;*---------------------------------------------------------------------*/
;*    rem! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (rem!::node node::node owner current))

;*---------------------------------------------------------------------*/
;*    rem! ::atom ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (rem! node::atom owner current)
   node)

;*---------------------------------------------------------------------*/
;*    rem! ::kwote ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (rem! node::kwote owner current)
   node)

;*---------------------------------------------------------------------*/
;*    rem! ::var ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (rem! node::var owner current)
   node)

;*---------------------------------------------------------------------*/
;*    rem! ::sequence ...                                              */
;*---------------------------------------------------------------------*/
(define-method (rem! node::sequence owner current)
   (with-access::sequence node (nodes)
      (rem*! nodes owner current)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::sync ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (rem! node::sync owner current)
   (with-access::sync node (body mutex prelock)
      (set! mutex (rem! mutex owner current))
      (set! prelock (rem! prelock owner current))
      (set! body (rem! body owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (rem! node::app owner current)
   (with-access::app node (args)
      (rem*! args owner current)
      node))
	  
;*---------------------------------------------------------------------*/
;*    rem! ::app-ly ...                                                */
;*---------------------------------------------------------------------*/
(define-method (rem! node::app-ly owner current)
   (with-access::app-ly node (fun arg)
      (set! fun (rem! fun owner current))
      (set! arg (rem! arg owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::funcall ...                                               */
;*---------------------------------------------------------------------*/
(define-method (rem! node::funcall owner current)
   (with-access::funcall node (fun args)
      (set! fun (rem! fun owner current))
      (rem*! args owner current)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::extern ...                                                */
;*---------------------------------------------------------------------*/
(define-method (rem! node::extern owner current)
   (with-access::extern node (expr*)
      (rem*! expr* owner current)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::cast ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (rem! node::cast owner current)
   (with-access::cast node (arg)
      (rem! arg owner current)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::setq ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (rem! node::setq owner current)
   (with-access::setq node (value)
      (set! value (rem! value owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::conditional ...                                           */
;*---------------------------------------------------------------------*/
(define-method (rem! node::conditional owner current)
   (with-access::conditional node (test true false)
      (set! test (rem! test owner current))
      (set! true (rem! true owner current))
      (set! false (rem! false owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::fail ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (rem! node::fail owner current)
   (with-access::fail node (proc msg obj)
      (set! proc (rem! proc owner current))
      (set! msg (rem! msg owner current))
      (set! obj (rem! obj owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::switch ...                                                */
;*---------------------------------------------------------------------*/
(define-method (rem! node::switch owner current)
   (with-access::switch node (clauses test)
      (set! test (rem! test owner current))
      (for-each (lambda (clause)
		   (set-cdr! clause (rem! (cdr clause) owner current)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::let-fun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (rem! node::let-fun owner current)
   (with-access::let-fun node (body locals)
      (set! body (rem! body owner current))
      (let liip ((obindings locals)
		 (nbindings '()))
	 (cond
	    ((null? obindings)
	     (set! locals nbindings)
	     node)
	    ((=fx (sfun/Ginfo-bmark (local-value (car obindings))) *round*)
	     (trace (globalize 2) " ### already: "
		    (shape (car obindings)) #\Newline
		    "    plugged: " (shape (sfun/Ginfo-plugged-in
					    (local-value (car obindings))))
		    "    current: " (shape current)
		    #\Newline)
	     (if (eq? current
		      (sfun/Ginfo-plugged-in (local-value (car obindings))))
		 ;; ok, we keep this function
		 (liip (cdr obindings) (cons (car obindings) nbindings))
		 ;; this function is already in host
		 (liip (cdr obindings)
		       nbindings)))
	    ((is-in? (car obindings) owner)
	     (trace (globalize 2) " ### is-in: "
		    (shape (car obindings))
		    "  [current: " (shape current) "]"
		    #\Newline)
	     ;; we mark that the function is defined
	     (sfun/Ginfo-bmark-set! (local-value (car obindings)) *round*)
	     ;; we plug the function
	     (sfun/Ginfo-plugged-in-set! (local-value (car obindings)) current)
	     (let* ((fun (local-value (car obindings)))
		    (bod (sfun-body fun)))
		(sfun-body-set! fun (rem! bod owner (car obindings)))
		(liip (cdr obindings)
		      (cons (car obindings) nbindings))))
	    ((local/Ginfo-escape? (car obindings))
	     (trace (globalize 2) " ### escaping: " 
		    (shape (car obindings)) #\Newline)
	     ;; we don't remove now the escaping functions
	     (liip (cdr obindings)
		   (cons (car obindings) nbindings)))
	    (else
	     (trace (globalize 2) " ### removing: "
		    (shape (car obindings)) " because not escaping and not in "
		    (shape owner) #\Newline)
	     (liip (cdr obindings)
		   nbindings))))))

;*---------------------------------------------------------------------*/
;*    rem! ::let-var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (rem! node::let-var owner current)
   (with-access::let-var node (body bindings)
      (set! body (rem! body owner current))
      (for-each (lambda (binding)
		   (set-cdr! binding (rem! (cdr binding) owner current)))
		bindings)
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::set-ex-it ...                                             */
;*---------------------------------------------------------------------*/
(define-method (rem! node::set-ex-it owner current)
   (with-access::set-ex-it node (body)
      (set! body (rem! body owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::jump-ex-it ...                                            */
;*---------------------------------------------------------------------*/
(define-method (rem! node::jump-ex-it owner current)
   (with-access::jump-ex-it node (exit value)
      (set! exit (rem! exit owner current))
      (set! value (rem! value owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::make-box ...                                              */
;*---------------------------------------------------------------------*/
(define-method (rem! node::make-box owner current)
   (with-access::make-box node (value)
      (set! value (rem! value owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::box-ref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (rem! node::box-ref owner current)
   (with-access::box-ref node (var)
      (set! var (rem! var owner current))
      node))

;*---------------------------------------------------------------------*/
;*    rem! ::box-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (rem! node::box-set! owner current)
   (with-access::box-set! node (var value)
      (set! var (rem! var owner current))
      (set! value (rem! value owner current))
      node))
	    
;*---------------------------------------------------------------------*/
;*    rem*! ...                                                        */
;*---------------------------------------------------------------------*/
(define (rem*! node* owner current)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (rem! (car node*) owner current))
	     (loop (cdr node*))))))



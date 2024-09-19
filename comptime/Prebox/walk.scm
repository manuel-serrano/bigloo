;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Prebox/walk.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Thu Sep 19 10:43:45 2024 (serrano)                */
;*    Copyright   :  2010-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Optimize box/unbox operations. Useful mostly for non C backends. */
;*    Typically, replaces:                                             */
;*                                                                     */
;*        (let ((arg1253::bool                                         */
;*                  (if (let ((vector::vector node::obj))              */
;*                        ($obj->bool (vref vector::vector 6)))        */
;*                    #f                                               */
;*                    #t)))                                            */
;*            (let ((vector::vector node::obj))                        */
;*              (vset! vector::vector 6                                */
;*                     (|$bool->bbool::bbool[::_]|                     */
;*                       arg1253::bool))))                             */
;*                                                                     */
;*    with                                                             */
;*                                                                     */
;*        (let ((arg1253::bbool                                        */
;*                  (if (let ((vector::vector node::obj))              */
;*                        ($obj->bool (vref vector::vector 6)))        */
;*                    (bool->bbool #f)                                 */
;*                    (bool->bbool #t))))                              */
;*            (let ((vector::vector node::obj))                        */
;*              (vset! vector::vector 6 arg1253::bbool)))              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module prebox_walk
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
   (export  (prebox-walk! globals)
	    (init-prebox-cache!)))

;*---------------------------------------------------------------------*/
;*    prebox-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (prebox-walk! globals)
   (pass-prelude "prebox" init-prebox-cache!) 
   (for-each prebox-fun! globals)
   (pass-postlude globals clear-prebox-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *boxed-types* '())

;*---------------------------------------------------------------------*/
;*    boxinfo ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct boxinfo utype btype unbox box)

;*---------------------------------------------------------------------*/
;*    varinfo ...                                                      */
;*---------------------------------------------------------------------*/
(define-struct varinfo var binding boxinfo nodes boxedinit)

;*---------------------------------------------------------------------*/
;*    boxinfo-get ...                                                  */
;*---------------------------------------------------------------------*/
(define (boxinfo-get ty::type)
   (let ((c (assq ty *boxed-types*)))
      (when (pair? c)
	 (cdr c))))

;*---------------------------------------------------------------------*/
;*    init-prebox-cache! ...                                           */
;*---------------------------------------------------------------------*/
(define (init-prebox-cache!)
   (unless (pair? *boxed-types*)
      (set! *boxed-types*
	 (list
	    (cons *bool*
	       (boxinfo *bool* *bbool*
		  (find-global '$bbool->bool 'foreign)
		  (find-global '$bool->bbool 'foreign)))
	    (cons *long*
	       (boxinfo *long* *bint*
		  (find-global '$bint->long 'foreign)
		  (find-global '$long->bint 'foreign)))
	    (cons *char*
	       (boxinfo *char* *bchar*
		  (find-global '$bchar->char 'foreign)
		  (find-global '$char->bchar 'foreign))))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-prebox-cache! ...                                          */
;*---------------------------------------------------------------------*/
(define (clear-prebox-cache!)
   (set! *boxed-types* '()))

;*---------------------------------------------------------------------*/
;*    prebox-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (prebox-fun! var)
   (with-trace 'prebox "prebox-fun"
      (trace-item "fun: " (shape var))
      (enter-function (variable-id var))
      (let ((fun (variable-value var)))
	 (sfun-body-set! fun (prebox! (sfun-body fun)))
	 (leave-function)
	 var)))

;*---------------------------------------------------------------------*/
;*    prebox! ...                                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (prebox! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    prebox! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (prebox! node::let-var)
   (with-access::let-var node (bindings body type)
      (for-each (lambda (b)
		   (set-cdr! b (prebox! (cdr b))))
	 bindings)
      (prebox! body)
      (let ((vis (filter-map (lambda (b)
				(let ((var (car b)))
				   (when (eq? (variable-access var) 'read)
				      (let ((bi (boxinfo-get (variable-type var))))
					 (when bi
					    (varinfo var b bi '() #f))))))
		    bindings)))
	 (if (null? vis)
	     node
	     (let ((bxs (find-boxings body vis)))
		(with-trace 'prebox "prebox! ::let-var"
		   (trace-item (map (lambda (v) (shape (varinfo-var v))) bxs))
		   (if (null? bxs)
		       node)
		      (let ((pxs (filter-map prebox-expr bxs)))
			 (if (pair? pxs)
			     (let ((nodes '()))
				(trace-item "pxs: " (length pxs))
				(for-each (lambda (px)
					     ;; modify the variable declaration
					     (variable-type-set!
						(varinfo-var px)
						(boxinfo-btype (varinfo-boxinfo px)))
					     (set-cdr! (varinfo-binding px)
						(varinfo-boxedinit px))
					     (set! nodes
						(append (varinfo-nodes px)
						   nodes)))
				   pxs)
				;; removing all boxing operations of all
				;; the newly boxed variables
				;; (tprint "**** APPLY")
				(set! body (unbox-node! body nodes))
				node)
			     node))))))))

;*---------------------------------------------------------------------*/
;*    find-boxings ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-boxings::pair-nil body::node vis::pair)
   (find-usages body vis)
   (filter (lambda (vi)
	      (let ((box (boxinfo-box (varinfo-boxinfo vi))))
		 (every (lambda (n)
			   (when (isa? n app)
			      (with-access::app n (fun args)
				 (with-access::var fun (variable)
				    (eq? variable box)))))
		    (varinfo-nodes vi))))
      vis))
   
;*---------------------------------------------------------------------*/
;*    find-usages ...                                                  */
;*    -------------------------------------------------------------    */
;*    Find all the node the unbox variables are involved.              */
;*---------------------------------------------------------------------*/
(define-walk-method (find-usages node::node vis::pair)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    find-usages ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (find-usages node::var vis::pair)
   (with-access::var node (variable)
      (let ((vi (find-varinfo variable vis)))
	 (when vi
	    (varinfo-nodes-set! vi (cons node (varinfo-nodes vi)))))))

;*---------------------------------------------------------------------*/
;*    find-usages ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (find-usages node::app vis::pair)
   (with-access::app node (fun args)
      (for-each (lambda (a)
		   (if (isa? a var)
		       (with-access::var a (variable)
			  (let ((vi (find-varinfo variable vis)))
			     (when vi
				(unless (memq node (varinfo-nodes vi))
				   (varinfo-nodes-set! vi
				      (cons node (varinfo-nodes vi)))))))
		       (find-usages a vis)))
	 args)))

;*---------------------------------------------------------------------*/
;*    find-varinfo ...                                                 */
;*---------------------------------------------------------------------*/
(define (find-varinfo variable::variable vis::pair)
   (find (lambda (vi) (eq? (varinfo-var vi) variable)) vis))

;*---------------------------------------------------------------------*/
;*    prebox-expr ...                                                  */
;*---------------------------------------------------------------------*/
(define (prebox-expr vi)
   (with-trace 'pregox "prebox-expr"
      (let* ((bi (varinfo-boxinfo vi))
	     (node (cdr (varinfo-binding vi)))
	     (pnode (prebox-node node bi)))
	 (unless (eq? node pnode)
	    (varinfo-boxedinit-set! vi pnode)
	    vi))))

;*---------------------------------------------------------------------*/
;*    prebox-walk-node ::node ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (prebox-node node::node bi::struct)
   node)

;*---------------------------------------------------------------------*/
;*    prebox-node ::literal ...                                        */
;*---------------------------------------------------------------------*/
(define-method (prebox-node node::literal bi::struct)
   (with-access::literal node (value type)
      (if (eq? type (boxinfo-utype bi))
	  (with-access::node node (loc)
	     (instantiate::app
		(loc loc)
		(type (boxinfo-btype bi))
		(fun (instantiate::ref
			(loc loc)
			(type (variable-type (boxinfo-box bi)))
			(variable (boxinfo-box bi))))
		(args (list node))))
	  node)))
   
;*---------------------------------------------------------------------*/
;*    prebox-node ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (prebox-node node::app bi::struct)
   (with-access::app node (fun args)
      (with-access::var fun (variable)
	 (if (eq? variable (boxinfo-unbox bi))
	     (car args)
	     node))))

;*---------------------------------------------------------------------*/
;*    prebox-node ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (prebox-node node::sequence bi::struct)
   (with-access::sequence node (nodes)
      (let* ((n (car (last-pair nodes)))
	     (p (prebox-node n bi)))
	 (if (eq? n p)
	     node
	     (duplicate::sequence node
		(type (boxinfo-btype bi))
		(nodes (reverse (cons p (cdr (reverse nodes))))))))))

;*---------------------------------------------------------------------*/
;*    prebox-node ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (prebox-node node::conditional bi::struct)
   (with-access::conditional node (test true false)
      (let ((ntrue (prebox-node true bi))
	    (nfalse (prebox-node false bi)))
	 (if (or (eq? true ntrue) (eq? false nfalse))
	     node
	     (duplicate::conditional node
		(type (boxinfo-btype bi))
		(true ntrue)
		(false nfalse))))))
   
;*---------------------------------------------------------------------*/
;*    unbox-node! ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (unbox-node! node::node nodes)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    unbox-node! ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (unbox-node! node::app nodes)
   (if (memq node nodes)
       (with-access::app node (args)
	 (let ((v (car args)))
	    (duplicate::ref (car args) 
	       (type (variable-type (var-variable v))))))
       (call-default-walker)))
	    
			 

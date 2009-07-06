;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/typeof.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:25:05 1996                          */
;*    Last change :  Thu Apr 22 11:11:51 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type of the things                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_typeof
   (include "Tvector/tvector.sch")
   (import  type_type
	    type_cache
	    ast_node
	    ast_var
	    tools_shape)
   (export  (typeof-atom::type  <atom>)
	    (typeof-kwote::type <kwote>)
	    (generic typeof::type ::node)))

;*---------------------------------------------------------------------*/
;*    typeof-atom ...                                                  */
;*---------------------------------------------------------------------*/
(define (typeof-atom atom)
   (cond
      ((null? atom)
       *bnil*)
      ((fixnum? atom)
       *long*)
      ((real? atom)
       *real*)
      ((boolean? atom)
       *bool*)
      ((char? atom)
       *char*)
      ((string? atom)
       *bstring*)
      ((eq? atom #unspecified)
       *unspec*)
      ((elong? atom)
       *elong*)
      ((llong? atom)
       *llong*)
      ((keyword? atom)
       *keyword*)
      (else
       *obj*)))

;*---------------------------------------------------------------------*/
;*    typeof-kwote ...                                                 */
;*---------------------------------------------------------------------*/
(define (typeof-kwote kwote)
   (cond
      ((symbol? kwote)
       *symbol*)
      ((keyword? kwote)
       *keyword*)
      ((pair? kwote)
       *pair*)
      ((null? kwote)
       *bnil*)
      ((vector? kwote)
       *vector*)
      ((a-tvector? kwote)
       (a-tvector-type kwote))
      (else
       *obj*)))

;*---------------------------------------------------------------------*/
;*    typeof ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (typeof::type node::node))

;*---------------------------------------------------------------------*/
;*    typeof ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (typeof node::atom)
   (with-access::atom node (value)
      (typeof-atom value)))
 
;*---------------------------------------------------------------------*/
;*    typeof ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (typeof node::kwote)
   (with-access::kwote node (value)
      (typeof-kwote value)))

;*---------------------------------------------------------------------*/
;*    typeof ...                                                       */
;*---------------------------------------------------------------------*/
(define-method (typeof node::var)
   (with-access::var node (variable)
      (let ((value (variable-value variable)))
	 (cond
	    ((sfun? value)
	     *procedure*)
	    ((cfun? value)
	     (error "typeof" "Not implemented yet" (shape node)))
	    (else
	     (variable-type variable))))))

;*---------------------------------------------------------------------*/
;*    typeof ::closure ...                                             */
;*---------------------------------------------------------------------*/
(define-method (typeof node::closure)
   *procedure*)

;*---------------------------------------------------------------------*/
;*    typeof ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (typeof node::sequence)
   (with-access::sequence node (nodes)
      (typeof (car (last-pair nodes)))))

;*---------------------------------------------------------------------*/
;*    typeof ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (typeof node::extern)
   (with-access::extern node (type)
      type))

;*---------------------------------------------------------------------*/
;*    typeof ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (typeof node::cast)
   (with-access::cast node (type)
      type))

;*---------------------------------------------------------------------*/
;*    typeof ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (typeof node::setq)
   *unspec*)

;*---------------------------------------------------------------------*/
;*    typeof ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (typeof node::conditional)
   (with-access::conditional node (test true false)
      (let ((ttrue (typeof true))
	    (tfalse (typeof false)))
	 (cond ((or (eq? ttrue tfalse) (eq? tfalse *magic*)) ttrue)
	       ((eq? ttrue *magic*) tfalse)
	       (else *obj*)))))

;*---------------------------------------------------------------------*/
;*    typeof ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (typeof node::fail)
   (with-access::fail node (proc msg obj)
      *magic*))

;*---------------------------------------------------------------------*/
;*    typeof ::select ...                                              */
;*---------------------------------------------------------------------*/
(define-method (typeof node::select)
   (with-access::select node (clauses test)
      (let loop ((clauses (cdr clauses))
		 (type    (typeof (cdr (car clauses)))))
	 (if (null? clauses)
	     type
	     (let ((ntype (typeof (cdr (car clauses)))))
		(cond
		   ((eq? type *magic*)
		    (loop (cdr clauses) ntype) )
		   ((or (eq? ntype type) (eq? ntype *magic*))
		    (loop (cdr clauses) type))
		   (else *obj*)))))))

;*---------------------------------------------------------------------*/
;*    typeof ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (typeof node::let-fun)
   (with-access::let-fun node (body)
      (typeof body)))

;*---------------------------------------------------------------------*/
;*    typeof ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (typeof node::let-var)
   (with-access::let-var node (body)
      (typeof body)))
 
;*---------------------------------------------------------------------*/
;*    typeof ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (typeof node::set-ex-it)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (typeof node::jump-ex-it)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (typeof node::make-box)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (typeof node::box-ref)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (typeof node::box-set!)
   *unspec*)

;*---------------------------------------------------------------------*/
;*    typeof ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (typeof node::app-ly)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (typeof node::funcall)
   *obj*)

;*---------------------------------------------------------------------*/
;*    typeof ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (typeof node::app)
   (with-access::app node (fun)
      (variable-type (var-variable fun))))

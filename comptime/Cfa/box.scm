;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/box.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:08:59 1996                          */
;*    Last change :  Sat Jul  7 08:41:12 2001 (serrano)                */
;*    Copyright   :  1996-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The procedure approximation management                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_box
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    engine_param
	    type_type
	    type_cache
	    ast_var 
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_setup
	    cfa_approx
	    cfa_cfa
	    cfa_iterate
	    cfa_closure))
     
;*---------------------------------------------------------------------*/
;*    node-setup! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::make-box)
   (with-access::make-box node (value)
      (node-setup! value)
      (widen!::make-box/Cinfo node
	 (approx (make-type-approx *obj*)))
      (approx-set-top! (make-box/Cinfo-approx node))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-make-box ...                                   */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-make-box)
   (with-access::pre-make-box node (value)
      (node-setup! value)
      (let* ((node (shrink! node))) 
	 (let ((node (widen!::make-box/O-Cinfo node
			(approx (make-type-approx *obj*))
			(value-approx (make-empty-approx)))))
	    (make-box/O-Cinfo-approx-set!
	     node
	     (make-type-alloc-approx *obj* node))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::box-set!)
   (with-access::box-set! node (var value)
      (node-setup! var)
      (node-setup! value)
      (if (>=fx *optim* 1)
	  (widen!::box-set!/O-Cinfo node
	     (approx (make-type-approx *unspec*)))
	  (widen!::box-set!/Cinfo node
	     (approx (make-type-approx *unspec*))))))

;*---------------------------------------------------------------------*/
;*    node-setup! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::box-ref)
   (with-access::box-ref node (var)
      (node-setup! var)
      (if (>=fx *optim* 1)
	  (widen!::box-ref/O-Cinfo node
	     (approx (make-type-approx *obj*)))
	  (begin
	     (widen!::box-ref/Cinfo node
		(approx (make-type-approx *obj*)))
	     (approx-set-top! (box-ref/Cinfo-approx node))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::make-box/O-Cinfo ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::make-box/O-Cinfo)
   (with-access::make-box/O-Cinfo node (value-approx value approx)
      (trace (cfa 3) "cfa!(make-box/O-cinfo): " (shape value) #\Newline)
      (let ((init-value-approx (cfa! value)))
	 (union-approx! value-approx init-value-approx)
	 approx)))

;*---------------------------------------------------------------------*/
;*    cfa! ::box-ref/O-Cinfo ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::box-ref/O-Cinfo)
   (with-access::box-ref/O-Cinfo node (approx var)
      (let ((box-approx (cfa! var)))
	 (for-each-approx-alloc
	  (lambda (box)
	     (cond
		((make-box/O-Cinfo? box)
		 (with-access::make-box/O-Cinfo box (value-approx)
		    (union-approx! approx value-approx)))
		((make-box/Cinfo? box)
		 (internal-error
		  "box-ref"
		  "Illegal mixed of optimized and unoptimize `make-box'"
		  (shape node)))))
	  box-approx))
      approx))

;*---------------------------------------------------------------------*/
;*    cfa! ::box-set!/O-Cinfo ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::box-set!/O-Cinfo)
   (with-access::box-set!/O-Cinfo node (approx var value)
      (let ((box-approx (cfa! var))
	    (val-approx (cfa! value)))
	 ;; we check if we have top on the vector
	 (if (approx-top? box-approx)
	     ;; yes, we have, hence we loose every thing.
	     (loose! val-approx 'all)
	     (for-each-approx-alloc
	      (lambda (box)
		 (cond
		    ((make-box/O-Cinfo? box)
		     (with-access::make-box/O-Cinfo box (value-approx)
			(union-approx! value-approx val-approx)))
		    ((make-box/Cinfo? box)
		     (internal-error
		      "box-ref"
		      "Illegal mixed of optimized and unoptimize `make-box'"
		      (shape node)))))
	      box-approx)))
      approx))
	     
;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*    -------------------------------------------------------------    */
;*    loosing a box does not make anything because make-box are not    */
;*    first object citizen.                                            */
;*---------------------------------------------------------------------*/
(define-method (loose-alloc! alloc::make-box)
   #unspecified)

   



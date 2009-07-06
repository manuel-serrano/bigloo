;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/cinfo3.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Feb 25 13:50:29 1999                          */
;*    Last change :  Fri Mar  5 16:29:25 2004 (serrano)                */
;*    Copyright   :  1999-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The format cfa_info used is to big. Its compilation was          */
;*    requiring to many memory. I have simply slit that module.        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_info3

   (import type_type
	   type_cache
	   ast_var
	   ast_node
	   cfa_info)

   (export ;; extern extension
           (wide-class pragma/Cinfo::pragma (approx::approx read-only))
	   (wide-class getfield/Cinfo::getfield (approx::approx read-only))
	   (wide-class setfield/Cinfo::setfield (approx::approx read-only))
	   (wide-class new/Cinfo::new (approx::approx read-only))
	   (wide-class isa/Cinfo::isa (approx::approx read-only))
	   (wide-class cast-null/Cinfo::cast-null (approx::approx read-only))

	   ;; vector management
	   (wide-class vref/Cinfo::vref
	      (approx::approx read-only)
	      (tvector?::bool (default #f)))
	   (wide-class vset!/Cinfo::vset!
	      (approx::approx read-only)
	      (tvector?::bool (default #f)))
	   (wide-class vlength/Cinfo::vlength
	      (approx::approx read-only)
	      (tvector?::bool (default #f)))

	   ;; vector creation handling
	   (wide-class pre-valloc/Cinfo::valloc
	      (owner::variable read-only))
	   (wide-class valloc/Cinfo::valloc
	      (approx::approx read-only))
	   (wide-class valloc/Cinfo+optim::valloc
	      ;; the approx of the make-vector (i.e. *vector*)
              approx::approx
              ;; the approximation of the values holded by the vector
              (value-approx::approx read-only)
              ;; a stamp to avoid infinit loop when loosing a vector
              (lost-stamp::long (default -1))
              ;; an allocation owner
              (owner::variable read-only)
              ;; can we stack allocate this vector (to be use in conjonction
              ;; with lost-stamp).
              (stackable?::bool (default #t))
              ;; a stamp use for the stack loosing propagation
              (stack-stamp (default '()))
              ;; Is the vector subject to a vector-ref or a vector-set?
              ;; If not, this vector cannot be optimized
              (seen?::bool (default #f)))))

		       
	   

   

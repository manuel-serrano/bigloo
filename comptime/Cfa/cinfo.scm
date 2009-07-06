;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/cinfo.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 24 15:46:49 1996                          */
;*    Last change :  Fri Nov  5 16:08:22 2004 (serrano)                */
;*    Copyright   :  1996-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The cfa's information structures                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_info
   
   (import type_type
	   type_cache
	   ast_var
	   ast_node)
   
   (export ;; the approximations
    (class approx
       ;; the type of the approximation. Just one type, we
       ;; do not compute several types approximation because
       ;; anything receiving more than one type is of type obj.
       (type::type (default *_*))
       ;; A type locked means that the type field can't be changed
       ;; (e.g. because the type has been set by the user, as for
       ;; variable).
       (type-locked?::bool read-only (default #f))
       ;; its allocations
       (allocs read-only)
       ;; or top
       (top?::bool (default #f))
       ;; a stamp to avoid useless multiple loose.
       (lost-stamp::long (default -1)))
    
    ;; function extensions
    (wide-class cfun/Cinfo::cfun
       (approx::approx read-only))
    (wide-class extern-sfun/Cinfo::sfun
       (approx::approx read-only))
    (wide-class intern-sfun/Cinfo::sfun
       (approx::approx read-only)
       (stamp::long (default -1)))
    
    ;; cnst extension
    (wide-class scnst/Cinfo::scnst
       (approx::approx read-only))
    
    ;; var extensions
    (wide-class pre-clo-env::svar)
    (wide-class svar/Cinfo::svar
       (approx::approx read-only)
       ;; is this variable holding a closure environement
       ;; (if it is it won't be lost when loosing the
       ;; function whose's this variable owner).
       (clo-env?::bool (default #f)))
    (wide-class cvar/Cinfo::cvar
       (approx::approx read-only))
    
    ;; exit extensions
    (wide-class sexit/Cinfo::sexit
       (approx::approx read-only))
    
    ;; global and local reshaping
    (wide-class reshaped-local::local
       (binding-value read-only (default #f)))
    (wide-class reshaped-global::global)
    
    ;; node extension
    (wide-class atom/Cinfo::atom
       (approx::approx read-only))
    (wide-class kwote/node::kwote
       (node::node read-only))
    (wide-class kwote/Cinfo::kwote
       (approx::approx read-only))
    (wide-class app-ly/Cinfo::app-ly
       (approx::approx read-only))
    (wide-class funcall/Cinfo::funcall
       (approx::approx read-only)
       (va-approx::approx read-only)
       (arity-error-noticed?::bool (default (=fx (bigloo-warning) 0)))
       (type-error-noticed?::bool (default (=fx (bigloo-warning) 0))))
    (wide-class setq/Cinfo::setq
       (approx::approx read-only))
    (wide-class conditional/Cinfo::conditional
       (approx::approx read-only))
    (wide-class fail/Cinfo::fail
       (approx::approx read-only))
    (wide-class select/Cinfo::select
       (approx::approx read-only))
    (wide-class set-ex-it/Cinfo::set-ex-it
       (approx::approx read-only))
    (wide-class jump-ex-it/Cinfo::jump-ex-it
       (approx::approx read-only))
    
    ;; boxes
    (wide-class pre-make-box::make-box)
    (wide-class make-box/Cinfo::make-box
       (approx::approx read-only))
    (wide-class make-box/O-Cinfo::make-box
       approx::approx
       (value-approx::approx read-only))
    (wide-class box-set!/Cinfo::box-set!
       (approx::approx read-only))
    (wide-class box-ref/Cinfo::box-ref
       (approx::approx read-only))
    (wide-class box-set!/O-Cinfo::box-set!
       (approx::approx read-only))
    (wide-class box-ref/O-Cinfo::box-ref
       (approx::approx read-only))))
   

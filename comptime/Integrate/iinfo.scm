;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Integrate/iinfo.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 28 14:06:46 1996                          */
;*    Last change :  Wed Oct 13 11:28:44 2021 (serrano)                */
;*    Copyright   :  1996-2021 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The extension for the integrate pass.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_info
   
   (import  type_type
	    ast_var
	    ast_node)

   (include "Integrate/iinfo.sch")
   
   (export (wide-class svar/Iinfo::svar
	      ;; a stamp to compute free variables
	      (f-mark (default #unspecified))
	      ;; a stamp to compute union of variables
	      (u-mark (default #unspecified))
	      ;; is a local kaptured ?
	      (kaptured?::bool (default #f))
	      ;; celled?
	      (celled?::bool (default #f))
	      ;; set-exit handler
	      (xhdl::obj (default #f)))

	   (wide-class sexit/Iinfo::sexit
	      ;; a stamp to compute free variables
	      (f-mark (default #unspecified))
	      ;; a stamp to compute union of variables
	      (u-mark (default #unspecified))
	      ;; is a local kaptured ?
	      (kaptured?::bool (default #f))
	      ;; celled?
	      (celled?::bool (default #f)))

	   (wide-class sfun/Iinfo::sfun
	      ;; the variable which bound the local function
	      owner
	      ;; the free variables
	      (free (default #unspecified))
	      ;; the bound variables list
	      (bound (default '()))
	      ;; the list of variables which calls the fun
	      (cfrom (default '()))
	      ;; the list of variables which are called.
	      (cto (default #unspecified))
	      ;; the K property
	      (K (default '()))
	      ;; the K* property
	      (K* (default '()))
	      ;; the U property
	      (U (default #unspecified))
	      ;; the Cn property
	      (Cn (default '()))
	      ;; the Ct property
	      (Ct (default '()))
	      ;; the variable from which self is the kont.
	      (kont (default '()))
	      ;; is this function globalized?
	      G?::bool
	      ;; is this function forced to be globalized?
	      (forceG?::bool (default #f))
	      ;; the integrator (if not G?)
	      (L (default #unspecified))
	      ;; the integrated functions (if G?).
	      (Led (default '()))
	      ;; a stamp to integrate functions.
	      (istamp (default #unspecified))
	      ;; a pointer to the globalized function.
	      (global (default #unspecified))
	      ;; the list of the kaptured variables
	      (kaptured (default #unspecified))
	      ;; tail-coercion
	      (tail-coercion::obj (default #unspecified))
	      ;; is it an exit handler
	      (xhdl?::bool (default #f))
	      ;; the list of exit handlers the function calls
	      (xhdls::pair-nil (default '())))))
	      
	   

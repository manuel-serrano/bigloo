;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/iinfo.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 28 14:06:46 1996                          */
;*    Last change :  Thu Apr  3 10:46:58 2003 (serrano)                */
;*    Copyright   :  1996-2003 Manuel Serrano, see LICENSE file        */
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
   
   (export (wide-class svar/Iinfo::svar
	      ;; a stamp to compute free variables
	      (f-mark (default #unspecified))
	      ;; a stamp to compute union of variables
	      (u-mark (default #unspecified))
	      ;; is a local kaptured ?
	      (kaptured?::bool (default #f))
	      ;; celled?
	      (celled?::bool (default #f)))

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
	      (cto (default '()))
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
	      ;; is this function globalized ?
	      G?::bool
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
	      (tail-coercion::obj (default #unspecified)))))
	      
	   

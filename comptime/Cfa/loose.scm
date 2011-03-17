;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/loose.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 12:02:51 1996                          */
;*    Last change :  Thu Mar 17 09:16:42 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Loosing approximations means values are going outside.           */
;*    Toping approximations means add top the sets.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_loose
   (include "Tools/trace.sch")
   (import  type_type
	    tools_shape
	    ast_var
	    ast_node
	    cfa_iterate
	    cfa_set
	    cfa_info
	    cfa_info2
	    cfa_approx
	    cfa_procedure)
   (export  (loose!::approx ::approx ::obj)
	    (generic loose-alloc! ::node)
	    (global-loose! ::global ::approx)))
 
;*---------------------------------------------------------------------*/
;*    loose! ...                                                       */
;*---------------------------------------------------------------------*/
(define (loose!::approx approx::approx owner)
   (trace (cfa 4) "loose!: " (shape approx) #\Newline)
   (with-access::approx approx (lost-stamp)
      (if (<fx lost-stamp *cfa-stamp*)
	  (begin
	     (set! lost-stamp *cfa-stamp*)
	     (for-each-approx-alloc loose-alloc! approx))))
   approx)

;*---------------------------------------------------------------------*/
;*    loose-alloc! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (loose-alloc! node::node))

;*---------------------------------------------------------------------*/
;*    global-loose! ...                                                */
;*    -------------------------------------------------------------    */
;*    Global variable are not allowed to hold optimized procedure      */
;*    because due to the module compilation we can't ensure that       */
;*    the global variable is initialized and then, we always need      */
;*    to type check it (which cannot be performed with optimized       */
;*    procedures).                                                     */
;*---------------------------------------------------------------------*/
(define (global-loose! global approx)
   (trace (cfa 2)
	  "global-loose!: " (shape global) " " (shape approx) #\Newline)
   (if (memq (global-import global) '(import export))
       (loose! approx 'all)
       (disable-X-T! approx "global track is lost")))

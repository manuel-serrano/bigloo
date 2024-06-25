;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/Integrate/definition.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 13 16:16:29 1995                          */
;*    Last change :  Tue Jun 25 15:02:39 2024 (serrano)                */
;*    Copyright   :  1995-2024 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The integration of one global definition.                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_definition
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  type_type
	    engine_param
	    ast_var
	    ast_node
	    ast_remove
	    tools_shape
	    tools_speek
	    integrate_info
	    integrate_a
	    integrate_kk
	    integrate_u
	    integrate_ctn
	    integrate_g
	    integrate_kaptured
	    integrate_let-fun
	    integrate_node
	    integrate_local->global)
   (export  (integrate-definition! <global>)))
 
;*---------------------------------------------------------------------*/
;*    integrate-definition! ...                                        */
;*    -------------------------------------------------------------    */
;*    This pass is another globalization pass. This pass is            */
;*    mandatory for the C back-end because this language does not      */
;*    possess local [recursive] functions. The theoretical             */
;*    fundations of this pass can be found in the Nitsan Seniak        */
;*    thesis about Sqil (page 100). Here is briefly described          */
;*    the organization of this pass:                                   */
;*        i) compute the Phi set (the set of used functions).          */
;*           The computed Phi is a subset of the real                  */
;*           Phi. Here its formal definition:                          */
;*                  Phi = { f in PHI ^ !G( f ) }                       */
;*           ie. Phi = PHI minus all the Globalized functions          */
;*           by the `Globalize' pass.                                  */
;*       ii) Compute A                                                 */
;*      iii) Compute K                                                 */
;*       iv) Compute K*                                                */
;*        v) Compute U                                                 */
;*       vi) Compute Cn, Ct and G                                      */
;*---------------------------------------------------------------------*/
(define (integrate-definition! global)
   (with-trace 'integrate "integrate-definition!"
      (trace-item "global=" (shape global))
      (let* ((fun (global-value global))
	     (body (sfun-body fun))
	     (A (A global body)))
	 (trace-item "A=" (map shape A))
	 (K*! (K! A global))
	 (U!) 
	 (let ((G (G! (Cn&Ct! A))))
	    (trace-item "G=" (map shape G))
	    ;; for each globalized function, we set the integrated
	    ;; functions in order to be able to build the new
	    ;; global functions
	    (if (null? G)
		;; an optimization to avoid useless long compilations.
		(list global)
		(begin
		   ;; we print the globalization result
		   (verb-globalization global G)
		   (for-each
		      (lambda (f)
			 (when (and (local? f) (not (sfun/Iinfo-G? (local-value f))))
			    (let* ((g (sfun/Iinfo-L (local-value f)))
				   (ifu (variable-value g)))
			       (sfun/Iinfo-Led-set! ifu
				  (cons f (sfun/Iinfo-Led ifu))))))
		      *phi*)
		   ;; for each function (local and global), we add/remove
		   ;; the integrated local functions.
		   (for-each displace-let-fun! G)
		   (displace-let-fun! global)
		   ;; we have computed for all the global functions (including
		   ;; the root global one) the new bodies. Now we compute
		   ;; the set of kaptured variables (on for the local functions,
		   ;; of course).
		   (set-kaptured! G)
		   ;; now for each function, we allocate a new
		   ;; global definition
		   (let ((new-G (map local->global G)))
		      (sfun-body-set! fun (integrate-globalize! body global '()))
		      (cons global new-G))))))))

;*---------------------------------------------------------------------*/
;*    verb-globalization ...                                           */
;*---------------------------------------------------------------------*/
(define (verb-globalization global G)
   (for-each (lambda (local)
		(verbose 3 "           " (shape local) " -->" #\Newline))
	     G))
 

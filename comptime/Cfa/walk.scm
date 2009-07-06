;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/walk.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Thu Sep 25 18:07:49 2003 (serrano)                */
;*    Copyright   :  1995-2003 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `control flow analysis' and its optimizations described in:  */
;*                                                                     */
;*    @InProceedings{ sf:icfp96                                        */
;*      author	  = {Serrano, M. and Feeley, M.},                      */
;*      title	  = {{S}orage {U}se {A}nalysis and its {A}pplications},*/
;*      year	  = 1996,                                              */
;*      month	  = may,                                               */
;*      booktitle = "1fst " # icfp,                                    */
;*      address	  = {Philadelphia, Penn, US},                          */
;*    }                                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_walk
   (include "Engine/pass.sch")
   (import  tools_shape
	    tools_error
	    engine_param
	    type_type
	    ast_remove
	    ast_var
	    ast_node
	    ast_shrinkify
	    cfa_collect
	    cfa_setup
	    cfa_iterate
	    cfa_show
	    cfa_info
	    cfa_info2
	    cfa_info3
	    cfa_approx
	    cfa_type
	    cfa_closure
	    cfa_specialize
	    cfa_tvector
	    cfa_arithmetic)
   (export  (cfa-walk! <global>*)))

;*---------------------------------------------------------------------*/
;*    cfa-walk! ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function is the engine of all the cfa based optimizations.  */
;*    First, this function invokes the cfa, then the following         */
;*    optimizations are performed:                                     */
;*       1. dead code remove                                           */
;*       2. closure optimization (mapping to X and T)                  */
;*       3. type setting (a kind of reverse type inference)            */
;*---------------------------------------------------------------------*/
(define (cfa-walk! globals)
   (pass-prelude "Cfa")
   ;; first of all, we scan the global definitions in order
   ;; to collect all used types and allocations.
   (collect-all-approx! globals)
   ;; we have collected all the approximation, we can now declare
   ;; the approximations sets.
   (declare-approx-sets!)
   ;; we have collect all the possible approximation, we now
   ;; prepare the ast to the iteration process.
   (set-initial-approx! globals)
   ;; ok, we can start now the control flow analysis
   (let ((iteration-roots (cfa-iterate-to-fixpoint! globals)))
      ;; the number of iterations
      (show-cfa-nb-iterations)
      ;; dead code removal
      (let ((globals (remove-var '(cfa inline) globals)))
	 ;; we show the result of the approximation (after dead-code-removal!)
	 (show-cfa-results globals)
	 ;; tvector optimization
	 (let ((additional (profile tvect (vector->tvector! globals))))
	    ;; type settings
	    (profile type (type-settings! globals))
	    ;; we optimize closure allocations
	    (if (not (and (number? *profile-mode*) (> *profile-mode* 0)))
		(profile clo (closure-optimization! globals)))
	    ;; we cleanup the arithmetic optimizer
	    (cleanup-arithmetic-nodes!)
	    ;; generic arithmetic specialization
	    (specialize! globals)
	    ;; and we are done
	    (pass-postlude (shrinkify! (append additional globals))
			   unpatch-vector-set!)))))
 
 

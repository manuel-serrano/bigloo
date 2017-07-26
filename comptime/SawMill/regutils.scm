;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/regutils.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 31 09:56:21 2005                          */
;*    Last change :  Mon Jul 24 12:45:43 2017 (serrano)                */
;*    Copyright   :  2005-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute the liveness analysis of the rtl instructions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_regutils
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    tools_speek
	    backend_backend
	    saw_lib
	    saw_defs
	    saw_node2rtl
	    saw_regset)
   
   (export  (collect-register!::pair-nil ::rtl_reg)
	    (collect-registers!::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    collect-register! ...                                            */
;*---------------------------------------------------------------------*/
(define (collect-register!::pair-nil o::rtl_reg)
   (if (or (rtl_reg/ra? o) (rtl_reg-onexpr? o))
       '()
       (begin
	  (set! *register-count* (+fx 1 *register-count*))
	  (list (widen!::rtl_reg/ra o (num *register-count*))))))

;*---------------------------------------------------------------------*/
;*    *register-count* ...                                             */
;*---------------------------------------------------------------------*/
(define *register-count* -1)

;*---------------------------------------------------------------------*/
;*    collect-registers!::pair-nil ...                                 */
;*    -------------------------------------------------------------    */
;*    Collect all the registers from a list of blocks.                 */
;*---------------------------------------------------------------------*/
(define (collect-registers!::pair-nil o)
   
   (define (args-collect-registers! o)
      (cond
	 ((rtl_reg? o)
	  (collect-register! o))
	 ((rtl_ins? o)
	  (with-access::rtl_ins o (dest args)
	     (append! (args-collect-registers! dest)
		(append-map! args-collect-registers! args))))
	 (else
	  '())))
   
   (define (ins-collect-registers! o)
      (with-access::rtl_ins o (dest args)
	 (append! (args-collect-registers! dest)
	    (append-map! args-collect-registers! args))))
   
   (define (block-collect-registers! o)
      (append-map! ins-collect-registers! (block-first o)))
   
   (set! *register-count* -1)
   (append-map! block-collect-registers! o))
   

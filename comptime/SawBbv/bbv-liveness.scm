;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawBbv/bbv-liveness.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 27 15:14:18 2023                          */
;*    Last change :  Wed Sep 27 15:17:37 2023 (serrano)                */
;*    Copyright   :  2023 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV liveness (and widening).                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-liveness
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawBbv/bbv-types.sch")
   
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
	    saw_regset
	    saw_regutils
	    saw_bbv-types
	    saw_bbv-cache
	    saw_bbv-utils
	    saw_bbv-range
	    saw_bbv-merge
	    saw_bbv-config
	    saw_bbv-cost)

   (export (bbv-liveness!::pair-nil ::backend ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bbv-liveness! ...                                                */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*    -------------------------------------------------------------    */
;*    This function implements a fix point interation to find the      */
;*    maximal solution of the equations:                               */
;*       in[ n ] = use[ n ] U (out[ n ] - def[ n ])                    */
;*      out[ n ] = Union(succ[ n ]) in[ s ]                            */
;*---------------------------------------------------------------------*/
(define (bbv-liveness! back blocks params)
   
   (define (rtl_ins/bbv-in i)
      (with-access::rtl_ins/bbv i (in)
	 in))
   
   (define (liveness-block! block)
      (with-access::block block (succs first)
	 (let loop ((inss (reverse first))
		    (succ (map (lambda (b) (car (block-first b))) succs))
		    (t #f))
	    (if (pair? inss)
		(with-access::rtl_ins/bbv (car inss) (out fun in def)
		   (let ((u (cond
			       ((rtl_ins? succ)
				(regset-union! out (rtl_ins/bbv-in succ)))
			       ((pair? succ)
				(regset-union*! out (map rtl_ins/bbv-in succ)))
			       (else
				#f))))
		      (regset-for-each
			 (lambda (r)
			    (when (not (regset-member? r def))
			       (set! u (or (regset-add! in r) u))))
			 out)
		      (loop (cdr inss) (car inss) (or t u))))
		t))))
   
   (with-trace 'bbv "liveness"
      (let* ((cregs (collect-registers! blocks))
	     (hregs (append-map! collect-register! (backend-registers back)))
	     (pregs (filter rtl_reg/ra? params))
	     (regs (append hregs cregs)))
	 ;; pre widen the instructions
	 (widen-bbv! blocks regs)
	 ;; add the argument of the function to the IN set of the
	 ;; first instruction of the first block
	 (when (pair? blocks)
	    (let ((inss (block-first (car blocks))))
	       (when (pair? inss)
		  (let ((ins (car inss)))
		     (with-access::rtl_ins/bbv ins (in)
			(for-each (lambda (a) (regset-add! in a)) pregs))))))
	 ;; fix-point iteration
	 (let loop ((i 0))
	    (let liip ((bs blocks)
		       (t #f))
	       (if (null? bs)
		   (if t
		       (loop (+fx i 1))
		       regs)
		   (liip (cdr bs) (or (liveness-block! (car bs)) t))))))))

;*---------------------------------------------------------------------*/
;*    widen-bbv! ...                                                   */
;*    -------------------------------------------------------------    */
;*    Widen the blocks and instructions for preparing the register     */
;*    allocation.                                                      */
;*---------------------------------------------------------------------*/
(define (widen-bbv! o regs::pair-nil)
   
   (define (get-args o)
      (filter rtl_reg/ra? (rtl_ins-args* o)))
   
   (define (args-widen-bbv! o)
      (when (rtl_ins? o)
	 (with-access::rtl_ins o (args fun dest)
	    (widen!::rtl_ins/bbv o
	       (def (make-empty-regset regs))
	       (in (list->regset (get-args o) regs))
	       (out (if dest
			(list->regset (list dest) regs)
			(make-empty-regset regs))))
	    (for-each args-widen-bbv! args))))

   (define (overflow-def o)
      ;; return the register assigned in special overflow operations
      (when (rtl_ins-fxovop? o)
	 ;; these special instructions assigned (so "define") their
	 ;; third arguments
	 (with-access::rtl_ins o (fun args)
	    (with-access::rtl_ifne fun (then)
	       (let ((call (car args)))
		  (with-access::rtl_ins (car args) (args)
		     (caddr args)))))))
   
   (define (ins-widen-bbv! o)
      (with-access::rtl_ins o (dest args fun)
	 (widen!::rtl_ins/bbv o
	    (def (cond
		    ((overflow-def o)
		     =>
		     (lambda (reg)
			(list->regset (list reg) regs)))
		    ((or (not dest) (rtl_reg-onexpr? dest))
		     (make-empty-regset regs))
		    (else
		     (list->regset (list dest) regs))))
	    (in (list->regset (get-args o) regs))
	    (out (make-empty-regset regs)))
	 (for-each args-widen-bbv! args)))
   
   (define (block-widen-bbv! o)
      (widen!::blockV o)
      (with-access::block o (first)
	 (for-each ins-widen-bbv! first)))

   (for-each block-widen-bbv! o))


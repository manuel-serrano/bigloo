;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/SawMill/regalloc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 31 09:56:21 2005                          */
;*    Last change :  Fri Jul 21 09:29:33 2017 (serrano)                */
;*    Copyright   :  2005-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Compute the liveness analysis of the rtl instructions            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_register-allocation
   
   (include "Tools/trace.sch"
	    "SawMill/regalloc.sch")
   
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
	    saw_regset
	    saw_regutils)
   
   (static  (wide-class block/ra::block
	       last::pair-nil)
	    
	    (wide-class rtl_ins/ra::rtl_ins
	       (def (default #unspecified))
	       (out (default #unspecified))
	       (in (default #unspecified))
	       (spill (default #unspecified))))
   
   (export  (register-allocation::pair-nil ::backend
	       ::global ::pair-nil ::pair-nil)
	    (generic type-interference! ::backend ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    register-allocation ...                                          */
;*---------------------------------------------------------------------*/
(define (register-allocation back global params blocks)
   (let* ((id (global-id global))
	  (p (and *saw-register-allocation?*
		  (not (memq id *saw-no-register-allocation-functions*))
		  (if (pair? *saw-register-allocation-functions*)
		      (memq id *saw-register-allocation-functions*)
		      (or (<fx *saw-register-allocation-max-size* 0)
			  (<fx (rtl-size blocks)
			       *saw-register-allocation-max-size*))))))
      (if p
	  (%register-allocation back global params blocks)
	  (verbose 2 "        NOT reg. alloc. "
		   (shape global) " [size=" (rtl-size blocks)
		   " instrs]: too large\n"))
      blocks))

;*---------------------------------------------------------------------*/
;*    %register-allocation ...                                         */
;*---------------------------------------------------------------------*/
(define (%register-allocation back global params blocks)
   (verbose 2 "        reg. alloc. "
      (shape global) " [size=" (rtl-size blocks) " instrs]:\n")
   ;; cleanup the tree to remove useless dest registers
   (cleanup-move-tree! blocks)
   ;; compute the set of temporaries of blocks
   (let* ((cregs (collect-registers! blocks))
	  (hregs (append-map! collect-register! (backend-registers back)))
	  (pregs (filter rtl_reg/ra? params))
	  (regs (append hregs cregs)))
      (when (pair? cregs)
	 ;; prepare the register for interference
	 (for-each (lambda (r)
		      (with-access::rtl_reg/ra r (interfere)
			 (set! interfere (make-empty-regset regs))))
	    regs)
	 (verbose 3  "          number of registers... " (length hregs) "\n")
	 (verbose 3  "          number of parameters... " (length pregs) "\n")
	 (verbose 3  "          number of temporaries... " (length cregs) "\n")
	 ;; mark the registers involved in protect instructions as interfering
	 (protect-interference! blocks regs)
	 ;; widen the blocks and instructions
	 (widen-ra! blocks regs)
	 ;; temporaries liveness
	 (liveness! blocks pregs)
	 (on-trace (jvmas 3)
	    (dump-basic-blocks liveness: global pregs blocks))
	 ;; add the interference in between hardware registers and temporaries
	 (hardware-mutual-interference! hregs)
	 (hardware-parameters-interference! back pregs)
	 (hardware-interference! back blocks)
	 ;; the backend-depend register type interference
	 (type-interference! back regs)
	 ;; interference graph
	 (interference! blocks regs)
	 ;; cleanup the useless dest registers
	 (cleanup-dest! blocks)
	 ;; graph coloring
	 (register-coloring pregs hregs cregs)
	 ;; funcall spill
	 (funcall-spill! hregs blocks)
	 ;; register allocation
	 (on-trace (jvmas 4)
	    (dump-basic-blocks before-mapping: global pregs blocks))
	 (rtl-map-registers! pregs cregs blocks)
	 ;; remove useless move
	 (when (>fx *optim* 1)
	    (remove-nop-move! blocks)))
      (on-trace (jvmas 2)
	 (dump-basic-blocks reg-alloc: global pregs blocks))
      ;; shrink the hardware registers
      (for-each shrink! regs)
      (for-each (lambda (r) (when (isa? r rtl_reg/ra) (shrink! r))) pregs)))

;*---------------------------------------------------------------------*/
;*    widen-ra! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Widen the blocks and instructions for preparing the register     */
;*    allocation.                                                      */
;*---------------------------------------------------------------------*/
(define (widen-ra! o regs::pair-nil)
   (define (get-args o)
      (filter rtl_reg/ra? (rtl_ins-args* o)))
   (define (args-widen-ra! o)
      (when (rtl_ins? o)
	 (with-access::rtl_ins o (args fun)
	    (widen!::rtl_ins/ra o
	       (def (make-empty-regset regs))
	       (in (list->regset (map use-register! (get-args o)) regs))
	       (out (make-empty-regset regs))
	       (spill (make-empty-regset regs)))
	    (for-each args-widen-ra! args))))
   (define (ins-widen-ra! o)
      (with-access::rtl_ins o (dest args fun)
	 (widen!::rtl_ins/ra o
	    (def (if (or (not dest) (rtl_reg-onexpr? dest))
		     (make-empty-regset regs)
		     (list->regset (list (use-register! dest)) regs)))
	    (in (list->regset (map use-register! (get-args o)) regs))
	    (out (make-empty-regset regs))
	    (spill (make-empty-regset regs)))
	 (for-each args-widen-ra! args)))
   (define (block-widen-ra! o)
      (with-access::block o (first)
	 (let ((last (reverse first)))
	    (for-each ins-widen-ra! first)
	    (widen!::block/ra o
	       (last last)))))
   (for-each block-widen-ra! o))

;*---------------------------------------------------------------------*/
;*    liveness! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Computes the liveness of a list of blocks. Returns the           */
;*    list of used registers.                                          */
;*---------------------------------------------------------------------*/
(define (liveness! blocks args)
   (verbose 3  "          liveness..")
   ;; add the argument of the function to the IN set of the
   ;; first instruction of the first block
   (when (pair? blocks)
      (let ((inss (block-first (car blocks))))
	 (when (pair? inss)
	    (let ((ins (car inss)))
	       (with-access::rtl_ins/ra ins (in)
		  (for-each (lambda (a) (regset-add! in a)) args))))))
   ;; fix-point iteration
   (let loop ((i 0))
      (verbose 3 "." i)
      (let liip ((bs blocks)
		 (t #f))
	 (if (null? bs)
	     (if t
		 (loop (+fx i 1))
		 #f)
	     (liip (cdr bs) (or (liveness-block! (car bs)) t)))))
   (verbose 3 " done\n"))

;*---------------------------------------------------------------------*/
;*    liveness-block! ...                                              */
;*---------------------------------------------------------------------*/
(define (liveness-block! block)
   (with-access::block/ra block (succs last)
      (let loop ((inss last)
		 (succ (map (lambda (b) (car (block-first b))) succs))
		 (t #f))
	 (if (pair? inss)
	     (with-access::rtl_ins/ra (car inss) (out fun in def)
		(let ((u (cond
			    ((rtl_ins? succ)
			     (regset-union! out (rtl_ins/ra-in succ)))
			    ((pair? succ)
			     (regset-union*! out (map rtl_ins/ra-in succ)))
			    (else
			     #f))))
		   (regset-for-each (lambda (r)
				       (if (not (regset-member? r def))
					   (set! u (or (regset-add! in r) u))))
		      out)
		   (loop (cdr inss) (car inss) (or t u))))
	     t))))

;*---------------------------------------------------------------------*/
;*    protect-interference! ...                                        */
;*    -------------------------------------------------------------    */
;*    Walk a list of blocks searching for the protect intructions.     */
;*---------------------------------------------------------------------*/
(define-generic (protect-interference! o regs::pair-nil)
   (define (ins-protect-interference! o)
      (with-access::rtl_ins o (dest fun)
	 (when (rtl_protect? fun)
	    ;; dest is protected, it interfers with all the other registers
	    (for-each (lambda (r) (interfere-reg! dest r)) regs))))
   (define (block-protect-interference! o)
      (for-each ins-protect-interference! (block-first o)))
   (for-each block-protect-interference! o))

;*---------------------------------------------------------------------*/
;*    hardware-mutual-interference! ...                                */
;*    -------------------------------------------------------------    */
;*    Mark that all hardware registers interfere one with each others. */
;*---------------------------------------------------------------------*/
(define (hardware-mutual-interference! regs::pair-nil)
   (let loop ((r regs))
      (when (pair? r)
	 (let ((r1 (car r)))
	    (for-each (lambda (r2) (interfere-reg! r1 r2)) (cdr r))
	    (loop (cdr r))))))

;*---------------------------------------------------------------------*/
;*    hardware-parameters-interference! ...                            */
;*    -------------------------------------------------------------    */
;*    The function handles the constraints imposed by the back-end     */
;*    on the allocation of parameters (i.e., which registers are       */
;*    used to send parameters).                                        */
;*    -------------------------------------------------------------    */
;*    The REGS interfere with all the backend registers that are       */
;*    not used in the funcall protocol.                                */
;*---------------------------------------------------------------------*/
(define (hardware-parameters-interference! back::backend pregs::pair-nil)
   (with-access::backend back (pregisters registers)
      (for-each (lambda (r)
		   (unless (memq r pregisters)
		      (for-each (lambda (pr)
				   (interfere-reg! pr r))
				pregs)))
		registers)))
   
;*---------------------------------------------------------------------*/
;*    hardware-interference! ::obj ...                                 */
;*    -------------------------------------------------------------    */
;*    Walk a list of blocks in order to handle the hardware            */
;*    interferences imposed by the backend.                            */
;*---------------------------------------------------------------------*/
(define (hardware-interference! back::backend o::obj)
   (define (ins-hardware-interference! o::rtl_ins/ra in out)
      (multiple-value-bind (reset spill rdest a0 a1)
	 (backend-instr-reset-registers back o)
	 (when (pair? reset)
	    (regset-for-each (lambda (r1)
				(for-each (lambda (r2) (interfere-reg! r1 r2))
					  reset))
			     in)
	    (regset-for-each (lambda (r1)
				(for-each (lambda (r2) (interfere-reg! r1 r2))
					  reset))
			     out))
	 (with-access::backend back (registers)
	    (with-access::rtl_ins o (dest args)
	       ;; the constraints imposed on the destination
	       (when (and (pair? rdest) (rtl_reg/ra? dest))
		  (for-each (lambda (r)
			       (unless (memq r rdest)
				  (interfere-reg! dest r)))
			    registers))
	       ;; the constraints imposed on the first argument
	       (when (and (pair? a0) (pair? args) (rtl_reg/ra? (car args)))
		  (for-each (lambda (r)
			       (unless (memq r a0)
				  (interfere-reg! r (car args))))
			    registers))
	       ;; the constraints imposed on the second argument
	       (when (and (pair? a1)
			  (pair? args) (pair? (cdr args))
			  (rtl_reg/ra? (cadr args)))
		  (for-each (lambda (r)
			       (unless (memq r a1)
				  (interfere-reg! r (cadr args))))
			    registers))
	       ;; recursive traversal of the arguments
	       (for-each (lambda (a)
			    (when (rtl_ins? a)
			       (ins-hardware-interference! a in out)))
			 args)))
	 (for-each (lambda (r)
		      (regset-add! (rtl_ins/ra-spill o) r))
		   spill)))
   (define (block-hardware-interference! o)
      (for-each (lambda (o)
		   (with-access::rtl_ins/ra o (in out)
		      (ins-hardware-interference! o in out)))
		(block-first o)))
   (for-each block-hardware-interference! o))
					
;*---------------------------------------------------------------------*/
;*    type-interference! ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is generic in order to let backend to override     */
;*    the general definition. For instance, the JVM backend overrides  */
;*    it because the JVM does not use typed registers.                 */
;*---------------------------------------------------------------------*/
(define-generic (type-interference! back::backend regs)
   (when (pair? regs)
      (let loop ((regs regs))
         (when (pair? (cdr regs))
            (let* ((r1 (car regs))
                   (t1 (rtl_reg-type r1)))
               (when (type? t1)
                  (for-each (lambda (r2)
                               (let ((t2 (rtl_reg-type r2)))
                                  (unless (or (eq? t1 t2)
					      (and (eq? t1 *obj*)
						   (bigloo-type? t2))
					      (and (eq? t2 *obj*)
						   (bigloo-type? t1)))
                                     (interfere-reg! r1 r2))))
		     (cdr regs)))
               (loop (cdr regs)))))))

;*---------------------------------------------------------------------*/
;*    interference! ...                                                */
;*    -------------------------------------------------------------    */
;*    Computes the interference graph for REGS based on                */
;*    the liveness computed over BLOCKS.                               */
;*---------------------------------------------------------------------*/
(define (interference! blocks regs)
   (verbose 3  "          graph interference... ")
   ;; fill the interference graph with liveness information
   (for-each (lambda (b)
		(for-each (lambda (i) (interfere-ins! i)) (block-first b)))
      blocks)
   (verbose 3  "done\n"))

;*---------------------------------------------------------------------*/
;*    interfere-ins! ...                                               */
;*---------------------------------------------------------------------*/
(define (interfere-ins! i::rtl_ins/ra)
   (with-access::rtl_ins/ra i (in out)
      (regset-for-each
	 (lambda (r1)
	    (regset-for-each
	       (lambda (r2)
		  (interfere-reg! r1 r2))
	       in))
	 in)
      (regset-for-each
	 (lambda (r1)
	    (regset-for-each
	       (lambda (r2)
		  (interfere-reg! r1 r2))
	       out))
	 out)))

;*---------------------------------------------------------------------*/
;*    interfere-reg! ...                                               */
;*---------------------------------------------------------------------*/
(define (interfere-reg! r1 r2)
   (unless (or (eq? r1 r2) (regset-member? r2 (rtl_reg/ra-interfere r1)))
      (regset-add! (rtl_reg/ra-interfere r1) r2)
      (regset-add! (rtl_reg/ra-interfere r2) r1)))

;*---------------------------------------------------------------------*/
;*    cleanup-dest! ...                                                */
;*---------------------------------------------------------------------*/
(define (cleanup-dest! blocks::pair-nil)
   (verbose 3  "          removed useless dest. registers... ")
   (let ((n 0))
      (for-each (lambda (b)
		   (for-each (lambda (i)
				(with-access::rtl_ins/ra i (dest out)
				   (when (rtl_reg/ra? dest)
				      (unless (regset-member? dest out)
					 (set! n (+fx n 1))
					 (set! dest #f)))))
			     (block-first b)))
		blocks)
      (verbose 3 n "\n")))

;*---------------------------------------------------------------------*/
;*    register-coloring ...                                            */
;*---------------------------------------------------------------------*/
(define (register-coloring pregs::pair-nil hregs::pair-nil cregs::pair)
   ;; duplicate the intefere sets of allocable registers (i.e. not hregs)
   (for-each (lambda (r)
		(with-access::rtl_reg/ra r (interfere interfere2)
		   (set! interfere2 (duplicate-regset interfere))))
	     (append pregs hregs cregs))
   ;; select the coloring schema according the backend imposed constraints
   (let ((cregs (filter (lambda (r) (eq? (rtl_reg/ra-color r) #f)) cregs)))
      (when (pair? cregs)
	 (cond
	    ((null? hregs)
	     (register-stack-coloring pregs cregs))
	    ((pair? cregs)
	     (register-hardware-coloring pregs hregs cregs))))))

;*---------------------------------------------------------------------*/
;*    register-stack-coloring ...                                      */
;*---------------------------------------------------------------------*/
(define (register-stack-coloring pregs::pair-nil cregs::pair)
   (define (select-register! registers size)
      (let loop ((regs registers)
		 (min #f)
		 (mval (+fx (interference-degree (car registers)) 1))
		 (prev #f))
	 (cond
	    ((null? regs)
	     (values min (interference-degree min) (remq! min registers)))
	    ((<fx (interference-degree (car regs)) size)
	     (let* ((reg (car regs))
		    (sz (interference-degree reg)))
		(if (pair? prev)
		    (begin
		       (set-cdr! prev (cdr regs))
		       (values reg sz registers))
		    (values reg sz (cdr regs)))))
	    ((<fx (interference-degree (car regs)) mval)
	     (loop (cdr regs)
		   (car regs)
		   (interference-degree (car regs))
		   regs))
	    (else
	     (loop (cdr regs)
		   min
		   mval
		   regs)))))
   (define colors (make-vector (length (append pregs cregs)) #f))
   (define (find-color interfere max)
      (vector-fill! colors #f)
      (regset-for-each (lambda (r)
			  (with-access::rtl_reg/ra r (color)
			     (when (fixnum? color)
				(vector-set! colors color #t))))
		       interfere)
      (if (=fx (vector-length colors) max)
	  max
	  (let loop ((i 0))
	     (if (vector-ref colors i)
		 (loop (+fx i 1))
		 i))))
   (define (colorize! min stack)
      (let loop ((stack stack)
		 (max min))
	 (if (pair? stack)
	     (with-access::rtl_reg/ra (car stack) (interfere color)
		(set! color (find-color interfere max))
		(loop (cdr stack) (if (=fx color max) (+fx max 1) max)))
	     max)))
   ;; pre-allocate the arguments
   (let ((n 0))
      (for-each (lambda (r)
		   (simplify-reg! r)
		   (rtl_reg/ra-color-set! r n)
		   (set! n (+fx n 1)))
		pregs))
   ;; allocate the temporaries
   (let ((min (length pregs))
	 (sregs (sort cregs
		      (lambda (r1 r2)
			 (<fx (interference-degree r1)
			      (interference-degree r2))))))
      (let loop ((regs sregs)
		 (stack '())
		 (size 0))
	 (if (null? regs)
	     (let ((n (colorize! min stack)))
		(verbose 3 "          stack coloring... " n
			 " (of " (+ (length pregs) (length cregs)) ")"
			 " registers\n"))
	     (multiple-value-bind (reg nsize regs)
		(select-register! regs size)
		(simplify-reg! reg)
		(loop regs (cons reg stack) nsize))))))

;*---------------------------------------------------------------------*/
;*    register-hardware-coloring ...                                   */
;*---------------------------------------------------------------------*/
(define (register-hardware-coloring pregs::pair-nil hregs::pair cregs::pair)
   (define (select-simplify-register regs::pair k::int)
      (any (lambda (r)
	      (and (not (rtl_reg-hardware r))
		   (<fx (interference-degree r) k)
		   r))
	   regs))
   (define (select-spill-register::rtl_reg/ra regs::pair)
      ;; select the register with the highest use/degree score
      (define (score reg)
	 (/fl (fixnum->flonum (rtl_reg/ra-occurrences reg))
	      (fixnum->flonum (interference-degree reg))))
      (let loop ((regs regs)
		 (reg #unspecified)
		 (ms 0.0))
	 (if (null? regs)
	     reg
	     (let* ((r (car regs))
		    (s (score r)))
		(if (>fl s ms)
		    (loop (cdr regs) r s)
		    (loop (cdr regs) reg ms))))))
   (define (allocate-color! r)
      (with-access::rtl_reg/ra r (interfere color)
	 (let ((nr (any (lambda (reg)
			   (and (not (regset-member? reg interfere))
				(not (any (lambda (r)
					     (eq? (rtl_reg/ra-color r) reg))
					   (regset->list interfere)))
				(not (regset-member?
				      r
				      (rtl_reg/ra-interfere2 reg)))
				reg))
			hregs)))
	    (regset-for-each (lambda (ri)
			       (regset-add! (rtl_reg/ra-interfere2 nr) ri))
			     (rtl_reg/ra-interfere r))
	    (set! color nr))))
   (define (allocate-colors! stack)
      (for-each allocate-color! stack))
   (let ((k (length hregs)))
      (on-trace (jvmas 3)
		(display "+-- Interference: \n" *trace-port*)
		(for-each (lambda (r)
			     (with-access::rtl_reg/ra r (interfere2 type)
				(display "| " *trace-port*)
				(display (shape r) *trace-port*)
				(when *type-shape?*
				   (display "::" *trace-port*)
				   (display (shape type) *trace-port*))
				(display ":" *trace-port*)
				(regset-for-each
				 (lambda (r)
				    (display " " *trace-port*)
				    (display (shape r) *trace-port*))
				 interfere2)
				(newline *trace-port*)))
			  cregs)
		(display "+----------------\n\n" *trace-port*))
      (let loop ((regs cregs)
		 (stack '())
		 (s 0)
		 (n 0))
	 (if (null? regs)
	     (begin
		(allocate-colors! stack)
		(verbose 3 "          register coloring... "
			 k " h-regs, "
			 (length cregs) " temps -> "
			 n " regs, " s " spills"
			 "\n"))
	     (let ((sr (select-simplify-register regs k)))
		(if sr
		    ;; allocate fresh colors
		    (begin
		       (simplify-reg! sr)
		       (loop (remq sr regs) (cons sr stack) s (+fx 1 n)))
		    ;; cannot allocate colors, spill
		    (let ((r (select-spill-register regs)))
		       (with-access::rtl_reg/ra r (color)
			  (set! color #unspecified)
			  (loop (remq r regs) stack (+fx 1 s) n)))))))))

;*---------------------------------------------------------------------*/
;*    interference-degree ...                                          */
;*    -------------------------------------------------------------    */
;*    The number of registers reg interferes with.                     */
;*---------------------------------------------------------------------*/
(define (interference-degree::int reg::rtl_reg/ra)
   (regset-length (rtl_reg/ra-interfere2 reg)))

;*---------------------------------------------------------------------*/
;*    simplify-reg! ...                                                */
;*    -------------------------------------------------------------    */
;*    Simply a register. I.e. removes it from the interference         */
;*    graph.                                                           */
;*---------------------------------------------------------------------*/
(define (simplify-reg! r::rtl_reg/ra)
   (with-access::rtl_reg/ra r (interfere2)
      (regset-for-each (lambda (r2)
			  (regset-remove! (rtl_reg/ra-interfere2 r2) r)
			  (regset-remove! interfere2 r2))
		       interfere2)))

;*---------------------------------------------------------------------*/
;*    rtl-map-registers! ...                                           */
;*    -------------------------------------------------------------    */
;*    This function maps the temporaries to the registers allocated    */
;*    during the coloring. In addition, it removes the destination     */
;*    registers that are not present in the OUT set of the             */
;*    instructions.                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-map-registers! pregs cregs blocks)
   (let ((colors (make-vector (+fx (length pregs) (length cregs)))))
      (for-each (lambda (r)
		   (with-access::rtl_reg/ra r (color)
		      (when (fixnum? color)
			 (vector-set! colors (rtl_reg/ra-color r) r))))
		(append cregs pregs))
      (define mapping '())
      (define (type-register ty::type r::rtl_reg)
	 (cond
	    ((not (rtl_reg/ra? r))
	     r)
	    ((not (rtl_reg-hardware r))
	     r)
	    ((eq? ty *obj*)
	     r)
	    (else
	     (let ((k (cons r ty)))
		(let ((c (assoc k mapping)))
		   (if (pair? c)
		       (cdr c)
		       (let ((newr (duplicate::rtl_reg/ra r
				      (key (gensym))
				      (type ty))))
			  (set! mapping (cons (cons k newr) mapping))
			  newr)))))))
      (define (map-register o)
	 (cond
	    ((rtl_reg/ra? o)
	     (type-register
	      (rtl_reg-type o)
	      (with-access::rtl_reg/ra o (color)
		 (cond
		    ((fixnum? color)
		     (vector-ref colors color))
		    ((rtl_reg? color)
		     color)
		    (else
		     o)))))
	    ((rtl_ins? o)
	     (with-access::rtl_ins o (args %spill)
		(set! %spill (map map-register %spill))
		(set! args (map map-register args)))
	     o)
	    (else
	     o)))
      (for-each (lambda (b)
		   (for-each (lambda (i)
				(with-access::rtl_ins/ra i (dest args %spill)
				   (when (rtl_reg/ra? dest)
				      (set! dest (map-register dest)))
				   (set! %spill (map map-register %spill))
				   (set! args (map map-register args))))
			     (block-first b)))
		blocks)
      blocks))

;*---------------------------------------------------------------------*/
;*    remove-nop-move! ...                                             */
;*    -------------------------------------------------------------    */
;*    Remove useless move (e.g. move ri, ri).                          */
;*---------------------------------------------------------------------*/
(define (remove-nop-move! blocks::pair-nil)
   (define (move-nop? i)
      (with-access::rtl_ins/ra i (dest args fun)
	 (and (rtl_mov? fun)
	      (pair? args)
	      (null? (cdr args))
	      (eq? dest (car args)))))
   (define (remove-block! b)
      (let loop ((ins (block-first b))
		 (prev #f)
		 (n 0))
	 (if (pair? ins)
	     (if (move-nop? (car ins))
		 (with-access::rtl_ins/ra (car ins) (dest args fun)
		    (cond
		       ((pair? prev)
			(set-cdr! prev (cdr ins))
			(loop (cdr ins) prev (+fx 1 n)))
		       ((null? (cdr ins))
			(set! fun (instantiate::rtl_nop))
			(set! dest #f)
			(set! args '())
			(+fx 1 n))
		       (else
			(block-first-set! b (cdr ins))
			(loop (cdr ins) prev (+fx 1 n)))))
		 (loop (cdr ins) ins n))
	     n)))
   (let ((n (apply + (map remove-block! blocks))))
      (verbose 3 "          move... " n " removed\n")))

;*---------------------------------------------------------------------*/
;*    funcall-spill! ...                                               */
;*    -------------------------------------------------------------    */
;*    Introduce spill code around funcalls.                            */
;*    -------------------------------------------------------------    */
;*    The spill field from rtl_ins/ra instruction depends on the       */
;*    backend. It has been set by the HARDWARE-INTERFERENCE! function. */
;*---------------------------------------------------------------------*/
(define (funcall-spill! hregs::pair-nil blocks::pair-nil)
   (define (args-funcall-spill! a)
      (when (rtl_ins? a)
	 (ins-funcall-spill! a)))
   (define (ins-funcall-spill! i::rtl_ins/ra)
      (with-access::rtl_ins/ra i (args fun spill out def)
	 (let ((aspill (regset-filter
			(lambda (r)
			   (and (not (regset-member? r def))
				(with-access::rtl_reg/ra r (color)
				   (and (rtl_reg? color)
					(regset-member? color spill)))))
			out)))
	    (when (pair? aspill)
	       (rtl_ins-%spill-set! i aspill)))
	 (for-each args-funcall-spill! args)))
   (define (block-funcall-spill! b)
      (with-access::block b (first)
	 (for-each ins-funcall-spill! first)))
   (if (null? hregs)
       blocks
       (for-each block-funcall-spill! blocks)))

;*---------------------------------------------------------------------*/
;*    cleanup-move-tree! ...                                           */
;*    -------------------------------------------------------------    */
;*    Remove the useless move registers inside a tree.                 */
;*---------------------------------------------------------------------*/
(define (cleanup-move-tree! blocks::pair-nil)

   (define (is-mov? i)
      (when (isa? i rtl_ins)
	 (isa? (rtl_ins-fun i) rtl_mov)))
   
   (define (args-cleanup-move-tree! a)
      (if (rtl_ins? a)
	  (with-access::rtl_ins a (fun dest args)
	     (set! args (map args-cleanup-move-tree! args))
	     (if (rtl_mov? fun)
		 (car args)
		 a))
	  a))
   
   (define (ins-cleanup-move-tree! i)
      (with-access::rtl_ins i (fun dest args)
	 (set! args (map args-cleanup-move-tree! args))
	 i))
   
   (define (block-cleanup-move-tree! b)
      (with-access::block b (first)
	 (for-each ins-cleanup-move-tree! first)))
   
   (for-each block-cleanup-move-tree! blocks))

;*---------------------------------------------------------------------*/
;*    use-register! ...                                                */
;*    -------------------------------------------------------------    */
;*    Mark that the following register and one additional occurrence.  */
;*    The number of occurrence might used for selecting spilled        */
;*    registers during the coloring of the graph.                      */
;*---------------------------------------------------------------------*/
(define (use-register! reg::rtl_reg/ra)
   (with-access::rtl_reg/ra reg (occurrences)
      (set! occurrences (+fx 1 occurrences)))
   reg)

;*---------------------------------------------------------------------*/
;*    rtl-size ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (rtl-size o)
   (if (pair? o)
       (apply + (map rtl-size o))
       1))

;; rtl-size ::block
(define-method (rtl-size o::block)
   (rtl-size (block-first o)))

;; rtl-size ::ins
(define-method (rtl-size o::rtl_ins)
   (with-access::rtl_ins o (args)
      (+fx 1 (rtl-size args))))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins/ra ...                                            */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins/ra p m)
   (with-access::rtl_ins/ra o (out in)
      (call-next-method)
      (on-trace (jvmas 3)
		(when *access-shape?*
		   (display "\n" p)
		   (display "      [in:" p)
		   (regset-for-each
		    (lambda (r) (display " " p) (dump r p 0)) in)
		   (display "]\n" p)
		   (display "      [out:" p)
		   (regset-for-each
		    (lambda (r) (display " " p) (dump r p 0)) out)
		   (display "]" p))
		(when *user-shape?*
		   (multiple-value-bind (reset spill rdest a0 a1)
		      (backend-instr-reset-registers (the-backend) o)
		      (display "\n      [reset:" p)
		      (for-each (lambda (r) (display " " p) (dump r p 0))
				reset)
		      (display "]\n" p)
		      (display "      [spill:" p)
		      (for-each (lambda (r) (display " " p) (dump r p 0))
				spill)
		      (display "]" p))))))


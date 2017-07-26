(module saw_defs
   (import engine_param
	   tools_shape
	   tools_trace
	   type_type
	   ast_var
	   ast_node
	   saw_lib)
   (include "SawMill/defs.sch")
   (export
      ;; Regs
      (final-class rtl_reg
	 type::type ; ::type
	 var ; ::(or local #f)
	 (onexpr? (default #f))
	 (name read-only (default (gensym)))
	 (key read-only (default (gensym)))
	 (hardware read-only (default #f)) )
      ;; Functions
      (class rtl_fun (loc (default #f)))
      ; dest = #f and no continuation (last instruction of terminals blocks)
      (class rtl_last::rtl_fun)
      (class rtl_return::rtl_last type::type)
      (class rtl_jumpexit::rtl_last)
      (class rtl_fail::rtl_last)
      ; dest = #f and multiple continuation (last instruction of blocks)
      (class rtl_notseq::rtl_fun)
      (class rtl_if::rtl_notseq)
      (class rtl_select::rtl_notseq type::type patterns)
      (class rtl_switch::rtl_select labels)
      (class rtl_ifeq::rtl_notseq then::block)
      (class rtl_ifne::rtl_notseq then::block)
      (class rtl_go::rtl_notseq to::block)
      ; doesn't make side effects
      (class rtl_pure::rtl_fun)
      (class rtl_nop::rtl_pure)
      (class rtl_mov::rtl_pure)
      (class rtl_loadi::rtl_pure constant::atom)
      (class rtl_loadg::rtl_pure var::global)
      (class rtl_loadfun::rtl_pure var::global)
      (class rtl_globalref::rtl_pure var::global)
      (class rtl_getfield::rtl_pure name::bstring objtype::type type::type)
      (class rtl_valloc::rtl_pure type::type vtype::type)
      (class rtl_vref::rtl_pure type::type vtype::type)
      (class rtl_vlength::rtl_pure type::type vtype::type)
      (class rtl_instanceof::rtl_pure type::type)
      (class rtl_makebox::rtl_pure)
      (class rtl_boxref::rtl_pure)
      ; dest = #f and make side-effect
      (class rtl_effect::rtl_fun)
      (class rtl_storeg::rtl_effect var::global)
      (class rtl_setfield::rtl_effect name::bstring objtype::type type::type)
      (class rtl_vset::rtl_effect type::type vtype::type)
      (class rtl_boxset::rtl_effect)
      ; others
      (class rtl_new::rtl_fun type::type constr::pair-nil)
      (class rtl_call::rtl_fun var::global)
      (class rtl_apply::rtl_fun)
      (class rtl_lightfuncall::rtl_fun name::symbol funs::pair-nil rettype)
      (class rtl_funcall::rtl_fun)
      (class rtl_pragma::rtl_fun format::bstring)
      (class rtl_cast::rtl_fun totype::type fromtype::type)
      (class rtl_cast_null::rtl_fun type::type)
      (class rtl_protect::rtl_fun)
      (class rtl_protected::rtl_fun)
      
      ;; Instructions
      (final-class rtl_ins
	 (loc (default #f))
	 (%spill::pair-nil (default '()))
	 (dest (default #f)) ; ::(or reg #f)
	 (fun::rtl_fun)
	 (args::pair-nil) )   ; ::(list (or reg ins))
      
      ;; Block of instructions
      (final-class block
	 (label::int (default 0))
	 (preds::pair-nil (default '()))		; ::(list block)
	 (succs::pair-nil (default '()))		; ::(list block)
	 first::pair )				; ::(list ins)

      (rtl_ins-args*::pair-nil ::rtl_ins)
      
      (dump-basic-blocks id v params l)
      (rtl-dump ::obj ::output-port)
      (generic dump ::obj ::output-port ::int)
      (generic dump-fun o::rtl_fun dest args p m)
      (dump* o ::output-port ::int)
      (dump-margin ::output-port ::int)
      (dump-ins-rhs o::rtl_ins p m)
      ))

;*---------------------------------------------------------------------*/
;*    rtl_ins-args* ...                                                */
;*---------------------------------------------------------------------*/
(define (rtl_ins-args* ins::rtl_ins)
   (let loop ((args (rtl_ins-args ins))
	      (res '()))
      (cond
	 ((null? args)
	  res)
	 ((rtl_reg? (car args))
	  (loop (cdr args) (cons (car args) res)))
	 ((rtl_ins? (car args))
	  (loop (cdr args) (append (rtl_ins-args* (car args)) res)))
	 (else
	  (loop (cdr args) res)))))

;*---------------------------------------------------------------------*/
;*    shape ::rtl_ins ...                                              */
;*---------------------------------------------------------------------*/
(define-method (shape i::rtl_ins)
   (call-with-output-string
      (lambda (op)
	 (dump i op 0))))

;*---------------------------------------------------------------------*/
;*    shape ::rtl_reg ...                                              */
;*---------------------------------------------------------------------*/
(define-method (shape o::rtl_reg)
   (let ((p (open-output-string)))
      (with-access::rtl_reg o (var hardware name onexpr? type key)
	 (cond
	    (onexpr?
	     (display "*" p)
	     (display name p))
	    (hardware
	     (display "%" p)
	     (display hardware p))
	    (var
	     (display "!" p)
	     (display (variable-id var) p))
	    (else
	     (display "$" p)
	     (display name p)))
	 (when *type-shape?*
	    (display "::" p)
	    (display (type-id type) p))
	 (when *key-shape?*
	    (display "@" p)
	    (display key p)))
      (close-output-port p)))

;*---------------------------------------------------------------------*/
;*    dump-basic-blocks ...                                            */
;*---------------------------------------------------------------------*/
(define (dump-basic-blocks id v params l)
   (fprint *trace-port* "+-- " id " " (shape v))
   (display "| args:" *trace-port*)
   (map (lambda (a)
	   (display " " *trace-port*)
	   (dump a *trace-port* 0))
	params)
   (newline *trace-port*)
   (fprint *trace-port* "| Basic blocks: " )
   (for-each (lambda (b)
		(rtl-dump b *trace-port*)
		(newline *trace-port*))
	     l))

;*---------------------------------------------------------------------*/
;*    rtl-dump ...                                                     */
;*---------------------------------------------------------------------*/
(define (rtl-dump obj port)
   (dump obj port 0)
   (newline port))

;*---------------------------------------------------------------------*/
;*    dump-margin ...                                                  */
;*---------------------------------------------------------------------*/
(define (dump-margin p m)
   (let ((mgs '#("" " " "  " "   " "    " "     " "      " "       ")))
      (if (<fx m (vector-length mgs))
	  (display (vector-ref mgs m) p)
	  (display (make-string m #\space) p))))

;*---------------------------------------------------------------------*/
;*    dump ::obj ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (dump o p m)
   (cond
      ((or (string? o) (number? o) (symbol? o))
       (display o p))
      ((pair? o)
       (for-each (lambda (o)
		    (dump o p m)
		    (newline p)
		    (if (>fx m 0)
			(dump-margin p m)
			(newline p)))
		 o))
      (else
       (write o p))))

;*---------------------------------------------------------------------*/
;*    dump* ...                                                        */
;*---------------------------------------------------------------------*/
(define (dump* o p m)
   (cond
      ((null? o)
       #unspecified)
      ((null? (cdr o))
       (dump (car o) p m))
      (else
       (let loop ((o o))
	  (dump (car o) p m)
	  (when (pair? (cdr o))
	     (newline p)
	     (dump-margin p m)
	     (loop (cdr o)))))))

;*---------------------------------------------------------------------*/
;*    dump-args ...                                                    */
;*---------------------------------------------------------------------*/
(define (dump-args args p)
   (let loop ((args args))
      (when (pair? args)
	 (let ((a (car args)))
	    (cond
	       ((rtl_reg? a)
		(display " " p)
		(dump a p 0))
	       ((rtl_ins? a)
		(display " " p)
		(dump-ins-rhs a p 0))
	       (else
		(display " " p)
		(display a p)))
	    (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    dump ::block ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (dump o::block p m)
   (with-access::block o (label first)
      (fprint p "(block " label)
      (with-access::block o (preds succs)
	 (dump-margin p (+fx m 1))
	 (fprint p ":preds " (map block-label preds))
	 (dump-margin p (+fx m 1))
	 (fprint p ":succs " (map block-label succs)))
      (dump-margin p (+fx m 1))
      (dump* first p (+fx m 1))
      (display ")\n" p)))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_ins ...                                               */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_ins p m)
   (with-access::rtl_ins o (%spill fun dest args)
      (when dest
	 (display "[" p)
	 (dump dest p m)
	 (display " <- " p))
      (dump-ins-rhs o p m)
      (when (pair? %spill)
	 (display " (" p)
	 (for-each (lambda (r)
		      (display (shape r) p)
		      (display " " p))
	    %spill)
	 (display ")" p))
      (when dest (display "]" p))))

;*---------------------------------------------------------------------*/
;*    dump-ins-rhs ...                                                 */
;*---------------------------------------------------------------------*/
(define (dump-ins-rhs o::rtl_ins p m)
   (with-access::rtl_ins o (fun dest args)
      (display "(" p)
      (dump-fun fun dest args p m)
      (display ")" p)))

;*---------------------------------------------------------------------*/
;*    dump ::rtl_reg ...                                               */
;*---------------------------------------------------------------------*/
(define-method (dump o::rtl_reg p m)
   (display (shape o) p)
   (when *type-shape?*
      (display "::" p)
      (display (shape (rtl_reg-type o)) p)))

;*---------------------------------------------------------------------*/
;*    show-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define (show-fun o p)
   (let ((c (symbol->string (class-name (object-class o)))))
      (display (substring c 4 (string-length c)) p)))
   
;*---------------------------------------------------------------------*/
;*    dump-fun ...                                                     */
;*---------------------------------------------------------------------*/
(define-generic (dump-fun o::rtl_fun dest args p m)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_fun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_fun dest args p m)
   (show-fun o p)
   (dump-args args p))
   
;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadi ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadi dest args p m)
   (show-fun o p)
   (display " " p)
   (display (atom-value (rtl_loadi-constant o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_mov ...                                           */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_mov dest args p m)
   (show-fun o p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadg ...                                         */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadg dest args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_loadg-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_loadfun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_loadfun dest args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_loadfun-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_globalref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_globalref dest args p m)
   (show-fun o p)
   (display " " p)
   (display (shape (rtl_globalref-var o)) p)
   (dump-args args p))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_ifeq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_ifeq dest args p m)
   (with-access::rtl_ifeq o (then)
      (show-fun o p)
      (dump-args args p)
      (display " " p)
      (display (block-label then) p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_ifne ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_ifne dest args p m)
   (with-access::rtl_ifne o (then)
      (show-fun o p)
      (dump-args args p)
      (display " " p)
      (display (block-label then) p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_go ...                                            */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_go dest args p m)
   (with-access::rtl_go o (to)
      (show-fun o p)
      (display " " p)
      (display (block-label to) p)
      (dump-args args p)))

;*---------------------------------------------------------------------*/
;*    dump-fun ::rtl_call ...                                          */
;*---------------------------------------------------------------------*/
(define-method (dump-fun o::rtl_call dest args p m)
   (with-access::rtl_call o (var)
      (display (let ((ou *user-shape?*)
		     (oa *access-shape?*))
		  (set! *user-shape?* #f)
		  (set! *access-shape?* #f)
		  (let ((r (shape var)))
		     (set! *user-shape?* ou)
		     (set! *access-shape?* oa)
		     r))
	       p)
      (dump-args args p)))


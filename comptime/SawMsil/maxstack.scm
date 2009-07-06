(module msil_maxstack
   (import type_type
	   ast_node
	   ast_var
	   saw_defs
	   msil_type )
   (export (maxstack::int l)
	   (generic size-dest-fun::int fun::rtl_fun))
   (static (wide-class stacked::block)) )

(define (maxstack::int l)
   (let ( (max '0) )
      (let ( (setmax! (lambda (n) (if (>fx n max) (set! max n)) n)) )
	 (let dfs ( (b (car l)) (stk '0) )
	    (widen!::stacked b)
	    (for-each (lambda (ins) (set! stk (stack-ins ins stk setmax!)))
		      (block-first b) )
	    (for-each (lambda (s) (if (not (stacked? s)) (dfs s stk)))
		      (block-succs b) ))
	 max )))

(define (stack-ins ins stk max)
   (with-access::rtl_ins ins (dest fun)
      (pop-dest dest fun (stack-expr ins stk max)) ))

(define (stack-expr ins stk max)
   (with-access::rtl_ins ins (dest fun args)
      (let ( (stk (max (push-args fun args stk max))) )
	 (max (push-dest dest fun (pop-args fun args stk))) )))

(define (push-args fun args stk max)
   (set! stk (+fx (extra-args fun args) stk))
   (for-each (lambda (a) (set! stk (push-arg a stk max))) args)
   stk )

(define (push-arg arg stk max)
   (if (not (rtl_reg? arg))
       (stack-expr arg stk max)
       (if (on-stack? arg)
	   stk
	   (+fx stk (typeSize (rtl_reg-type arg))) )))

(define (on-stack? a)
   #f )

(define (pop-args fun args stk)
   (for-each (lambda (a) (set! stk (pop-arg a stk))) args)
   (-fx stk (extra-args fun args)) )

(define (pop-arg arg stk)
   (let ( (reg (if (rtl_reg? arg) arg (rtl_ins-dest arg))) )
      (-fx stk (typeSize (rtl_reg-type reg))) ))

(define (push-dest dest fun stk)
   (+fx stk (if dest (typeSize (rtl_reg-type dest)) (size-dest-fun fun))) )

(define (pop-dest dest fun stk)
   (-fx stk (if dest (typeSize (rtl_reg-type dest)) (size-dest-fun fun))) )

;;
(define-generic (extra-args::int fun::rtl_fun args)
   '0 )

(define-method (extra-args::int fun::rtl_funcall args)
   (if (>fx (length args) 4) 1 0) )

(define-method (extra-args::int fun::rtl_globalref args)
   '2 )

(define-method (extra-args::int fun::rtl_switch args)
   '1 )

(define-method (extra-args::int fun::rtl_call args)
   (let ( (id (global-id (rtl_call-var fun))) )
      (cond
	 ((or (eq? id 'cnst-table-ref)
	      (eq? id 'cnst-table-set!)
	      (eq? id 'make-l-procedure)
	      (eq? id '$obj->bool)
	      (eq? id 'c-null?) )
	  '1 )
	 (else '0) )))

;;
;;
(define-generic (size-dest-fun::int fun::rtl_fun)
   ;; default for function which have an associated type
   (typeSize (type-dest-fun fun)) )

(define-method (size-dest-fun::int fun::rtl_last) 0)
(define-method (size-dest-fun::int fun::rtl_notseq) 0)
(define-method (size-dest-fun::int fun::rtl_effect) 0)

;; CARE not really true...
(define-method (size-dest-fun::int fun::rtl_pure) 1)

(define-method (size-dest-fun::type fun::rtl_new) 1)
(define-method (size-dest-fun::type fun::rtl_apply) 1)
(define-method (size-dest-fun::type fun::rtl_lightfuncall) 1)
(define-method (size-dest-fun::type fun::rtl_funcall) 1)
(define-method (size-dest-fun::type fun::rtl_cast) 1)

;;
;;
(define-generic (type-dest-fun::type fun::rtl_fun)
   ;; CARE nothing is tested in recette
   (error 'size-dest-fun "no method for" (class-name (object-class fun))) )

(define-method (type-dest-fun::type fun::rtl_call)
   (global-type (rtl_call-var fun)) )



;;;     (class rtl_protect::rtl_fun)

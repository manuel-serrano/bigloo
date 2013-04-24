(module saw_expr
   (import type_type
	   ast_var
	   ast_node
	   backend_backend
	   saw_defs
	   tools_shape)
   (export (pre-build-tree ::backend params::pair-nil l::pair-nil)
	   (build-tree ::backend params::pair-nil l::pair-nil)
	   (generic accept-folding? b::backend ins tree))
   (include "SawMill/expr.sch")
   (static (wide-class ireg::rtl_reg index status)
	   (wide-class preg::rtl_reg index status)
	   (wide-class inlined::rtl_ins) ))

;;;
(define (pre-build-tree be::backend params::pair-nil l::pair-nil);()
   (pre-mark-locals params l)
   (set! *count* 0)
   (for-each
    (lambda (b)
       (let ( (l (block-first b)) (moves '()) )
	  (for-each (lambda (ins) (set! moves (pre-analyse be moves ins)))
		    l )))
    l ))

(define (pre-mark-locals params l) ;()
   (let ( (n 0) )
      (define (stat r nstat) (preg-status-set! r nstat))
      (define (reg->preg r)
	 (if (preg? r)
	     (preg-status r)
	     (begin (widen!::preg r (index n) (status 'reset))
		    (set! n (+fx n 1))
		    'reset )))
      (define (reg->preg/read r)
	 (let ( (s (reg->preg r)) )
	    (cond ((eq? s 'reset) (stat r 'readen)) ;CARE no def
		  ((eq? s 'written) (stat r 'ok))
		  (else (stat r 'ko)) )))
      (define (reg->preg/write r)
	 (let ( (s (reg->preg r)) )
	    (cond ((eq? s 'reset) (stat r 'written))
		  ((eq? s 'readen) (stat r 'ok)) ;CARE stange def
		  (else (stat r 'ko)) )))
      (define (visit b::block)
	 (for-each (lambda (ins)
		      (with-access::rtl_ins ins (dest fun args)
			 (if dest (reg->preg/write dest))
			 (for-each reg->preg/read args) ))
		   (block-first b) ))
      (for-each reg->preg/write params)
      (for-each visit l)
;      (print "Before-building-tree : " n " registers.")
      ))

;;;
(define (build-tree be::backend params::pair-nil l::pair-nil);()
   (define (nop)
      (instantiate::rtl_ins (fun (instantiate::rtl_nop)) (args '())) )
   (define (usefull ins)
      (define (x=x ins dest)
	 (let ( (fun (rtl_ins-fun ins)) )
	    (if (rtl_mov? fun)
		(let ( (arg (car (rtl_ins-args ins))) )
		   (if (rtl_ins? arg)
		       (x=x arg dest)
		       (eq? arg dest) ))
		#f )))
      (not (or (inlined? ins) (x=x ins (rtl_ins-dest ins)))) )
   (mark-locals params l)
   (for-each (lambda (b)
		(let ( (l (block-first b)) (moves '()) )
		   (for-each (lambda (ins) (set! moves (analyse be moves ins)))
			     l )
		   (let ( (r (filter! usefull l)) )
		      (block-first-set! b (if (null? r) (list (nop)) r)) )))
	     l ))

(define (mark-locals params l) ;()
   (let ( (n 0) )
      (define (stat r nstat) (ireg-status-set! r nstat))
      (define (reg->ireg r)
	 (if (ireg? r)
	     (ireg-status r)
	     (begin (widen!::ireg r (index n) (status 'reset))
		    (set! n (+fx n 1))
		    'reset )))
      (define (reg->ireg/read r)
	 (let ( (s (reg->ireg r)) )
	    (cond ((eq? s 'reset) (stat r 'readen)) ;CARE no def
		  ((eq? s 'written) (stat r 'ok))
		  (else (stat r 'ko)) )))
      (define (reg->ireg/write r)
	 (let ( (s (reg->ireg r)) )
	    (cond ((eq? s 'reset) (stat r 'written))
		  ((eq? s 'readen) (stat r 'ok)) ;CARE stange def
		  (else (stat r 'ko)) )))
      (define (visit b::block)
	 (for-each (lambda (ins)
		      (with-access::rtl_ins ins (dest fun args)
			 (if dest (reg->ireg/write dest))
			 (for-each reg->ireg/read args) ))
		   (block-first b) ))
      (for-each reg->ireg/write params)
      (for-each visit l)
;      (print "Before-building-tree : " n " registers.")
      ))

;;
(define *count* 0)
(define (pre-analyse b moving ins)
   (let (  (ins-effect (effects (rtl_ins-fun ins))) (args (rtl_ins-args ins)) )
      (define (inline! tree pos)
	 (when (accept-folding? b ins tree)
	    (set! *count* (+fx 1 *count*))
	    (rtl_reg-onexpr?-set! (rtl_ins-dest tree) #t) ))
      (define (walk l r)
	 (cond ((null? l) r)
	       ((memq (rtl_ins-dest (car l)) args)
		=> (lambda (pos)
		      (inline! (car l) pos)
		      (walk (cdr l) r) ))
	       ((conflict? (car l) ins ins-effect)
		;; cannot cross this instruction
		(walk (cdr l) r) )
	       (else (walk (cdr l) (cons (car l) r))) ))
      (let ( (r (walk moving '())) (d (rtl_ins-dest ins)) )
	 ;; Do we add this instruction to the candidats
	 (if (and d
		  (eq? (preg-status d) 'ok)
		  (not (rtl_protect? (rtl_ins-fun ins))) )
	     (cons ins r)
	     r ))))

;;
(define (analyse b moving ins)
   ;; CARE to be rewrited with pre-analysis annotation!!!
   (let (  (ins-effect (effects (rtl_ins-fun ins))) (args (rtl_ins-args ins)) )
      (define (inline! tree pos)
	 (if (accept-folding? b ins tree)
	     (begin
		(set-car! pos tree)
		(if (not (inlined? tree)) (widen!::inlined tree)) )))
      (define (walk l r)
	 ;(print "/* walk " (map ins-name l) " " (length r) " */")
	 (cond ((null? l) r)
	       ((memq (rtl_ins-dest (car l)) args)
		=> (lambda (pos)
		      (inline! (car l) pos)
		      (walk (cdr l) r) ))
	       ((conflict? (car l) ins ins-effect)
		;; cannot cross this instruction
		(walk (cdr l) r) )
	       (else (walk (cdr l) (cons (car l) r))) ))
      ;(print "/* analyse " (ins-name ins) " */")
      (let ( (r (walk moving '())) (d (rtl_ins-dest ins)) )
	 ;; Do we add this instruction to the candidats
	 (if (and d
		  (eq? (ireg-status d) 'ok)
		  (not (rtl_protect? (rtl_ins-fun ins))) )
	     (cons ins r)
	     r ))))

;;
(define-generic (accept-folding? b::backend ins tree)
   #t )

;;
(define (conflict? mover ins ins-effects)
   (let ( (r (xx mover ins ins-effects)) )
;      (if r
;* 	  (print "/* CON " (typeof mover) " " (typeof ins) " *}")      */
;* 	  )                                                            */
      r ))
      
(define (xx mover ins ins-effects)
   (or (memq (rtl_ins-dest ins) (rtl_ins-args mover))
       (matcheffect (effects (rtl_ins-fun mover)) ins-effects)
       (any (lambda (sub) (conflict? sub ins ins-effects))
	    (filter rtl_ins? (rtl_ins-args mover)) )))

;(define (matcheffect f1 f2)
;   (cond
;      ((or (eq? f1 'noeffect) (eq? f2 'noeffect)) ;; one is completly safe
;       #f )
;      ((or (eq? f1 #unspecified) (eq? f2 #unspecified))
;       ;; one may garbage every thing
;       #t )
;      ((and (pair? f1) (pair? f2))       ;; both write on same area
;       (eq? (car f1) (car f2)) )
;      ((pair? f1)                        ;; f1 write where f2 read
;       (eq? (car f1) f2) )
;      ((pair? f2)                        ;; f2 write where f1 read
;       (eq? (car f2) f1) )
;      (else                              ;; ok
;       #f )))

(define (matcheffect f1 f2)
   (define (lintersection? l1 l2)
      (cond ((null? l1) #f)
	    ((memq (car l1) l2) #t)
	    (else (lintersection? (cdr l1) l2)) ))
   (define (intersection? l1 l2)
      (cond ((null? l1) #f)
	    ((null? l2) #f)
	    ((eq? l1 'top) #t)
	    ((eq? l2 'top) #t)
	    (else (lintersection? l1 l2)) ))
   (let ( (w1 (feffect-write f1)) (w2 (feffect-write f2)) )
      (or (intersection? w1 w2)
	  (intersection? (feffect-read f1) w2)
	  (intersection? (feffect-read f2) w1) )))

;;
;; What effect does one instruction
;;
(define *noeffect* (instantiate::feffect))
(define *loadg* (instantiate::feffect (read '(memory))))
(define *storeg* (instantiate::feffect (write '(memory))))
(define *vref* (instantiate::feffect (read '(vector))))
(define *vset* (instantiate::feffect (write '(vector))))
(define *boxref* (instantiate::feffect (read '(box))))
(define *boxset* (instantiate::feffect (write '(box))))
(define *getf* (instantiate::feffect (read '(field))))
(define *setf* (instantiate::feffect (write '(field))))
(define *full* (instantiate::feffect (read 'top) (write 'top)))

(define-generic (effects fun::rtl_fun) *noeffect*);()
(define-method (effects fun::rtl_loadg) *loadg*);()
(define-method (effects fun::rtl_storeg) *storeg*);()
(define-method (effects fun::rtl_vref) *vref*);()
(define-method (effects fun::rtl_vset) *vset*);()
(define-method (effects fun::rtl_boxref) *boxref*);()
(define-method (effects fun::rtl_boxset) *boxset*);()
(define-method (effects fun::rtl_getfield) *getf*);()
(define-method (effects fun::rtl_setfield) *setf*);()
(define-method (effects fun::rtl_apply) *full*);()
(define-method (effects fun::rtl_lightfuncall) *full*);()
(define-method (effects fun::rtl_funcall) *full*);()
(define-method (effects fun::rtl_pragma) *full*);()
(define-method (effects fun::rtl_protect) *full*);()

(define-method (effects fun::rtl_call);
   (let ( (var (rtl_call-var fun)) )
      (or (bs-effect var)
	  (manu-effect var) )))

(define (bs-effect v)
   (getprop (global-id v) (global-module v)) )

(define (manu-effect var)   
   (let ( (fun (global-value var)) )
      (if (fun? fun)
	  (let ( (effect (fun-effect fun)) )
	     (if (feffect? effect)
		 effect
		 (begin '(print (global-id var) " has no effect")
			*full* )))
	  *full* )))

;;
(define-macro (define-effect module id r w)
   `(putprop! ',id ',module (instantiate::feffect (read ',r) (write ',w))) )

(define *pref* (instantiate::feffect (read '(procedure))))
(define *pset* (instantiate::feffect (write '(procedure))))

(define-effect foreign $cons () ())
(define-effect foreign make-fx-procedure () ())
(define-effect foreign procedure-ref (procedure) ())
(define-effect foreign procedure-set! () (procedure))
(define-effect foreign $tvector-descr-set! () (vector))
(define-effect foreign c-current-output-port (output-port) ())
(define-effect foreign c-write-char () (output-port))


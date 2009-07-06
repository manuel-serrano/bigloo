(module jas_peep
   (import jas_classfile jas_stack)
   (export peep **last-number-of-locals** *jas-peephole*) )

(define *jas-peephole* #t)

(define **last-number-of-locals** 0)

(define (peep classfile param locals code)
   (set! **last-number-of-locals** (+fx (length param) (length locals)))
   (if *jas-peephole*
       (let ( (c1 (keeplocals classfile (length param) (length locals) code)) )
	  (simplematch c1)
	  (packlocals classfile (length param) (length locals) c1)
	  (removebranch c1)
	  (removesequence c1)
	  c1 )
       code ))

;;
(define (load? ins)
   (let ( (opcode (car ins)) )
      (and (>=fx opcode 21) (<=fx opcode 25)) ))

(define (load2? ins)
   (let ( (opcode (car ins)) )
      (or (=fx opcode 22) (=fx opcode 24)) ))

(define (store? ins)
   (let ( (opcode (car ins)) )
      (and (>=fx opcode 54) (<=fx opcode 58)) ))

(define (store2? ins)
   (let ( (opcode (car ins)) )
      (or (=fx opcode 55) (=fx opcode 57)) ))

(define (goto? ins)
   (let ( (opcode (car ins)) )
      (=fx opcode 167) ))

(define (iinc? ins)
   (let ( (opcode (car ins)) )
      (=fx opcode 132) ))

(define (ret? ins)
   (let ( (opcode (car ins)) )
      (=fx opcode 169) ))

(define (nocont? ins)
   (let ( (opcode (car ins)) )
      (if (and (>=fx opcode 169) (<=fx opcode 177))
	  #t
	  (memq opcode '(167 191)) )))

(define (huge? ins)
   (let ( (opcode (car ins)) )
      (or (=fx opcode 170) (=fx opcode 171)) ))

(define (single-lab? ins)
   (let ( (opcode (car ins)) )
      (if (and (>=fx opcode 153) (<=fx opcode 168))
	  #t
	  (memq opcode '(198 199)) )))

(define (branch? ins)
   (let ( (opcode (car ins)) )
      (if (and (>=fx opcode 153) (<=fx opcode 171))
	  #t
	  (memq opcode '(198 199)) )))

;;
;; Keep locals in stack
;;
(define (keeplocals classfile nparam nlocals code)
   (let* ( (n (+fx nparam nlocals))
	   (w (make-vector n 0))
	   (r (make-vector n 0))
	   (candidate (make-vector n #t)) )
      (define (init-write i)
	 (if (<fx i nparam)
	     (begin (vector-set! w i 1) (init-write (+fx i 1)))
	     'ok ))
      (define (incr v i) (vector-set! v i (+fx 1 (vector-ref v i))))
      (define (get-access ins)
	 (if (load? ins) (incr r (cadr ins))
	     (if (store? ins) (incr w (cadr ins))
		 (if (ret? ins) (incr r (cadr ins))
		     (if (iinc? ins)
			 (begin (incr w (cadr ins))
				(incr r (cadr ins)) ))))))
      (define (set-candidate i)
	 (if (<fx i n)
	     (begin
		(vector-set! candidate i
			     (and (=fx (vector-ref w i) 1)
				  (=fx (vector-ref r i) 1) ))
		(set-candidate (+fx i 1)) )
	     'ok ))
      (define (find-load v p l sp)
	 (if (or (null? l) (symbol? (car l)))
	     #f
	     (let ( (ins (car l)) )
		(if (and (load? ins) (=fx (cadr ins) v))
		    (if (=fx sp 0) p #f)
		    (let* ( (down (-fx sp (nbpop classfile ins)))
			    (up (+fx down (nbpush classfile ins))) )
		       (if (<fx down 0)
			   #f
			   (if (branch? ins)
			       #f
			       (find-load v l (cdr l) up) )))))))
      (define (removed? p l)
	 (let ( (ins (car l)) )
	    (if (store? ins)
		(let ( (v (cadr ins)) )
		   (if (vector-ref candidate v)
		       (let ( (pp (find-load v l (cdr l) 0)) )
			  (if pp
			      (begin
				 (set-cdr! pp (cddr pp))
				 (set-cdr! p (cdr l))
				 #t )
			      #f ))
		       #f ))
		#f )))
      (define (walk p l)
	 (cond ((null? l)         'ok)
	       ((symbol? (car l)) (walk l (cdr l)))
	       ((removed? p l)    (walk p (cdr p)))
	       (else              (walk l (cdr l))) ))
      (init-write 0)
      (for-each (lambda (x) (if (pair? x) (get-access x))) code)
      (set-candidate 0)
      (let ( (hook (cons 'dummy code)) )
	 (walk hook code)
	 (cdr hook) )))

;;
;; remove unused locals
;;
(define (packlocals classfile nparam nlocals code)
   (let* ( (n (+fx nparam nlocals))
	   (access (make-vector n #f))
	   (mapping (make-vector n 0)) )
      (define (init-write i)
	 (if (<fx i nparam)
	     (begin (vector-set! access i #t) (init-write (+fx i 1)))
	     'ok ))
      (define (get-access ins)
	 (if (or (load? ins) (store? ins) (ret? ins) (iinc? ins))
	     (begin
		(vector-set! access (cadr ins) #t)
		(if (or (load2? ins) (store2? ins))
		    (vector-set! access (+fx (cadr ins) 1) #t) ))))
      (define (set-mapping free i)
	 (if (<fx i n)
	     (if (vector-ref access i)
		 (begin
		    (vector-set! mapping i free)
		    (set-mapping (+fx free 1) (+fx i 1)) )
		 (begin
		    (vector-set! mapping i -1)
		    (set-mapping free (+fx i 1)) ))
	     free ))
      (define (walk1 ins)
	 (cond
	    ((or (load? ins) (store? ins) (ret? ins) (iinc? ins))
	     (set-car! (cdr ins) (vector-ref mapping (cadr ins))) )
	    ((eq? (car ins) 205)
	     (let ( (adr (cdr (cddddr ins))) )
		(let ( (ivar (car adr)) )
		   (if (vector-ref access ivar)
		       (set-car! adr (vector-ref mapping ivar))
		       (set-car! ins 204) ))))))
      (define (walk l)
	 (cond ((null? l)         'ok)
	       ((symbol? (car l)) (walk (cdr l)))
	       (else              (walk1 (car l))
				  (walk (cdr l))) ))
      (init-write 0)
      (for-each (lambda (x) (if (pair? x) (get-access x))) code)
      (let ( (really-used (set-mapping 0 0)) )
	 (set! **last-number-of-locals** really-used)
	 (walk code)
	 really-used )))

;;
;; Simple 
;;
(define (simplematch code)
   (define (walk l)
      (match-case l
	 (() 'ok)
	 (((153 ?true) (3) (167 ?retest) ?true (4) ?retest (153 ?lab) . ?-)
	  ;; Le JIT semble le faire deja
	  (let ( (p (cdddr (cdddr l))) (c (gensym 'h)) )
	     (set-car! l (list 154 lab))
	     (set-car! (cdr l) (list 167 c))
	     (set-cdr! p (cons c (cdr p)))
	     (walk p) ))
	 (((3) (153 ?lab) . ?-) ;; ICONST_0; IFEQ lab => GOTO lab
	  (set-car! l `(167 ,lab))
	  (walk (cdr l)) )
	 ((?prev (4) (153 ?lab) . ?-) ;; ICONST_1; IFEQ lab => NOP
	  (set-cdr! l (cdddr l))
	  (walk l) )
	 ((?prev (178 . ?-) (87) . ?-) ;; GETSTATIC;POP => NOP
	  (set-cdr! l (cdddr l))
	  (walk l) )
	 ((?prev (178 . ?-) (88) . ?-) ;; GETSTATIC;POP2 => NOP
	  (set-cdr! l (cdddr l))
	  (walk l) )
	 (((9) (136) . ?-) ;; LCONST_0;L2I => ICONST_0
	  ;; CARE Must be more generale (LDC2 ??) (L2I) -> (LDC ??)
	  (set-car! l '(3))
	  (set-cdr! l (cddr l))
	  (walk (cddr l)) )
	 (((10) (136) . ?-) ;; LCONST_1;L2I -> ICONST_1
	  (set-car! l '(4))
	  (set-cdr! l (cddr l))
	  (walk (cddr l)) )
	 (else (walk (cdr l))) ))
   (walk code) )

;;
;; Branch tensioning
;;
(define (removebranch code)
   (let ( (done '()) )
      (define (collect l from r n)
	 (cond ((or (null? l) (>fx n 5) (eq? (car l) from)) #f)
	       ((symbol? (car l)) (collect (cdr l) from r n))
	       ((huge? (car l)) #f)
	       ((nocont? (car l)) (reverse! (cons (car l) r)))
	       (else (collect (cdr l) from (cons (car l) r) (+fx n 1))) ))
      (define (walk-at-lab lab)
	 (walk-from lab (cdr (memq lab code))) )
      (define (walk-from from l)
	 (if (memq from done)
	     'done
	     (begin (set! done (cons from done))
		    (walk from l) )))
      (define (walk from l)
	 (cond ((null? l) 'done)
	       ((symbol? (car l))
		(walk-from (car l) (cdr l)) )
	       ((goto? (car l))
		(let ( (lab (cadar l))  (next (cdr l)) )
		   ;; insure lab is done to avoid loop
		   (walk-at-lab lab)
		   (let ( (dup (collect (memq lab code) from '() 0)) )
		      (if dup
			  (begin (set! dup (map (lambda (x) (append x '()))
						dup ))
				 (set-car! l (car dup))
				 (set-cdr! l (append (cdr dup) next)) ))
		      (walk from next) )))
	       (else (walk from (cdr l))) ))
      (walk 'begin code) ))

;;
;; Dead code elimination
;;
(define (removesequence code)
   (define (make-labenv code)
      (let loop ((code code)
		 (acc '()))
	 (cond ((null? code) acc)
	       ((symbol? (car code))
		(loop (cdr code) (cons (cons (car code) 0) acc)))
	       (else
		(loop (cdr code) acc)))))
   (define (count-lab lab env)
      (let ( (slot (assq lab env)) )
	 (set-cdr! slot (+ 1 (cdr slot))) ))
   (define (count-ins ins env)
      (if (and (pair? ins) (single-lab? ins))
	  (count-lab (cadr ins) env)
	  (match-case ins
	     ((170 ?def ?- . ?labs)
	      (count-lab def env)
	      (for-each (lambda (lab) (count-lab lab env)) labs) )
	     ((171 ?def . ?table)
	      (count-lab def env)
	      (for-each (lambda (slot) (count-lab (cdr slot) env))
			table ))
	     ((205 ?beg ?end . ?-)
	      (count-lab beg env)
	      (count-lab end env) )
	     ((202 ?beg ?end ?lab ?-)
	      (count-lab beg env)
	      (count-lab end env)
	      (count-lab lab env) ))))
   (let ( (env (make-labenv code)) )
      (define (dead-code prev code)
	 (cond ((null? code)
		(set-cdr! prev '()) )
	       ((symbol? (car code))
		(if (= 0 (cdr (assq (car code) env)))
		    (dead-code prev (cdr code))
		    (begin (set-cdr! prev code)
			   (walk code (cdr code)) )))
	       (else (dead-code prev (cdr code))) ))
      (define (walk prev code)
	 (cond ((null? code) 'ok)
	       ((and (symbol? (car code))
		     (= 0 (cdr (assq (car code) env))) )
		(set-cdr! prev (cdr code))
		(walk prev (cdr code)) )
	       ((and (pair? (car code)) (nocont? (car code)))
		(dead-code code (cdr code)) )
	       (else (walk code (cdr code))) ))
      (for-each (lambda (ins) (count-ins ins env)) code)
      (walk (cons 'dummy code) code) ))

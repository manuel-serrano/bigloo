(module jas_labels
   (import jas_lib jas_classfile)
   (export resolve-labels) )

;;
;; Resolve labels
;;
(define (resolve-labels classfile l)
   (define (fixpoint)
      (if (wide-conditionals classfile l)
	  'ok
	  (fixpoint) ))
   (fixpoint)
   (resolve-definitive-labels classfile l) )



(define (wide-conditionals classfile l)
   (let ( (env (resolve-pc classfile l)) )
      (define (get-target label)
	 (cdr (or (assq label env)
		  (jas-error classfile "unknown label" label) )))
      (define (far label pc)
	 (>fx (abs (-fx (get-target label) pc)) #x7FF0) )
      (define (rev cop)
	 (if (<=fx cop 166)
	     (+fx cop (-fx (*fx 2 (remainderfx cop 2)) 1))
	     (if (=fx cop 198) 199 198) ))
      (define (walk1 l ins pc r)
	 (case (car ins)
	    ((153 154 155 156 157 158 159 160 161 162 163 164 165 166 198 199)
	     (if (far (cadr ins) pc)
		 (let ( (lab (gensym "F")) )
		    (set-car! l `(,(rev (car ins)) ,lab))
		    (set-cdr! l `((200 ,(cadr ins))
				  ,lab
				  . ,(cdr l) ))
		    #f )
		 r ))
	    ((167 168)
	     (if (far (cadr ins) pc)
		 (begin
		    (set-car! ins (+fx 200 (-fx (car ins) 167)))
		    #f )
		 r ))
	    ((202) ; HANDLER
	     (match-case (cdr ins)
		((?beg ?end ?lab ?type)
		 (if (or (>=fx (get-target end) #x10000)
			 (>=fx (get-target lab) #x10000) )
		     (jas-error classfile "far label in handler" ins)
		     r ))))
	    ((205) ; LOCALVAR
	     (match-case (cdr ins)
		((?beg ?end ?name ?type ?index)
		 (if (or (>=fx (get-target beg) #x10000)
			 (>=fx (get-target end) #x10000) )
		     (jas-error classfile "far label in localvar" ins)
		     r ))))
	    (else r) ))
      (define (walk l pc r)
	 (cond ((null? l) r)
	       ((symbol? (car l)) (walk (cdr l) pc r))
	       (else
		(walk (cdr l)
		      (+fx pc (size-ins (car l) pc))
		      (walk1 l (car l) pc r) ))))
      (walk l 0 #t) ))

;;
;; Resolve definitive labels
;;
(define (resolve-definitive-labels classfile l)
   (let ( (env (resolve-pc classfile l)) )
      (define (resolve-label ins pc)
	 (define (get-target label)
	    (cdr (or (assq label env)
		     (jas-error classfile "unknown label" label) )))
	 (define (resolve-target label)
	    (let ( (d (- (get-target label) pc)) )
	       (if (> (abs d) #x7FF0)
		   (jas-error classfile "too far" label)
		   (u2 d) )))
	 (define (resolve-target2 label)
	    (u4 (- (get-target label) pc)) )
	 (case (car ins)
	    ((153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
		  198 199 )
	     (cons (car ins) (resolve-target (cadr ins))) )
	    ((200 201)
	     (cons (car ins) (resolve-target2 (cadr ins))) )
	    ((170) ; TABLESWITCH
	     `(170 ,@(padding pc) ,@(resolve-target2 (cadr ins))
		   ,@(u4 (caddr ins))
		   ,@(u4 (- (+ (caddr ins) (length (cdddr ins))) 1))
		   . ,(apply append (map resolve-target2 (cdddr ins))) ))
	    ((171) ; LOOKUPSWITCH
	     `(171 ,@(padding pc) ,@(resolve-target2 (cadr ins))
		   ,@(u4 (length (cddr ins)))
		   . ,(apply append
			    (map (lambda (x)
				    (append (u4 (car x))
					    (resolve-target2 (cdr x)) ))
				 (cddr ins) ))))
	    ((202) ; HANDLER
	     (match-case (cdr ins)
		((?beg ?end ?lab ?type)
		 (list 202 (get-target beg) (get-target end) (get-target lab)
		       type ))))
	    ((205) ; LOCALVAR
	     (match-case (cdr ins)
		((?beg ?end . ?rest)
		 (let ( (b (get-target beg)) (e (get-target end)) )
		    (cons* 205 b (-fx e b) rest) ))))
	    (else ins) ))
      (define (walk l pc done)
	 (cond ((null? l) (reverse! done))
	       ((symbol? (car l)) (walk (cdr l) pc done))
	       (else
		(walk (cdr l)
		      (+ pc (size-ins (car l) pc))
		      (cons (resolve-label (car l) pc) done) ))))
      (walk l 0 '()) ))

(define (padding pc)
   (case (modulofx pc 4)
      ((0) '(0 0 0))
      ((1) '(0 0))
      ((2) '(0))
      (else '()) ))

;;
;; Assign a program counter to each labels
;;
(define *opcode-size* '#(
; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 3 2 3  ;; 000-019
  3 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ;; 020-039
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1  ;; 040-059
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ;; 060-079
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ;; 080-099
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  ;; 100-119
  1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1  ;; 120-139
  1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3  ;; 140-159
  3 3 3 3 3 3 3 3 3 2 0 0 1 1 1 1 1 1 3 3  ;; 160-179
  3 3 3 3 3 5 0 3 2 3 1 1 3 3 1 1 0 4 3 3  ;; 180-199
  5 5 0 0 0 0                              ;; 200-201
  ))

(define (size-ins ins pc)
   (let ( (size (vector-ref *opcode-size* (car ins))) )
      (if (= size 0)
	  (case (car ins)
	     ((170) ; TABLESWITCH
	      (+ 1 (length (padding pc))
		 (* 4 (length ins)) ))
	     ((171) ; LOOKUPSWITCH
	      (+ 1 (length (padding pc)) 8
		 (* 8 (length (cddr ins))) ))
	     ((196) ; WIDE
	      (if (= (cadr ins) 132) 6 4) )
	     ((202) ; HANDLER
	      0 )
	     ((203) ; LINE
	      0 )
	     ((204) ; COMMENT
	      0 )
	     ((205) ; LOCALVAR
	      0 ))
	  size )))

(define (resolve-pc classfile code)
   (define (analyse-pc l env pc)
      (cond ((null? l)
	     env )
	    ((symbol? (car l))
	     (analyse-pc (cdr l) (cons (cons (car l) pc) env) pc) )
	    (else
	     (analyse-pc (cdr l) env (+ pc (size-ins (car l) pc))) )))
   (analyse-pc code '() 0) )

(module jas_stack
   (import jas_lib jas_classfile)
   (export (stk-analysis classfile code)
	   (nbpush classfile ins)
	   (nbpop classfile ins)) )

;;
;; Stack analysis. Run after cop resolution
;;
(define *pop* '#(
; 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  ;; 000-019
  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  ;; 020-039
  0  0  0  0  0  0  2  2  2  2  2  2  2  2  1  2  1  2  1  1  ;; 040-059
  1  1  1  2  2  2  2  1  1  1  1  2  2  2  2  1  1  1  1  3  ;; 060-079
  4  3  4  3  3  3  3  1  2  1  2  3  2  3  4  2  2  4  2  4  ;; 080-099
  2  4  2  4  2  4  2  4  2  4  2  4  2  4  2  4  1  2  1  2  ;; 100-119
  2  3  2  3  2  3  2  4  2  4  2  4  0  1  1  1  2  2  2  1  ;; 120-139
  1  1  2  2  2  1  1  1  4  2  2  4  4  1  1  1  1  1  1  2  ;; 140-159
  2  2  2  2  2  2  2  0  0  0  1  1  1  2  1  2  1  0  0  *  ;; 160-179
  1  *  *  *  *  *  0  0  1  1  1  1  1  1  1  1  *  *  1  1  ;; 180-199
  0  0  0  0  0  0                                            ;; 200-201
 ))

(define (nbpop classfile ins)
   (let* ( (cop (car ins)) (args (cdr ins)) (n (vector-ref *pop* cop)) )
      (if (eq? n '*)
	  (case cop
	     ((179 181) ;; PUT[FIELD|STATIC]
	      (let ( (n (type-size (field-type (car args)))) )
		 (case cop
		    ((179) n)
		    ((181) (+fx n 1)) )))
	     ((182 183 184 185) ;; INVOKE**
	      (with-access::fun (method-type (car args)) (tret targs)
		 (let ( (n (apply + (map type-size targs))) )
		    (if (=fx cop 184) n (+fx n 1)) )))
	     ((197) ;; MULTIANEWARRAY
	      (cadr args) )
	     (else (jas-error classfile "bad cop for pop" cop)) )
	  n )))

(define *push* '#(
; 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
  0  1  1  1  1  1  1  1  1  2  2  1  1  1  2  2  1  1  1  1  ;; 000-019
  2  1  2  1  2  1  1  1  1  1  2  2  2  2  1  1  1  1  2  2  ;; 020-039
  2  2  1  1  1  1  1  2  1  2  1  1  1  1  0  0  0  0  0  0  ;; 040-059
  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  ;; 060-079
  0  0  0  0  0  0  0  0  0  2  3  4  4  5  6  2  1  2  1  2  ;; 080-099
  1  2  1  2  1  2  1  2  1  2  1  2  1  2  1  2  1  2  1  2  ;; 100-119
  1  2  1  2  1  2  1  2  1  2  1  2  0  2  1  2  1  1  2  1  ;; 120-139
  2  2  1  2  1  1  1  1  1  1  1  1  1  0  0  0  0  0  0  0  ;; 140-159
  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  *  0  ;; 160-179
  *  0  *  *  *  *  0  1  1  1  1  0  1  1  0  0  *  1  0  0  ;; 180-199
  0  1  0  0  0  0                                            ;; 200-201
 ))

(define (nbpush classfile ins)
   (let* ( (cop (car ins)) (args (cdr ins)) (n (vector-ref *push* cop)) )
      (if (eq? n '*)
	  (case cop
	     ((178 180) ;; GET[FIELD|STATIC]
	      (type-size (field-type (car args))) )
	     ((182 183 184 185) ;; INVOKE**
	      (with-access::fun (method-type (car args)) (tret)
		 (type-size tret) ))
	     (else (jas-error classfile "bad cop for push" cop)) )
	  n )))

(define (stk-analysis classfile code)
   (let ( (env (make-stk-env code)) )
      (define (stk-analyse l n maxn)
	 (cond ((null? l)
		(jas-error classfile "return unreachable" l) )
	       ((symbol? (car l))
		(let* ( (slot (assq (car l) env)) (nlab (cdr slot)) )
		   (cond
		      ((=fx nlab -1)
		       (set-cdr! slot n)
		       (stk-analyse (cdr l) n maxn) )
		      ((=fx n nlab)
		       maxn )
		      (else (jas-warning classfile "different stack for label"
					 (list (car l) n nlab) )
			    maxn ))))
	       (else
		(let* ( (down (-fx n (nbpop classfile (car l))))
			(up   (+fx down (nbpush classfile (car l)))) )
		   (if (< down 0)
		       (begin
			  (jas-warning classfile "empty stack" down)
			  (set! maxn (-fx maxn down)) ))
		   (stk-continue (car l) (cdr l) up (max up maxn)) ))))
      (define (stk-continue ins l n maxn)
	 (define (follow lab) (stk-analyse (memq lab code) n maxn))
	 (define (cont l) (stk-analyse l n maxn))
	 (case (car ins)
	    ((153 154 155 156 157 158 159 160 161 162 163 164 165 166 198 199)
	     (max (cont l) (follow (cadr ins))) )  ;; All Ifs
	    ((167 200) ; GOTO
	     (follow (cadr ins)) )
	    ((168 201)       ;; JSR CARE suppose a clean use
	     (max (stk-analyse l (- n 1) maxn)
		  (follow (cadr ins)) ))
	    ((169 191) ; RET/THROW
	     maxn )
	    ((170) ; TABLESWITCH
	     (match-case (cdr ins)
		((?def ?beg . ?labs)
		 (apply max (cons (follow def) (map follow labs))) )))
	    ((171) ; LOOKUPSWITCH
	     (match-case (cdr ins)
		((?def . ?table)
		 (apply max (cons (follow def)
				  (map (lambda (slot) (follow (cdr slot)))
				       table ))))))
	    ((172 173 174 175 176 177) ; XRETURN
	     (if (not (=fx n 0)) (jas-warning classfile "stack not empty" n))
	     maxn )
	    ((202) ; HANDLER
	     (max (cont l) (stk-analyse (memq (cadddr ins) code) 1 maxn)) )
	    (else (cont l)) ))
      (stk-analyse code 0 0) ))

(define (make-stk-env l)
   (cond ((null? l) l)
	 ((symbol? (car l)) (cons (cons (car l) -1) (make-stk-env (cdr l))))
	 (else (make-stk-env (cdr l))) ))

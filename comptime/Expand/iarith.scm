;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Expand/iarith.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 26 09:16:56 1994                          */
;*    Last change :  Fri Mar 23 19:11:17 2018 (serrano)                */
;*    Copyright   :  1994-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Les expandeurs arithmetiques (entiers)                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_iarithmetique
   (import tools_misc)
   (export (expand-eq? ::obj ::procedure)
	   (expand-i+ ::obj ::procedure)
	   (expand-i- ::obj ::procedure)
	   (expand-i* ::obj ::procedure)
	   (expand-i/ ::obj ::procedure)
	   (expand-i= ::obj ::procedure)
	   (expand-i< ::obj ::procedure)
	   (expand-i> ::obj ::procedure)
	   (expand-i<= ::obj ::procedure)
	   (expand-i>= ::obj ::procedure)
	   (expand-+fx ::obj ::procedure)
	   (expand--fx ::obj ::procedure)
	   (expand-*fx ::obj ::procedure)
	   (expand-/fx ::obj ::procedure)
	   (expand-maxfx ::obj ::procedure)
	   (expand-minfx ::obj ::procedure)
	   (expand-bit-lsh ::obj ::procedure)
	   (expand-bit-lshu32 ::obj ::procedure)
	   (expand-bit-rsh ::obj ::procedure)
	   (expand-bit-rshs32 ::obj ::procedure)
	   (expand-bit-rshu32 ::obj ::procedure)
	   (expand-bit-ursh ::obj ::procedure)
	   (expand-bit-urshu32 ::obj ::procedure)
	   (expand-bit-ors32 ::obj ::procedure))
   (import tools_error))

;*---------------------------------------------------------------------*/
;*    epairify! ...                                                    */
;*---------------------------------------------------------------------*/
(define (epairify! x res)
   (replace! x (epairify-rec res x)))

;*---------------------------------------------------------------------*/
;*    expand-eq? ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-eq? x e)
   (match-case x
      ((?- (and (? (lambda (x) (or (fixnum? x) (char? x)))) ?y) ?y)
       #t)
      ((?- (quote ?y) (quote ?y))
       #t)
      ((?- (quote (? symbol?)) (quote (? symbol?)))
       #f)
      ((?- (? char?) (? char?))
       #f)
      ((?- ?x . (?y . ()))
       `(c-eq? ,(e x e) ,(e y e)))
      (else
       (error #f "Incorrect number of arguments for `eq?'" x))))

;*---------------------------------------------------------------------*/
;*    expand-i+ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i+ x e)
   (match-case x
      ((?-)
       0)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (+fx x y))
	  (else
	   (e `(+fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(+fx ,x (+ ,@y)) e)))) 
      
;*---------------------------------------------------------------------*/
;*    expand-i- ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i- x e)
   (match-case x
      ((?- . (?x . ()))
       (cond
	  ((fixnum? x)
	   (negfx x))
	  (else
	   `(negfx ,(e x e)))))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (-fx x y))
	  (else
	   (e `(-fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(-fx ,x (+ ,@y)) e)))) 
      
;*---------------------------------------------------------------------*/
;*    expand-i* ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i* x e)
   (match-case x
      ((?-)
       1)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (*fx x y))
	  (else
	   (e `(*fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(*fx ,x (* ,@y)) e)))) 
       
;*---------------------------------------------------------------------*/
;*    expand-i/ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i/ x e)
   (match-case x
      ((?- . (?a . ()))
       (user-warning "/"
		     "Turning \"(/ ...)\" into \"(/fx ...)\" which may result in precision penalty"
		     x)
       `(/fx 1 ,(e a e)))
      ((?- ?a . (?y . ()))
       (cond
	  ((and (fixnum? a) (fixnum? y) (not (=fx y 0)))
	   (user-warning "/"
	      (string-append "Turning \"(/ "
		 (number->string a)
		 " "
		 (number->string y)
		 ")\" into \""
		 (number->string (/fx a y))
		 "\" which may result in precision penalty")
	      x)
	   (/fx a y))
	  (else
	   (user-warning "/"
	      "Turning \"(/ ... ...)\" into \"(/fx ... ...)\" which may result in precision penalty"
	      x)
	   (e `(/fx ,a ,y) e))))
      ((?- ?a . ?y)
       (user-warning "/"
		     "Turning \"(/ ... ... ...)\" into \"(/fx ... (* ... ...))\" which may result in precision penalty"
		     x)
       (e `(/fx ,a (* ,@y)) e)))) 
      
;*---------------------------------------------------------------------*/
;*    expand-i= ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i= x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (=fx x y))
	  (else
	   (e `(=fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(and (=fx ,x ,(car y)) (= ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-i< ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i< x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (<fx x y))
	  (else
	   (e `(<fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(and (<fx ,x ,(car y)) (< ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-i> ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-i> x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (>fx x y))
	  (else
	   (e `(>fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(and (>fx ,x ,(car y)) (> ,@y)) e))))
     
;*---------------------------------------------------------------------*/
;*    expand-i<= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-i<= x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (<=fx x y))
	  (else
	   (e `(<=fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(and (<=fx ,x ,(car y)) (<= ,@y)) e))))
      
;*---------------------------------------------------------------------*/
;*    expand-i>= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-i>= x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (>=fx x y))
	  (else
	   (e `(>=fx ,x ,y) e))))
      ((?- ?x . ?y)
       (e `(and (>=fx ,x ,(car y)) (>= ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-+fx ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-+fx x e)
   (match-case x
      ((?- ?a . (?b . ()))
       (cond
	  ((and (fixnum? a) (fixnum? b))
	   (+fx a b))
	  (else
	   (set-car! (cdr x) (e a e))
	   (set-car! (cddr x) (e b e))
	   x)))
      (else
       (error #f "Incorrect number of arguments for `+fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand---fx ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand--fx x e)
   (match-case x
      ((?- ?a . (?b . ()))
       (cond
	  ((and (fixnum? a) (fixnum? b))
	   (-fx a b))
	  (else
	   (set-car! (cdr x) (e a e))
	   (set-car! (cddr x) (e b e))
	   x)))
      (else
       (error #f "Incorrect number of arguments for `-fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand--*fx ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-*fx x e)
   (match-case x
      ((?- ?a . (?b . ()))
       (cond
	  ((and (fixnum? a) (fixnum? b))
	   (let ((r (* a b)))
	      (if (and (fixnum? r) (<fx r (bit-lsh 1 29)))
		  ;; optimize only when the result is a small fixnum
		  r
		  (epairify! x `(*fx ,(e a e) ,(e b e))))))
	  (else
	   (set-car! (cdr x) (e a e))
	   (set-car! (cddr x) (e b e))
	   x)))
      (else
       (error #f "Incorrect number of arguments for `*fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand--/fx ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-/fx x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y) (not (=fx y 0)))
	   (/fx x y))
	  (else
	   `(/fx ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `/fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand-maxfx ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-maxfx x e)
   (match-case x
      ((?- ?x)
       (e x e))
      ((?- ?a . (?b . ()))
       (cond
	  ((and (fixnum? a) (fixnum? b))
	   (maxfx a b))
	  (else
	   (epairify! x 
	      `(let ((a ,(e a e))
		     (b ,(e b e)))
		  (if (>fx a b) a b))))))
      ((?- ?a . ?b)
       (epairify! x (e `(maxfx ,a (maxfx ,@b)) e)))
      (else
       (error #f "Incorrect number of arguments for `maxfx'" x))))

;*---------------------------------------------------------------------*/
;*    expand-minfx ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-minfx x e)
   (match-case x
      ((?- ?x)
       (e x e))
      ((?- ?a . (?b . ()))
       (cond
	  ((and (fixnum? a) (fixnum? b))
	   (minfx a b))
	  (else
	   (epairify! x 
	      `(let ((a ,(e a e))
		     (b ,(e b e)))
		  (if (<fx a b) a b))))))
      ((?- ?a . ?b)
       (epairify! x (e `(minfx ,a (minfx ,@b)) e)))
      (else
       (error #f "Incorrect number of arguments for `minfx'" x))))

;*---------------------------------------------------------------------*/
;*    expand-bit ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (expand-bit op test)
   `(match-case x
       ((?- (and ?n (? ,test)) (and ?shift (? fixnum?)))
	(if (and (<=fx shift 31)
		 (>= n 0)
		 (>= (,op n shift) 0)
		 (<= (,op n shift) (bit-lsh 1 28)))
	    (,op n shift)
	    (epairify! x (map (lambda (x) (e x e)) x))))
       (else
	(epairify! x (map (lambda (x) (e x e)) x)))))

;*---------------------------------------------------------------------*/
;*    expand-bit-lsh ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-bit-lsh x e) (expand-bit bit-lsh fixnum?))
(define (expand-bit-lshu32 x e) (expand-bit bit-lshu32 uint32?))

(define (expand-bit-rsh x e) (expand-bit bit-rsh fixnum?))
(define (expand-bit-rshs32 x e) (expand-bit bit-rshs32 int32?))
(define (expand-bit-rshu32 x e) (expand-bit bit-rshu32 uint32?))

(define (expand-bit-ursh x e)
   ;;(expand-bit bit-ursh fixnum?)
   (map (lambda (x) (e x e)) x))
(define (expand-bit-urshu32 x e) (expand-bit bit-urshu32 uint32?))
(define (expand-bit-ors32 x e)
   (match-case x
      ((?- (and (?n (? int32?))) (and (?m (? int32?))))
       (bit-ors32 n m))
      (else
       (epairify! x (map (lambda (x) (e x e)) x)))))

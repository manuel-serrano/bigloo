;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/iarith.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 26 09:16:56 1994                          */
;*    Last change :  Sat Nov 27 07:16:37 2010 (serrano)                */
;*    Copyright   :  1994-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Les expandeurs arithmetiques (entiers)                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_iarithmetique
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
	   (expand-maxfx ::obj ::procedure)
	   (expand-minfx ::obj ::procedure))
   (import tools_error))

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
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (+fx x y))
	  (else
	   `(+fx ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `+fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand---fx ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand--fx x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (-fx x y))
	  (else
	   `(-fx ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `-fx'" x))))

;*---------------------------------------------------------------------*/
;*    expand-maxfx ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-maxfx x e)
   (match-case x
      ((?- ?x)
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (maxfx x y))
	  (else
	   `(let ((x ,(e x e))
		  (y ,(e y e)))
	       (if (>fx x y) x y)))))
      ((?- ?x . ?y)
       (e `(maxfx ,x (maxfx ,@y)) e))
      (else
       (error #f "Incorrect number of arguments for `maxfx'" x))))

;*---------------------------------------------------------------------*/
;*    expand-minfx ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-minfx x e)
   (match-case x
      ((?- ?x)
       (e x e))
      ((?- ?x . (?y . ()))
       (cond
	  ((and (fixnum? x) (fixnum? y))
	   (minfx x y))
	  (else
	   `(let ((x ,(e x e))
		  (y ,(e y e)))
	       (if (<fx x y) x y)))))
      ((?- ?x . ?y)
       (e `(minfx ,x (minfx ,@y)) e))
      (else
       (error #f "Incorrect number of arguments for `minfx'" x))))


;*=====================================================================*/
;*    .../prgm/project/bigloo/flt/comptime/Expand/garith.scm.new       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 26 09:16:36 1994                          */
;*    Last change :  Tue Dec 10 09:56:46 2024 (serrano)                */
;*    Copyright   :  1994-2024 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Les expandeurs arithmetiques (generiques)                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_garithmetique
   (import engine_param)
   (export (expand-g+ ::obj ::procedure)
	   (expand-g- ::obj ::procedure)
	   (expand-g* ::obj ::procedure)
	   (expand-g/ ::obj ::procedure)
	   (expand-g= ::obj ::procedure)
	   (expand-g< ::obj ::procedure)
	   (expand-g> ::obj ::procedure)
	   (expand-g<= ::obj ::procedure)
	   (expand-g>= ::obj ::procedure)
	   (expand-gmax ::obj ::procedure)
	   (expand-gmin ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-g-number? ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-g-number? x)
   (or (fixnum? x) (flonum? x) (uint32? x) (int32? x)))

;*---------------------------------------------------------------------*/
;*    expand-g2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g2 x e op)
   
   (define (fx id)
      (cond
	 ((and *arithmetic-overflow* *arithmetic-new-overflow* (memq id '(+ - *)))
	  (symbol-append id 'fx/ov))
	 ((and *arithmetic-overflow* (memq id '(+ - / *)))
	  (symbol-append id 'fx-safe))
	 (else
	  (symbol-append id 'fx))))
   
   (define (fl id)
      (symbol-append id 'fl))
   
   (match-case x
      ((?id (and ?a (? fixnum?)) (and ?b (? symbol?)))
       (let ((nx `(if ($fixnum? ,b)
		      (,(fx id) ,a ,b)
		      (,(symbol-append '|2| id) ,a ,b))))
	  (e nx e)))
      ((?id (and ?a (? symbol?)) (and ?b (? fixnum?)))
       (let ((nx `(if ($fixnum? ,a)
		      (,(fx id) ,a ,b)
		      (,(symbol-append '|2| id) ,a ,b))))
	  (e nx e)))
      ((?id (and ?a (? flonum?)) (and ?b (? symbol?)))
       (let ((nx `(if ($fast-flonum? ,b)
		      (,(fl id) ,a ($fast-real->double ,b))
		      (,(symbol-append '|2| id) ,a ,b))))
	  (e nx e)))
      ((?id (and ?a (? symbol?)) (and ?b (? flonum?)))
       (let ((nx `(if ($fast-flonum? ,a)
		      (,(fl id) ($fast-real->double ,a) ,b)
		      (,(symbol-append '|2| id) ,a ,b))))
	  (e nx e)))
      ((?id (and ?a (or (? fixnum?) (? flonum?))) ?b)
       (let ((bid (gensym 'b)))
	  (let ((nx `(let ((,bid ,b))
			(,id ,a ,bid))))
	     (e nx e))))
      ((?id ?a (and ?b (or (? fixnum?) (? flonum?))))
       (let ((aid (gensym 'a)))
	  (let ((nx `(let ((,aid ,a))
			(,id ,aid ,b))))
	     (e nx e))))
      ((?id (and ?a (? symbol?)) ?a)
       (let ((nx `(if ($fixnum? ,a)
		      (,(fx id) ,a ,a)
		      ,(if *arithmetic-expand-flonum*
			   `(if ($fast-flonum? ,a)
				(,(fl id) ($fast-real->double ,a) ($fast-real->double ,a))
				((@ ,(symbol-append '|2| id) __r4_numbers_6_5) ,a ,a))
			   `((@ ,(symbol-append '|2| id) __r4_numbers_6_5) ,a ,a)))))
	  (e nx e)))
      ((?id (and ?a (? symbol?)) (and ?b (? symbol?)))
       (let ((nx `(if (and ($fixnum? ,a) ($fixnum? ,b))
		      (,(fx id) ,a ,b)
		      ,(if *arithmetic-expand-flonum*
			   `(if (and ($fast-flonum? ,a) ($fast-flonum? ,b) )
				(,(fl id) ($fast-real->double ,a) ($fast-real->double ,b))
				((@ ,(symbol-append '|2| id) __r4_numbers_6_5) ,a ,b))
			   `((@ ,(symbol-append '|2| id) __r4_numbers_6_5) ,a ,b)))))
	  (e nx e)))
      ((?id (and ?a (? symbol?)) ?b)
       (let* ((tmp (gensym 'b))
	      (nx `(let ((,tmp ,b)) (,id ,a ,tmp))))
	  (e nx e)))
      ((?id ?a (and ?b (? symbol?)))
       (let* ((tmp (gensym 'a))
	      (nx `(let ((,tmp ,a)) (,id ,tmp ,b))))
	  (e nx e)))
      ((?id ?a ?b)
       (if  (and (expand-g-number? a) (expand-g-number? b))
	    (apply op x)
	    (let* ((tmpa (gensym 'a))
		   (tmpb (gensym 'b))
		   (nx `(let* ((,tmpa ,a) (,tmpb ,b)) (,id ,tmpa ,tmpb))))
	       (e nx e))))))

;*---------------------------------------------------------------------*/
;*    expand-g+ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g+ x e)
   (match-case x
      ((?-)
       0)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?- . (?- . ()))
       (expand-g2 x e +))
      ((?- ?x . ?y)
       (expand-g2 `(+ ,x ,(e `(+ ,@y) e)) e +))))
      
;*---------------------------------------------------------------------*/
;*    expand-g- ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g- x e)
   (match-case x
      ((?- . (?x . ()))
       (cond
	  ((fixnum? x)
	   (negfx x))
	  ((real? x)
	   (negfl x))
	  (else
	   `(- ,(e x e)))))
      ((?- ?- . (?- . ()))
       (expand-g2 x e -))
      ((?- ?x . ?y)
       (expand-g2 `(- ,x ,(e `(+ ,@y) e)) e -))))
       
;*---------------------------------------------------------------------*/
;*    expand-g* ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g* x e)
   (match-case x
      ((?-)
       1)
      ((?- . (?x . ()))
       (e x e))
      ((?- ?- . (?- . ()))
       (expand-g2 x e *))
      ((?- ?x . ?y)
       (expand-g2 `(* ,x ,(e `(* ,@y) e)) e *))))
      
;*---------------------------------------------------------------------*/
;*    expand-g/ ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g/ x e)
   (match-case x
      ((?- . (?x . ()))
       `(2/ 1 ,(e x e)))
      ((?- ?a . (?b . ()))
       (cond
	  ((and (expand-g-number? a) (expand-g-number? b))
	   (if (= b 0)
	       (e `(2/ ,a ,b) e)
	       (/ a b)))
	  (else
	   (e `(2/ ,a ,b) e))))
      ((?- ?a . ?b)
       (e `(2/ ,a (* ,@b)) e))))
      
;*---------------------------------------------------------------------*/
;*    expand-g= ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g= x e)
   (match-case x
      ((?- ?- . (?- . ()))
       (expand-g2 x e =))
      ((?- ?-)
       (error "=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2= ,x ,(car y)) (= ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-g< ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g< x e)
   (match-case x
      ((?- ?- . (?- . ()))
       (expand-g2 x e <))
      ((?- ?-)
       (error "<" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2< ,x ,(car y)) (< ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-g> ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-g> x e)
    (match-case x
      ((?- ?- . (?- . ()))
       (expand-g2 x e >))
      ((?- ?-)
       (error ">" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2> ,x ,(car y)) (> ,@y)) e))))
     
;*---------------------------------------------------------------------*/
;*    expand-g<= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-g<= x e)
   (match-case x
      ((?- ?- . (?- . ()))
       (expand-g2 x e <=))
      ((?- ?-)
       (error "<=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2<= ,x ,(car y)) (<= ,@y)) e))))
      
;*---------------------------------------------------------------------*/
;*    expand-g>= ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-g>= x e)
    (match-case x
      ((?- ?- . (?- . ()))
       (expand-g2 x e >=))
      ((?- ?-)
       (error ">=" "Illegal form" x))
      ((?- ?x . ?y)
       (e `(and (2>= ,x ,(car y)) (>= ,@y)) e))))

;*---------------------------------------------------------------------*/
;*    expand-gmax ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-gmax x e)
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
	       (2max x y)))))
      ((?- ?x . ?y)
       (e `(2max ,x (max ,@y)) e))
      (else
       (error #f "Incorrect number of arguments for `max'" x))))

;*---------------------------------------------------------------------*/
;*    expand-gmin ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-gmin x e)
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
	       (2min x y)))))
      ((?- ?x . ?y)
       (e `(2min ,x (min ,@y)) e))
      (else
       (error #f "Incorrect number of arguments for `min'" x))))

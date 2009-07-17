;*=====================================================================*/
;*    serrano/prgm/project/bigloo/fthread/src/Llib/env2d.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 13 14:38:14 2002                          */
;*    Last change :  Sat Aug 16 14:11:52 2003 (serrano)                */
;*    Copyright   :  2002-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of fair environments.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ft_env2d
   
   (library pthread)
   
   (import __ft_types
	   __ft_env
	   __ft_signal)
   
   (static (class %ftenv2d::ftenv
	      %width::long
	      %height::long
	      %matrix::vector
	      %last-matrix::vector))
   
   (export (make-ftenv2d::ftenv . args)))

;*---------------------------------------------------------------------*/
;*    make-ftenv2d ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-ftenv2d . args)
   (define (make-env width height)
      (instantiate::%ftenv2d
	 (%width width)
	 (%height height)
	 (%matrix (make-vector (* width height) #f))
	 (%last-matrix (make-vector (* width height) #f))))
   (match-case args
      (()
       (make-env 10 10))
      (((and ?w (? integer?)))
       (make-env w 10))
      (((and ?w (? integer?)) (and ?h (? integer?)))
       (make-env w h))
      (else
       (error "make-ftenv2d" "Illegal options" args))))

;*---------------------------------------------------------------------*/
;*    ftenv-bind! ::%ftenv2d ...                                       */
;*---------------------------------------------------------------------*/
(define-method (ftenv-bind! env::%ftenv2d id sig)
   (with-access::%ftenv2d env (%matrix %width %height)
      (define (matrix-enlarge! neww newh)
	 (let ((oldm %matrix)
	       (newm (make-vector (*fx neww newh)))
	       (newlm (make-vector (*fx neww newh) #f))
	       (leno (* %width %height))
	       (oldw %width))
	    (let loop ((oldo 0)
		       (newo 0))
	       (if (<fx oldo leno)
		   (begin
		      (vector-set! newm newo (vector-ref oldm oldo))
		      (let ((y (/fx oldo oldw))
			    (x (remainderfx oldo oldw)))
			 (loop (+fx oldo 1)
			       (+fx x (*fx y neww)))))
		   (begin
		      (set! %matrix newm)
		      (set! %width neww)
		      (set! %height newh))))))
      (define (matrix-set! x y)
	 (if (or (>=fx x %width) (>= y %height))
	     (matrix-enlarge! (* 2 (max x %width)) (* 2 (max y %height))))
	 (vector-set! %matrix (+fx x (*fx y %width)) sig))
      (match-case id
	 (((and ?x (? fixnum?)) . (and ?y (? fixnum?)))
	  (cond
	     ((<fx x 0)
	      (error "ftenv-bind!(ftenv2d)" "Value out of bound" x))
	     ((<fx y 0)
	      (error "ftenv-bind!(ftenv2d)" "Value out of bound" y))
	     (else
	      (matrix-set! x y))))
	 (else
	  (error "ftenv-bind!" "Illegal identifier" id)))))
  
;*---------------------------------------------------------------------*/
;*    ftenv-lookup ::%ftenv2d ...                                      */
;*---------------------------------------------------------------------*/
(define-method (ftenv-lookup env::%ftenv2d id)
   (with-access::%ftenv2d env (%matrix %width %height)
      (match-case id
	 (((and ?x (? fixnum?)) . (and ?y (? fixnum?)))
	  (if (and (>=fx x 0) (<fx x %width)
		   (>=fx y 0) (<fx x %height))
	      (vector-ref %matrix (+fx x (*fx y %width)))
	      #f))
	 (else
	  (error "ftenv-lookup" "Illegal identifier" id)))))
	   
;*---------------------------------------------------------------------*/
;*    ftenv-last-lookup ::%ftenv2d ...                                 */
;*---------------------------------------------------------------------*/
(define-method (ftenv-last-lookup env::%ftenv2d id)
   (with-access::%ftenv2d env (%last-matrix %width %height)
      (match-case id
	 (((and ?x (? fixnum?)) . (and ?y (? fixnum?)))
	  (if (and (>=fx x 0) (<fx x %width)
		   (>=fx y 0) (<fx x %height))
	      (vector-ref %last-matrix (+fx x (*fx y %width)))
	      #f))
	 (else
	  (error "ftenv-last-lookup" "Illegal identifier" id)))))
	   
;*---------------------------------------------------------------------*/
;*    ftenv-filter! ...                                                */
;*---------------------------------------------------------------------*/
(define-method (ftenv-filter! env::%ftenv2d pred)
   (with-access::%ftenv2d env (%matrix %last-matrix %width %height)
      (let ((len (* %width %height)))
	 (let loop ((o 0))
	    (if (<fx o len)
		(let ((cell (vector-ref %matrix o)))
		   (if (not (pred cell))
		       (vector-set! %matrix o #f))
		   (vector-set! %last-matrix o cell)
		   (loop (+fx o 1))))))))

;*---------------------------------------------------------------------*/
;*    ftenv-handles? ::%ftenv2d ...                                    */
;*---------------------------------------------------------------------*/
(define-method (ftenv-handles? env::%ftenv2d object)
   (match-case object
      (((? integer?) . (? integer?))
       #t)
      (else
       #f)))


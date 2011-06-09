;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/farith.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 23 16:05:55 1995                          */
;*    Last change :  Thu Jun  9 06:49:37 2011 (serrano)                */
;*    Copyright   :  1995-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The flonum expanders.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_farithmetique
   (import type_type
	   ast_ident)
   (export (expand-fmax  ::obj ::procedure)
	   (expand-fmin  ::obj ::procedure)
	   (expand-fatan ::obj ::procedure)
	   (expand-+fl ::obj ::procedure)
	   (expand--fl ::obj ::procedure)
	   (expand-*fl ::obj ::procedure)
	   (expand-/fl ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-fmax ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-fmax x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (real? x) (real? y))
	   (maxfl x y))
	  (else
	   (e `(max-2fl ,x ,y) e))))
      ((?- ?x ?y . ?z)
       (let ((max (mark-symbol-non-user! (gensym 'max))))
	  (e `(let ((,max (max-2fl ,x ,y)))
		 (maxfl ,max ,@z))
	     e)))))

;*---------------------------------------------------------------------*/
;*    expand-fmin ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-fmin x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (real? x) (real? y))
	   (minfl x y))
	  (else
	   (e `(min-2fl ,x ,y) e))))
      ((?- ?x ?y . ?z)
       (let ((min (mark-symbol-non-user! (gensym 'min))))
	  (e `(let ((,min (min-2fl ,x ,y)))
		 (minfl ,min ,@z))
	     e)))))

;*---------------------------------------------------------------------*/
;*    expand-fatan ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-fatan x e)
   (match-case x
      ((?- . (?x . ()))
       (if (real? x)
	   (atanfl x)
	   (e `(atan-1fl ,x) e)))
      ((?- ?x . (?y . ()))
       (if (and (real? x) (real? y))
	   (atanfl x y)
	   (e `(atan-2fl ,x ,y) e)))
      (else
       (error '() "Too many arguments provided" x))))

;*---------------------------------------------------------------------*/
;*    expand-+fl ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-+fl x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (flonum? x) (flonum? y))
	   (+fl x y))
	  (else
	   `(+fl ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `+fl'" x))))

;*---------------------------------------------------------------------*/
;*    expand---fl ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand--fl x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (flonum? x) (flonum? y))
	   (-fl x y))
	  (else
	   `(-fl ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `-fl'" x))))

;*---------------------------------------------------------------------*/
;*    expand--*fl ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-*fl x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (flonum? x) (flonum? y))
	   (*fl x y))
	  (else
	   `(*fl ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `*fl'" x))))

;*---------------------------------------------------------------------*/
;*    expand--/fl ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-/fl x e)
   (match-case x
      ((?- ?x . (?y . ()))
       (cond
	  ((and (flonum? x) (flonum? y) (not (=fl y 0.)))
	   (/fl x y))
	  (else
	   `(/fl ,(e x e) ,(e y e)))))
      (else
       (error #f "Incorrect number of arguments for `/fl'" x))))

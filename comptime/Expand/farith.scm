;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/farith.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 23 16:05:55 1995                          */
;*    Last change :  Mon Mar 21 08:39:21 2011 (serrano)                */
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
	   (expand-fatan ::obj ::procedure)))

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
      


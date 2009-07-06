;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/pptype.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 31 08:28:30 1994                          */
;*    Last change :  Mon May 15 08:07:15 2000 (serrano)                */
;*    Copyright   :  1994-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The type pretty-printer                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_pptype
   (import type_type
	   ast_var)
   (export (function-type->string::bstring ::variable)
	   (variable-type->string::bstring ::variable)))

;*---------------------------------------------------------------------*/
;*    type-id->string ...                                              */
;*---------------------------------------------------------------------*/
(define (type-id->string type)
   (if (not (type? type))
       "_"
       (string-downcase (symbol->string (type-id type)))))

;*---------------------------------------------------------------------*/
;*    function-type->string ...                                        */
;*---------------------------------------------------------------------*/
(define (function-type->string variable)
   (let ((sfun (variable-value variable)))
      (string-append (let loop ((args (sfun-args sfun)))
			(if (null? args)
			    ""
			    (string-append
			     (string-downcase
			      (symbol->string
			       (type-id (local-type (car args)))))
			     (if (null? (cdr args))
				 ""
				 " x ")
			     (loop (cdr args)))))
		     " -> "
		     (string-downcase
		      (symbol->string (type-id (variable-type variable)))))))

;*---------------------------------------------------------------------*/
;*    variable-type->string ...                                        */
;*---------------------------------------------------------------------*/
(define (variable-type->string variable)
   (string-downcase (symbol->string (type-id (variable-type variable)))))

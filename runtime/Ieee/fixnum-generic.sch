;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/fixnum-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 12:33:04 2024                          */
;*    Last change :  Mon Jul 22 12:55:42 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable fixnum implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (integer_to_string::bstring ::long ::long))
   (extern (export integer_to_string "integer_to_string")))

;*---------------------------------------------------------------------*/
;*    letter ...                                                       */
;*---------------------------------------------------------------------*/
(define letters "0123456789abcdefghijklmnopqrstuvwxyz")

;*---------------------------------------------------------------------*/
;*    integer_to_string ...                                            */
;*---------------------------------------------------------------------*/
(define (integer_to_string x radix)
   (let loop ((bits (if (<=fx x 0) 1 0))
	      (ax x))
      (if (=fx ax 0)
	  (let ((aux (make-string bits)))
	     (string-set! aux (-fx bits 1) #a000)
	     (let loop ((bits bits)
			(ax x))
		(unless (=fx ax 0)
		   (string-set! aux (-fx bits 1)
		      (string-ref letters (absfx (remainderfx ax radix))))
		   (loop (-fx bits 1) (/fx ax radix)))
		(when (<fx x 0)
		   (string-set! aux 0 #\-)))
	     aux)
	  (loop (+fx bits 1) (/fx ax radix)))))

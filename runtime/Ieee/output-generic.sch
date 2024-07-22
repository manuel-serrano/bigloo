;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/output-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 15:24:13 2024                          */
;*    Last change :  Mon Jul 22 15:51:47 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable output implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (bgl_write_char::obj ::bchar ::output-port))
   (extern (export bgl_write_char "bgl_write_char")))

;*---------------------------------------------------------------------*/
;*    alpha ...                                                        */
;*---------------------------------------------------------------------*/
(define (alpha n)
   (string-ref "0123456789abcdef" n))

;*---------------------------------------------------------------------*/
;*    char-names ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (char-names)
   `',(apply vector
	 (map (lambda (n)
		 (call-with-output-string
		    (lambda (op)
		       (write (integer->char n) op))))
	    (iota 256))))

;*---------------------------------------------------------------------*/
;*    bgl_write_char ...                                               */
;*---------------------------------------------------------------------*/
(define (bgl_write_char o op)
   (define names (char-names))
   ($display-string (vector-ref names (char->integer o)) op))


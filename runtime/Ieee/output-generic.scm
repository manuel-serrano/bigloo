;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/output-generic.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 15:24:13 2024                          */
;*    Last change :  Mon Jul 22 15:40:32 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable output implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (bgl_write_char::obj ::bchar ::output-port)
	   (bgl_display_char::obj ::ubyte ::output-port))
   (extern (export bgl_write_char "bgl_write_char")
	   (export bgl_display_char "bgl_display_char")))

;*---------------------------------------------------------------------*/
;*    alpha ...                                                        */
;*---------------------------------------------------------------------*/
(define (alpha n)
   (string-ref "0123456789abcdef" n))

;*---------------------------------------------------------------------*/
;*    names ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (names)
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

   (define names (names))

   (vector-ref names (char->integer o)))

;*---------------------------------------------------------------------*/
;*    bgl_display_char ...                                             */
;*---------------------------------------------------------------------*/
(define (bgl_display_char c op)
   (let ((s (make-string 1)))
      (string-set! s 0 c)
      (bgl_display_string s op)))

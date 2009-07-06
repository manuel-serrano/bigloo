;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Write/scheme.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:07:23 1994                          */
;*    Last change :  Sat Mar 24 08:34:09 2001 (serrano)                */
;*    Copyright   :  1994-2001 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Some tools to write Scheme code.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module write_scheme
   (import engine_param)
   (export (write-scheme-file-header ::output-port string)
	   (write-scheme-comment     ::output-port . sexp)))

;*---------------------------------------------------------------------*/
;*    write-scheme-file-header ...                                     */
;*---------------------------------------------------------------------*/
(define (write-scheme-file-header port string)
   (fprint port ";; =========================================================")
   (fprint port ";; " string)
   (fprint port ";; " *bigloo-name*)
   (fprint port ";; " *bigloo-author* "       " *bigloo-date*)
   (fprint port ";; =========================================================")
   (newline port))

;*---------------------------------------------------------------------*/
;*    write-scheme-comment ...                                         */
;*---------------------------------------------------------------------*/
(define (write-scheme-comment port . sexp)
   (cond
      ((null? sexp)
       (fprint port ";;"))
      ((null? (cdr sexp))
       (fprint port ";; " (car sexp)))
      (else
       (apply fprint port ";; " sexp))))


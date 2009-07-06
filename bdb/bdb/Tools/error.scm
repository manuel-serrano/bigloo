;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdb/bdb/Tools/error.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 27 09:50:03 1999                          */
;*    Last change :  Sun Jul 30 08:52:40 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A simple ad-hoc error routine                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_error
   (import engine_param
	   tools_io)
   (export (bdb-error obj proc msg)
	   (bdb-notify-error obj proc msg)
	   (bdb-warning . msg)))

;*---------------------------------------------------------------------*/
;*    bdb-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdb-error obj proc msg)
   (bdb-notify-error obj proc msg))

;*---------------------------------------------------------------------*/
;*    bdb-notify-error ...                                             */
;*---------------------------------------------------------------------*/
(define (bdb-notify-error obj proc msg)
   (define (shape obj)
      (if (string? obj)
	  obj
	  (let ((port (open-output-string)))
	     (write obj port)
	     (close-output-port port))))
   (let ((proc (shape proc))
	 (msg  (shape msg))
	 (obj  (shape obj)))
      (console-error (string-append obj ": " proc " `" msg #"'."))))

;*---------------------------------------------------------------------*/
;*    bdb-warning ...                                                  */
;*---------------------------------------------------------------------*/
(define (bdb-warning . msg)
   (console-error "*** WARNING:")
   (for-each console-error msg))

;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Bee/binvoke.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 28 06:57:54 1999                          */
;*    Last change :  Mon Aug 14 15:48:44 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Invoking the Bee.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bee_invoke
   (import engine_param)
   (export (bee-call   command::bstring . args)
	   (bee-notify command::bstring . args)))

;*---------------------------------------------------------------------*/
;*    bee-call ...                                                     */
;*    -------------------------------------------------------------    */
;*    Call a service from the Bee.                                     */
;*---------------------------------------------------------------------*/
(define (bee-call fun . args)
   ;; first, we construct the command line
   (let ((cmd (match-case args
		 (()
		  (string-append "(" fun ")"))
		 (else
		  (let loop ((cmd (reverse args))
			     (res ")"))
		     (if (null? cmd)
			 (string-append "(" fun " " res)
			 (loop (cdr cmd)
			       (string-append "\"" (car cmd) "\" " res))))))))
      ;; emit the request
      (fprint *bout* cmd)
      ;; read the reply
      (read *bin*)))
	 
;*---------------------------------------------------------------------*/
;*    bee-notify ...                                                   */
;*    -------------------------------------------------------------    */
;*    Call a service from the Bee.                                     */
;*---------------------------------------------------------------------*/
(define (bee-notify fun . args)
   ;; first, we construct the command line
   (let ((cmd (match-case args
		 (()
		  (string-append "(" fun ")"))
		 (else
		  (let loop ((cmd (reverse args))
			     (res ")"))
		     (if (null? cmd)
			 (string-append "(" fun " " res)
			 (loop (cdr cmd)
			       (string-append (car cmd) " " res))))))))
      ;; emit the request
      (fprint *bout* cmd)))
	 

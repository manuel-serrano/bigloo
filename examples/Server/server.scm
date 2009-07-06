;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Server/server.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  8 06:43:34 2003                          */
;*    Last change :  Wed Jan  8 06:45:39 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    A simple server loop accepting mulitple consecutive connections. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module foo (main main))

;*---------------------------------------------------------------------*/
;*    The main loop                                                    */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((n (if (pair? (cdr argv))
                (string->integer (cadr argv))
                10))
	 (s (make-server-socket)))
      (print "s: " s)
      (let loop ((i 0))
         (if (<fx i n)
             (let ((s2 (socket-accept s)))
		(print "i: " i " " s2)
		(print (read-line (socket-input s2)))
		(socket-close s2)
                (loop (+fx i 1)))
	     (socket-shutdown s)))))


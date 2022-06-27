;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Tools/trace.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 15 15:04:42 1992                          */
;*    Last change :  Mon Jun 27 14:36:02 2022 (serrano)                */
;*    Copyright   :  1992-2022 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The trace management                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_trace
   (import engine_param
	   tools_error)
   (export (start-trace level pass)
	   (stop-trace)
	   *trace-port*
	   (trace-satisfy? mask level)
	   (print-trace . args)
	   (trace-tab ::int)))

;*---------------------------------------------------------------------*/
;*    L'initialisation des variables locales                           */
;*---------------------------------------------------------------------*/
(define *trace-port* #f)
(define *trace-pass* 'none)
(define *level*      0)
(define *trace-mode* #t)

(define-macro (make-margin len)
   (let loop ((len len)
	      (acc '()))
      (if (=fx len 0)
	  `',(list->vector (cons "" acc))
	  (loop (-fx len 1) (cons (make-string len #\space) acc)))))

(define *margins*
   (make-margin 10))
     
;*---------------------------------------------------------------------*/
;*    start-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (start-trace level pass)
   (when *trace-mode*
      (let ((trace-name *trace-name*))
	 (set! *trace-pass* pass)
	 (set! *trace-port* (open-output-file trace-name))
	 (if (not (output-port? *trace-port*))
	     (internal-error "start-trace"
		"Can't open trace file"
		trace-name)
	     (begin
		(with-output-to-port *trace-port*
		   (lambda ()
		      (print ";; " *bigloo-name*)" "
		      (print ";; " (current-date))
		      (print ";; " (command-line))))
		(set! *level* level))))))
      
;*---------------------------------------------------------------------*/
;*    stop-trace ...                                                   */
;*---------------------------------------------------------------------*/
(define (stop-trace)
   (when *trace-mode*
      (if (output-port? *trace-port*)
	  (close-output-port *trace-port*))))

;*---------------------------------------------------------------------*/
;*    trace-satisfy? ...                                               */
;*---------------------------------------------------------------------*/
(define (trace-satisfy? pass level)
   (and *trace-mode*
	(or (eq? pass *trace-pass*) (memq pass *additional-traces*))
	(<=fx level *level*)))
	      
;*---------------------------------------------------------------------*/
;*    print-trace ...                                                  */
;*---------------------------------------------------------------------*/
(define (print-trace . exp)
   (when *trace-mode*
      (for-each (lambda (e) (display-circle e *trace-port*)) exp)
      (flush-output-port *trace-port*)))
       
;*---------------------------------------------------------------------*/
;*    trace-tab ...                                                    */
;*---------------------------------------------------------------------*/
(define (trace-tab len)
   (when *trace-mode*
      (if (<fx len (vector-length *margins*))
	  (vector-ref *margins* len)
	  (make-string len #\space))))

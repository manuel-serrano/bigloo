;*=====================================================================*/
;*    .../bigloo/bigloo/api/libbacktrace/src/Llib/backtrace.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec  2 15:51:35 2021                          */
;*    Last change :  Thu Dec 12 17:06:11 2024 (serrano)                */
;*    Copyright   :  2021-24 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bigloo libbacktrace binding                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __libbacktrace

   (option (set! *dlopen-init-gc* #t))

   (export (backtrace-get #!key (depth -1) (start 0))
	   (backtrace-for-each proc::procedure))

   (extern ($backtrace-get::obj (::long ::long)
	      "bgl_backtrace_get")
	   ($backtrace-for-each::void (::procedure)
	      "bgl_backtrace_foreach")))

;*---------------------------------------------------------------------*/
;*    backtrace-get ...                                                */
;*---------------------------------------------------------------------*/
(define (backtrace-get #!key (depth -1) (start 0))
   (unless (fixnum? depth)
      (bigloo-type-error "backtrace-get" 'int depth))
   (unless (fixnum? start)
      (bigloo-type-error "backtrace-get" 'int start))
   ($backtrace-get depth start))

;*---------------------------------------------------------------------*/
;*    backtrace-for-each ...                                           */
;*---------------------------------------------------------------------*/
(define (backtrace-for-each proc)
   (unless (correct-arity? proc 3)
      (error "backtrace-for-each" "wrong procedure arity (3 expected)" proc))
   ($backtrace-for-each proc)
   #unspecified)

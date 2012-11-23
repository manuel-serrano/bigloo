;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pthread/src/Llib/pmutex.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 08:14:49 2004                          */
;*    Last change :  Fri Nov 23 17:33:03 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Mutex implementation.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_mutex

   (include "pmutex.sch")
   
   (export (make-pmutex #!optional (name (gensym 'mutex)))
	   (mutex-specific::obj ::mutex)
	   (mutex-specific-set!::obj ::mutex ::obj)))
	   
;*---------------------------------------------------------------------*/
;*    make-pmutex ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-pmutex #!optional (name (gensym 'mutex)))
   ($pmutex-make name))

;*---------------------------------------------------------------------*/
;*    mutex-specific ...                                               */
;*---------------------------------------------------------------------*/
(define (mutex-specific m)
   ($pmutex-specific m))

;*---------------------------------------------------------------------*/
;*    mutex-specific-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (mutex-specific-set! m v)
   ($pmutex-specific-set! m v)
   v)

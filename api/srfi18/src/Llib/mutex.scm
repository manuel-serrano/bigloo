;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/mutex.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 08:14:49 2004                          */
;*    Last change :  Tue Dec 11 15:16:43 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Mutex implementation.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_mutex

   (include "mutex.sch")
   
   (export (make-srfi18mutex #!optional (name (gensym 'mutex)))))
	   
;*---------------------------------------------------------------------*/
;*    make-srfi18mutex ...                                             */
;*---------------------------------------------------------------------*/
(define (make-srfi18mutex #!optional (name (gensym 'mutex)))
   ($srfi18mutex-make name))


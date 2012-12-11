;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi18/src/Llib/condvar.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 08:14:49 2004                          */
;*    Last change :  Tue Dec 11 17:11:38 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Condvar implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __srfi18_condvar

   (include "condvar.sch")
   
   (export (make-srfi18condvar #!optional (name (gensym 'condvar)))))

;*---------------------------------------------------------------------*/
;*    make-srfi18condvar ...                                           */
;*---------------------------------------------------------------------*/
(define (make-srfi18condvar #!optional (name (gensym 'condvar)))
   ($srfi18condvar-make name))



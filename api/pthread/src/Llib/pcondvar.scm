;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pthread/src/Llib/pcondvar.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  3 08:14:49 2004                          */
;*    Last change :  Fri Nov 23 17:33:28 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The public Posix Condvar implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pth_condvar

   (include "pcondvar.sch")
   
   (export (make-pcondvar #!optional (name (gensym 'condvar)))
	   (condition-variable-specific::obj ::condvar)
	   (condition-variable-specific-set!::obj ::condvar ::obj)))

;*---------------------------------------------------------------------*/
;*    make-pcondvar ...                                                */
;*---------------------------------------------------------------------*/
(define (make-pcondvar #!optional (name (gensym 'condvar)))
   ($pcondvar-make name))

;*---------------------------------------------------------------------*/
;*    condition-variable-specific ...                                  */
;*---------------------------------------------------------------------*/
(define (condition-variable-specific m)
   ($pcondvar-specific m))

;*---------------------------------------------------------------------*/
;*    condition-variable-specific-set! ...                             */
;*---------------------------------------------------------------------*/
(define (condition-variable-specific-set! m v)
   ($pcondvar-specific-set! m v)
   v)


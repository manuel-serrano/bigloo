;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/coercer.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 10 17:27:51 1996                          */
;*    Last change :  Mon May 15 08:06:54 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The coercer structure                                            */
;*=====================================================================*/

(define-struct coercer from to check-op coerce-op)


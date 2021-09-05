;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Cfa/set.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Sep  4 15:11:41 2021                          */
;*    Last change :  Sat Sep  4 15:15:31 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Set structure                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The `set' and `meta-set' structures                              */
;*---------------------------------------------------------------------*/
(define-struct meta-set table compacted-size)
(define-struct large-set the-set meta)

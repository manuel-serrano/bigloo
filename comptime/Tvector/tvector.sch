;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tvector/tvector.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 27 14:29:45 1995                          */
;*    Last change :  Mon May 15 08:06:16 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The structure used by all the `tvector' functions.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    a-tvector                                                        */
;*    -------------------------------------------------------------    */
;*    This structure is used to represente constants tvector. During   */
;*    the construction of the ast, expressions like '#toto(x y ...),   */
;*    are encoded as (ast-kwote (a-tvector toto (x y ...)))            */
;*---------------------------------------------------------------------*/
(define-struct a-tvector
   type
   vector)

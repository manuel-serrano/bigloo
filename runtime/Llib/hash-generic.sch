;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Llib/hash-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:49:25 2024                          */
;*    Last change :  Thu Aug  1 14:44:46 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic portable hash implementation.                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (bgl_obj_hash_number::long ::obj))
   (extern (export bgl_obj_hash_number "bgl_obj_hash_number")))

;*---------------------------------------------------------------------*/
;*    bgl_obj_hash_number ...                                          */
;*---------------------------------------------------------------------*/
(define (bgl_obj_hash_number obj)
   (cond
      ((vector? obj) 17)
      ((object? obj) 27)
      (else 23)))

 

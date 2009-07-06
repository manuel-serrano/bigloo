;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo0.2/Translate/type.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 28 15:10:40 1995                          */
;*    Last change :  Mon Apr  8 17:35:41 1996 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Type structures                                                  */
;*    -------------------------------------------------------------    */
;*    This file supposes Tools/union.sch already included.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    type                                                             */
;*---------------------------------------------------------------------*/
(define-union type

   ;; common fields
   (id c-name $)

   ;; simple type
   (define-node simple-t
      ids)    ;; just a list of identifier

   ;; pointer to
   (define-node pointer-t
      type)   ;; a pointer to ...

   ;; array of
   (define-node array-t
      type)   ;; an array of ...

   ;; struct/union
   (define-node struct-t
      tag
      class
      fields)

   ;; enum
   (define-node enum-t
      tag
      fields)

   ;; function
   (define-node function-t
      to
      from)

   ;; typedef-t
   (define-node typedef-t
      alias)
   )

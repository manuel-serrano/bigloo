;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/class.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 10 08:56:49 2015                          */
;*    Last change :  Sat Feb  7 08:10:36 2026 (serrano)                */
;*    Copyright   :  2015-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Module5 class info                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    class-info ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct class-info
   id depth super kind ctor properties registration expr register-ctor vlength)

;*---------------------------------------------------------------------*/
;*    prop-info ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct prop-info
   id type class defv? ronly? virtual? get set value expr vindex)

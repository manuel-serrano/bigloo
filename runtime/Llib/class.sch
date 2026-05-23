;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/runtime/Llib/class.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 10 08:56:49 2015                          */
;*    Last change :  Sat May 23 07:36:19 2026 (serrano)                */
;*    Copyright   :  2015-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Module5 class info                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    class-info ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct class-info
   id depth super kind ctor properties registration expr register-ctor vproperties)

;*---------------------------------------------------------------------*/
;*    prop-info ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct prop-info
   id type class defv? ronly? virtual? get set value expr vindex info)

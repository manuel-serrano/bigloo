;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/libinfo.sch          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Oct  3 13:29:11 2010                          */
;*    Last change :  Sun Oct  3 13:29:22 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Lib info structure (see runtime/library.scm)                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    libinfo ...                                                      */
;*    -------------------------------------------------------------    */
;*    This structure must be identically defined in                    */
;*    runtime/library.scm                                              */
;*---------------------------------------------------------------------*/
(define-struct libinfo
   id basename version
   init_s init_e
   module_s module_e
   class_s class_e
   init eval srfi)

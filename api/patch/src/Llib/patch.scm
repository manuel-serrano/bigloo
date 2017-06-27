;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/patch/src/Llib/patch.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  2 08:12:54 2017                          */
;*    Last change :  Tue Jun 27 08:47:25 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    PATCH object wrapper                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __patch_patch

   (extern (include "../Clib/bglpatch.h"))

   (extern (type $patch_descr_t* void* "bgl_patch_descr_t *")
	   (macro $patch-descr-nil::$patch_descr_t* "0L")
	   (infix macro $patch-descr-nil?::bool (::$patch_descr_t*) " == 0L")
	   (macro $patch-descr::$patch_descr_t* (::int) "BGL_PATCH_DESCR"))

   (export (class PatchDescr
	      ($descr::$patch_descr_t* read-only)))
	   
   (export (inline patch-bound? ::PatchDescr)
	   (inline patch-set! ::PatchDescr ::obj)))

;*---------------------------------------------------------------------*/
;*    patch-bound? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (patch-bound? obj)
   (with-access::PatchDescr obj ($descr)
      ($patch-descr-nil? $descr)))

;*---------------------------------------------------------------------*/
;*    patch-set! ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (patch-set! descr val)
   (with-access::PatchDescr descr ($descr)
      #f))

   
   


;*    (option (set! *optim-patch?* #t))                                */
;*                                                                     */
;* 	   (type descr* void* "__bgl_patch_descr *")                   */
;* 	   (type ptable void* "__bgl_patch_descr *")                   */
;* 	                                                               */
;* 	   (export dummy-patches "__bgl_patches")                      */
;* 	                                                               */
;* 	   (infix macro $the-patch-table::descr* () "__bgl_patches")   */
;* 	                                                               */
;* 	   (macro $patch-int32-set!::void (ptable long int32) "BGL_PATCH_INT32_SET") */
;* 	   (macro $patch-obj-set!::void (ptable long obj) "BGL_PATCH_OBJ_SET")) */
;*                                                                     */
;*    (static dummy-patches)                                           */
;*                                                                     */
;*    (export (inline the-patch-table::ptable)                         */
;* 	   (inline patch-obj-set!::obj ::ptable ::long ::obj)          */
;* 	   (inline patch-int-set!::long ::ptable ::long ::long)        */
;* 	   (inline patch-set!::obj ::ptable ::long ::obj)))            */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    dummy-patches ...                                                *} */
;* {*---------------------------------------------------------------------*} */
;* (define dummy-patches (pragma::ptable "0L"))                        */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    the-patch-table ...                                              *} */
;* {*---------------------------------------------------------------------*} */
;* (define-inline (the-patch-table)                                    */
;*    ($the-patch-table))                                              */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    patch-obj-set! ...                                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-inline (patch-obj-set! table idx val)                       */
;*    ($patch-obj-set! table idx val)                                  */
;*    val)                                                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    patch-int-set! ...                                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define-inline (patch-int-set! table idx val)                       */
;*    ($patch-int32-set! table idx (fixnum->int32 val))                */
;*    val)                                                             */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    patch-set! ...                                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define-inline (patch-set! table idx val)                           */
;*    (if (fixnum? val)                                                */
;*        (patch-int-set! table idx val)                               */
;*        (patch-obj-set! table idx val)))                             */


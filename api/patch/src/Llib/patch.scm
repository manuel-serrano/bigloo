;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/patch/src/Llib/patch.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  2 08:12:54 2017                          */
;*    Last change :  Tue Jun 27 18:16:10 2017 (serrano)                */
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
	   (macro $patch-obj32!::long (::$patch_descr_t* ::obj) "BGL_PATCH_DESCR32")
	   (macro $patch-long32!::long (::$patch_descr_t* ::long) "BGL_PATCH_DESCR32"))

   (export (class PatchDescr
	      ($descr::$patch_descr_t* read-only)))
	   
   (export (inline patch-bound? ::PatchDescr)
	   (inline patch-obj! ::PatchDescr ::obj)
	   (inline patch-long! ::PatchDescr ::long)))

;*---------------------------------------------------------------------*/
;*    patch-bound? ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (patch-bound? obj)
   (with-access::PatchDescr obj ($descr)
      (not ($patch-descr-nil? $descr))))

;*---------------------------------------------------------------------*/
;*    patch-obj! ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (patch-obj! descr val)
   (with-access::PatchDescr descr ($descr)
      ($patch-obj32! $descr val)
      val))

;*---------------------------------------------------------------------*/
;*    patch-long! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (patch-long! descr val)
   (with-access::PatchDescr descr ($descr)
      ($patch-long32! $descr val)
      val))




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


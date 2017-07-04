;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/patch/src/Llib/patch.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  2 08:12:54 2017                          */
;*    Last change :  Tue Jul  4 12:47:17 2017 (serrano)                */
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
	   (macro $patch-void*32!::long (::$patch_descr_t* ::void*) "BGL_PATCH_DESCR32")
	   (macro $patch-long32!::long (::$patch_descr_t* ::long) "BGL_PATCH_DESCR32"))

   (export (class PatchDescr
	      ($descr::$patch_descr_t* read-only)))
	   
   (export (inline patch-bound? ::PatchDescr)
	   (inline patch-obj! ::PatchDescr ::obj)
	   (inline patch-void*! ::PatchDescr ::void*)
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
;*    patch-void*! ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (patch-void*! descr val)
   (with-access::PatchDescr descr ($descr)
      ($patch-void*32! $descr val)
      val))

;*---------------------------------------------------------------------*/
;*    patch-long! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (patch-long! descr val)
   (with-access::PatchDescr descr ($descr)
      ($patch-long32! $descr val)
      val))

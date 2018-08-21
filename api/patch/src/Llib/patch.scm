;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/api/patch/src/Llib/patch.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  2 08:12:54 2017                          */
;*    Last change :  Tue Aug 21 11:11:18 2018 (serrano)                */
;*    Copyright   :  2017-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    PATCH object wrapper                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __patch_patch

   (extern (include "../Clib/cpatch.h")
	   (include "../Clib/bglpatch.h"))

   (extern (type $patch_descr_t* void* "bgl_patch_descr_t *")
	   (macro $patch-descr-nil::$patch_descr_t* "0L")
	   (infix macro $patch-descr-nil?::bool (::$patch_descr_t*) " == 0L")
	   (macro $patch-obj32!::long (::$patch_descr_t* ::obj)
		  "BGL_PATCH_DESCR32_OBJ")
	   (macro $patch-void*32!::long (::$patch_descr_t* ::void*)
		  "BGL_PATCH_DESCR32_PTR")
	   (macro $patch-long32!::long (::$patch_descr_t* ::long)
		  "BGL_PATCH_DESCR32_LONG")

	   (infix macro $patch-descr-addr::long (::$patch_descr_t*) "->addr")
	   (infix macro $patch-descr-offs::long (::$patch_descr_t*) "->offs")
	   (infix macro $patch-descr-mult::long (::$patch_descr_t*) "->mult")
	   (infix macro $patch-descr-kind::long (::$patch_descr_t*) "->kind"))

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

;*---------------------------------------------------------------------*/
;*    object-print ::PatchDescr ...                                    */
;*---------------------------------------------------------------------*/
(define-method (object-print obj::PatchDescr port print-slot::procedure)
   (with-access::PatchDescr obj ($descr)
      (fprintf port "#|~a addr=~x offs=~a mult=~a kind=~a|"
	 (class-name (object-class obj))
	 ($patch-descr-addr $descr)
	 ($patch-descr-offs $descr)
	 ($patch-descr-mult $descr)
	 ($patch-descr-kind $descr))))
   

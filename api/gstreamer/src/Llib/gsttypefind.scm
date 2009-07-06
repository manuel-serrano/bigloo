;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gsttypefind.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 14:45:56 2008                          */
;*    Last change :  Mon Feb 11 15:14:52 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstTypefind                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gsttypefind

   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)
	    

   (export  (class gst-type-find
	       (%gst-type-find-init)
	       ($builtin::$gst-type-find (default ($gst-type-find-nil))))

	    (%gst-type-find-init ::gst-type-find)
	    ($make-gst-type-find::obj ::$gst-type-find))

   (extern  (export $make-gst-type-find "bgl_gst_type_find_new")))
	    
;*---------------------------------------------------------------------*/
;*    $make-gst-type-find ...                                          */
;*---------------------------------------------------------------------*/
(define ($make-gst-type-find type-find::$gst-type-find)
   (instantiate::gst-type-find
      ($builtin type-find)))

;*---------------------------------------------------------------------*/
;*    %gst-type-find-init ::gst-type-find ...                          */
;*---------------------------------------------------------------------*/
(define (%gst-type-find-init o::gst-type-find)
   (with-access::gst-type-find o ($builtin)
      (when ($gst-element-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-type-find-init)
		   (msg "Illegal gst-type-find")
		   (obj o))))
      ;($gst-type-find-add-finalizer! o $builtin)
      o))

;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstbin.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  1 08:52:59 2008                          */
;*    Last change :  Mon Feb 11 15:11:04 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstBin wrapper                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstbin

   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelement
	    __gstreamer_gstpad
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)

   (export  (class gst-bin::gst-element
	       (%elements::pair-nil (default '())))

	    ($make-gst-bin ::$gst-bin ::obj)

	    (gst-bin-add! ::gst-bin ::gst-element . els)
	    (gst-bin-remove! ::gst-bin ::gst-element . els))

   (extern  (export $make-gst-bin "bgl_gst_bin_new")))

;*---------------------------------------------------------------------*/
;*    %gst-object-init ::gst-bin ...                                   */
;*---------------------------------------------------------------------*/
(define-method (%gst-object-init o::gst-bin)
   (with-access::gst-bin o ($builtin name)
      (when ($gst-object-null? $builtin)
	 (set! $builtin ($gst-element->object ($gst-bin-new name))))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    $make-gst-bin ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-gst-bin bin::$gst-bin finalizer)
   (instantiate::gst-bin
      ($builtin ($gst-bin->object bin))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    gst-bin-add! ...                                                 */
;*---------------------------------------------------------------------*/
(define (gst-bin-add! o::gst-bin el0 . els)
   
   (define (add! el)
      (if ($gst-bin-add! ($gst-bin (gst-element-$builtin o))
			 ($gst-element (gst-element-$builtin el)))
	  ;; We have to store the Bigloo object inside the pipeline
	  ;; otherwise the GC could collect the elements.
	  ;; In addition, gst-bin-remove decrement the ref counter, hence,
	  ;; since we want GST object to be reclaimed only when Bigloo
	  ;; no longer uses it, we manually increment its ref counter when
	  ;; adding an element into a bin.
	  (with-access::gst-bin o (%elements)
	     (%gst-object-ref! el)
	     (set! %elements (cons el %elements)))
	  (raise (instantiate::&gst-error
		    (proc 'gst-bin-add!)
		    (msg "Element cannot be added")
		    (obj el)))))
   
   (add! el0)
   
   (for-each (lambda (el)
		(if (gst-element? el)
		    (add! el)
		    (bigloo-type-error 'gst-bin-add! "gst-element" el)))
	     els)
   
   #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-bin-remove! ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-bin-remove! o::gst-bin el0 . els)
   
   (define (remove! el)
      (if ($gst-bin-remove! ($gst-bin (gst-element-$builtin o))
			    ($gst-element (gst-element-$builtin el)))
	  ;; we have to remove the Bigloo object from the pipeline
	  ;; otherwise the GC won't ever free the element
	  (with-access::gst-bin o (%elements)
	     (set! %elements (remq! el %elements)))
	  (raise (instantiate::&gst-error
		    (proc 'gst-bin-remove!)
		    (msg "Element cannot be removed")
		    (obj el)))))
   
   (remove! el0)
   
   (for-each (lambda (el)
		(if (gst-element? el)
		    (remove! el)
		    (bigloo-type-error 'gst-bin-remove! "gst-element" el)))
	     els)
   
   #unspecified)

   

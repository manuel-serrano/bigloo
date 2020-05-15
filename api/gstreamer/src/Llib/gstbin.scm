;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstbin.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  1 08:52:59 2008                          */
;*    Last change :  Fri Nov 18 18:25:43 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstBin wrapper                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstbin

   (include "gst.sch")
   
   (use	    __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelement
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)

   (import  __gstreamer_gstpad)

   (export  (class gst-bin::gst-element
	       (elements::pair-nil (default '())))

	    ($make-gst-bin ::$gst-bin ::obj)
	    ($gst-bin-elements-set! ::gst-bin ::pair-nil)

	    (gst-bin-add! ::gst-bin ::gst-element . els)
	    (gst-bin-remove! ::gst-bin ::gst-element . els)
	    (gst-bin-get ::gst-bin ::bstring))

   (extern  (export $make-gst-bin "bgl_gst_bin_new")
	    (export $gst-bin-elements-set! "bgl_gst_bin_elements_set")))

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
;*    $gst-bin-elements-set! ...                                       */
;*---------------------------------------------------------------------*/
(define ($gst-bin-elements-set! o el)
   (with-access::gst-bin o (elements)
      (set! elements el)))

;*---------------------------------------------------------------------*/
;*    gst-bin-add! ...                                                 */
;*---------------------------------------------------------------------*/
(define (gst-bin-add! o::gst-bin el0 . els)
   
   (define (add! el)
      (with-access::gst-element o ((obuiltin $builtin))
	 (with-access::gst-element el ((elbuiltin $builtin))
	    (if ($gst-bin-add! ($gst-bin obuiltin) ($gst-element elbuiltin))
		;; We have to store the Bigloo object inside the pipeline
		;; otherwise the GC could collect the elements.
		;; In addition, gst-bin-remove decrement the ref counter, hence,
		;; since we want GST object to be reclaimed only when Bigloo
		;; no longer uses it, we manually increment its ref counter when
		;; adding an element into a bin.
		(with-access::gst-bin o (elements)
		   (%gst-object-ref! el)
		   (set! elements (cons el elements)))
		(raise (instantiate::&gst-error
			  (proc 'gst-bin-add!)
			  (msg "Element cannot be added")
			  (obj el)))))))
   
   (add! el0)
   
   (for-each (lambda (el)
		(if (isa? el gst-element)
		    (add! el)
		    (bigloo-type-error 'gst-bin-add! "gst-element" el)))
	     els)
   
   #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-bin-remove! ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-bin-remove! o::gst-bin el0 . els)
   
   (define (remove! el)
      (with-access::gst-element o ((obuiltin $builtin))
	 (with-access::gst-element el ((elbuiltin $builtin))
	    (if ($gst-bin-remove! ($gst-bin obuiltin) ($gst-element elbuiltin))
		;; we have to remove the Bigloo object from the pipeline
		;; otherwise the GC won't ever free the element
		(with-access::gst-bin o (elements)
		   (set! elements (remq! el elements)))))
	  (raise (instantiate::&gst-error
		    (proc 'gst-bin-remove!)
		    (msg "Element cannot be removed")
		    (obj el)))))
   
   (remove! el0)
   
   (for-each (lambda (el)
		(if (isa? el gst-element)
		    (remove! el)
		    (bigloo-type-error 'gst-bin-remove! "gst-element" el)))
	     els)
   
   #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-bin-get ...                                                  */
;*---------------------------------------------------------------------*/
(define (gst-bin-get o::gst-bin name)
   (with-access::gst-bin o (elements)
      (let loop ((els elements))
	 (when (pair? els)
	    (with-access::gst-element (car els) ((ename name))
	       (if (string=? ename name)
		   (car els)
		   (loop (cdr els))))))))

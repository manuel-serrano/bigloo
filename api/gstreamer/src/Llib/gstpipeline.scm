;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstpipeline.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  1 08:52:59 2008                          */
;*    Last change :  Tue Mar 23 07:55:11 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstPipeline wrapper                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstpipeline
   
   (include "gst.sch")
   
   (import  __gstreamer_gstreamer
	    __gstreamer_gstobject
	    __gstreamer_gstelement
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstpad
	    __gstreamer_gstbus
	    __gstreamer_gstbin
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstmessage)

   (export  (class gst-pipeline::gst-bin
	       (bus
		read-only
		(get
		 (lambda (el)
		    (let ((b ($gst-pipeline-get-bus
			      ($gst-pipeline (gst-pipeline-$builtin el)))))
		       (unless ($gst-bus-null? b)
			  ($make-gst-bus b %gst-object-finalize!)))))))

	    (gst-pipeline-new::gst-pipeline ::bstring)
	    (gst-pipeline-play ::gst-pipeline)
	    ($make-gst-pipeline ::$gst-pipeline ::obj))

   (extern  (export $make-gst-pipeline "bgl_gst_pipeline_new")))

;*---------------------------------------------------------------------*/
;*    %gst-object-init ::gst-pipeline ...                              */
;*---------------------------------------------------------------------*/
(define-method (%gst-object-init o::gst-pipeline)
   (with-access::gst-pipeline o ($builtin)
      (when ($gst-object-null? $builtin)
	 (set! $builtin ($gst-element->object
			 ($gst-pipeline-new
			  (symbol->string (gensym 'pipeline))))))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    $make-gst-pipeline ...                                           */
;*---------------------------------------------------------------------*/
(define ($make-gst-pipeline pipeline::$gst-pipeline finalizer)
   (instantiate::gst-pipeline
      ($builtin ($gst-pipeline->object pipeline))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    gst-pipeline-new ...                                             */
;*    -------------------------------------------------------------    */
;*    This constructor is given for compatibilty with the C API.       */
;*---------------------------------------------------------------------*/
(define (gst-pipeline-new::gst-pipeline name)
   (instantiate::gst-pipeline
      ($builtin ($gst-element->object ($gst-pipeline-new name)))
      ($finalizer %gst-object-finalize!)))

;*---------------------------------------------------------------------*/
;*    gst-pipeline-play ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-pipeline-play pipeline::gst-pipeline)
   (let ((bus (gst-pipeline-bus pipeline))
	 (mtypes (bit-or $gst-message-eos $gst-message-error)))
      (gst-element-state-set! pipeline 'playing)
      (let loop ()
	 (let ((msg (gst-bus-poll bus :types mtypes)))
	    (cond
	       ((gst-message-eos? msg)
		'done)
	       ((gst-message-error? msg)
		(error 'gst-pipeline-play
		       "Cannot play"
		       (gst-message-error-string msg)))
	       (else
		(loop)))))
      (gst-element-state-set! pipeline 'null)))

;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstghostpad.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 14:45:56 2008                          */
;*    Last change :  Sun Jan 25 14:06:21 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstGhostPad                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstghostpad

   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstpad)
	    
   (export  (class gst-ghost-pad::gst-pad
	       (target::gst-pad
		(get (lambda (o)
			($gst-ghost-pad-get-target
			 ($gst-ghost-pad (gst-ghost-pad-$builtin o)))))
		(set (lambda (o v)
			(unless ($gst-ghost-pad-set-target
				 ($gst-ghost-pad (gst-ghost-pad-$builtin o))
				 ($gst-pad (gst-pad-$builtin v)))
			   (raise (instantiate::&gst-error
				     (proc 'gst-ghost-pad-target-set!)
				     (msg "Cannot set target")
				     (obj (list o v)))))))))

	    ($make-gst-ghost-pad::obj ::$gst-ghost-pad))

   (extern  (export $make-gst-ghost-pad "bgl_gst_ghost_pad_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-ghost-pad ...                                          */
;*---------------------------------------------------------------------*/
(define ($make-gst-ghost-pad pad::$gst-ghost-pad)
   (instantiate::gst-ghost-pad
      ($builtin ($gst-ghost-pad->object pad))))

;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstghostpad.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 14:45:56 2008                          */
;*    Last change :  Tue Nov 15 17:03:46 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstGhostPad                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstghostpad

   (include "gst.sch")
   
   (use	    __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)

   (import  __gstreamer_gstpad)
	    
   (export  (class gst-ghost-pad::gst-pad
	       (target::gst-pad
		  (get (lambda (o)
			  (with-access::gst-ghost-pad o ($builtin)
			     ($gst-ghost-pad-get-target
				($gst-ghost-pad $builtin)))))
		  (set (lambda (o v)
			  (with-access::gst-ghost-pad o ($builtin)
			     (with-access::gst-pad v ((pad-builtin $builtin))
				(unless ($gst-ghost-pad-set-target
					   ($gst-ghost-pad $builtin)
					   ($gst-pad pad-builtin))
				   (raise (instantiate::&gst-error
					     (proc 'gst-ghost-pad-target-set!)
					     (msg "Cannot set target")
					     (obj (list o v)))))))))))

	    ($make-gst-ghost-pad::obj ::$gst-ghost-pad))

   (extern  (export $make-gst-ghost-pad "bgl_gst_ghost_pad_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-ghost-pad ...                                          */
;*---------------------------------------------------------------------*/
(define ($make-gst-ghost-pad pad::$gst-ghost-pad)
   (instantiate::gst-ghost-pad
      ($builtin ($gst-ghost-pad->object pad))))

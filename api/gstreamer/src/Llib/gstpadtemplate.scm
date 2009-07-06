;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstpadtemplate.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan 16 12:06:09 2008                          */
;*    Last change :  Sun Jan 25 15:50:58 2009 (serrano)                */
;*    Copyright   :  2008-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstPadTemplate                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstpadtemplate
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstghostpad
	    __gstreamer_gstpad
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)
   
   (export  (class gst-pad-template::gst-object)
	    
	    (class gst-static-pad-template
	       (%gst-static-pad-template-init)
	       ($builtin::$gst-static-pad-template)
	       (name-template::string
		read-only
		(get (lambda (o)
			($gst-static-pad-template-name-template
			  (gst-static-pad-template-$builtin o)))))
	       (direction::symbol
		read-only
		(get (lambda (o)
			($gst-pad-direction->obj
			 ($gst-static-pad-template-direction
			  (gst-static-pad-template-$builtin o))))))
	       (presence::symbol
		read-only
		(get (lambda (o)
			($gst-pad-presence->obj
			 ($gst-static-pad-template-presence
			  (gst-static-pad-template-$builtin o)))))))
	    
	    ($make-gst-pad-template::obj ::$gst-pad-template ::obj)
	    ($make-gst-static-pad-template::obj ::$gst-static-pad-template)
	    
	    (%gst-pad-template-init ::gst-pad-template)
	    (%gst-static-pad-template-init ::gst-static-pad-template))
   
   (extern  (export $make-gst-pad-template "bgl_gst_pad_template_new")
	    (export $make-gst-static-pad-template "bgl_gst_static_pad_template_new")))

;*---------------------------------------------------------------------*/
;*    %gst-pad-template-init ...                                       */
;*---------------------------------------------------------------------*/
(define (%gst-pad-template-init o::gst-pad-template)
   (with-access::gst-pad-template o ($builtin $finalizer)
      (when ($gst-caps-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-pad-template-init)
		   (msg "Illegal gst-pad-template")
		   (obj o))))
      o))

;*---------------------------------------------------------------------*/
;*    $make-gst-pad-template ...                                       */
;*---------------------------------------------------------------------*/
(define ($make-gst-pad-template tpl::$gst-pad-template finalizer)
   (instantiate::gst-pad-template
      ($builtin ($gst-pad-template->object tpl))
      ($finalizer finalizer)))
	    
;*---------------------------------------------------------------------*/
;*    $make-gst-static-pad-template ...                                */
;*---------------------------------------------------------------------*/
(define ($make-gst-static-pad-template tpl::$gst-static-pad-template)
   (instantiate::gst-static-pad-template
      ($builtin tpl)))
	    
;*---------------------------------------------------------------------*/
;*    %gst-static-pad-template-init ::gst-static-pad-template ...      */
;*---------------------------------------------------------------------*/
(define (%gst-static-pad-template-init o::gst-static-pad-template)
   (with-access::gst-static-pad-template o ($builtin)
      (when ($gst-static-pad-template-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-static-pad-template-init)
		   (msg "Illegal gst-static-pad-template")
		   (obj o))))
      o))

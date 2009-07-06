;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstplugin.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:53:19 2008                          */
;*    Last change :  Wed Jan 23 13:41:04 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstPlugin                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstplugin
   
   (include "gst.sch")
   
   (import  __gstreamer_gstreamer
	    __gstreamer_gstobject)
   
   (export  (class gst-plugin::gst-object
	       (name::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-name
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (description::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-description
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (filename::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-filename
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (license::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-license
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (package::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-package
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (origin::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-origin
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (source::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-source
			 ($gst-plugin
			  (gst-plugin-$builtin o))))))
	       (version::string
		read-only
		(get (lambda (o)
			($gst-plugin-get-version
			 ($gst-plugin
			  (gst-plugin-$builtin o)))))))
	    
	    ($make-gst-plugin::obj ::$gst-plugin ::obj))

   (extern  (export $make-gst-plugin "bgl_gst_plugin_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-plugin ...                                             */
;*---------------------------------------------------------------------*/
(define ($make-gst-plugin plugin::$gst-plugin finalizer)
   (instantiate::gst-plugin
      ($builtin ($gst-plugin->object plugin))
      ($finalizer finalizer)))

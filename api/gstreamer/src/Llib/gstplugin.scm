;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstplugin.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:53:19 2008                          */
;*    Last change :  Tue Nov 15 17:35:49 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstPlugin                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstplugin
   
   (include "gst.sch")
   
   (import  __gstreamer_gstobject)
   
   (export  (class gst-plugin::gst-object
	       (name::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-name
				($gst-plugin $builtin))))))
	       (description::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-description
				($gst-plugin $builtin))))))
	       (filename::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-filename
				($gst-plugin $builtin))))))
	       (license::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-license
				($gst-plugin $builtin))))))
	       (package::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-package
				($gst-plugin $builtin))))))
	       (origin::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-origin
				($gst-plugin $builtin))))))
	       (source::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-source
				($gst-plugin $builtin))))))
	       (version::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin o ($builtin)
			     ($gst-plugin-get-version
				($gst-plugin $builtin)))))))
	    
	    ($make-gst-plugin::obj ::$gst-plugin ::obj))

   (extern  (export $make-gst-plugin "bgl_gst_plugin_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-plugin ...                                             */
;*---------------------------------------------------------------------*/
(define ($make-gst-plugin plugin::$gst-plugin finalizer)
   (instantiate::gst-plugin
      ($builtin ($gst-plugin->object plugin))
      ($finalizer finalizer)))

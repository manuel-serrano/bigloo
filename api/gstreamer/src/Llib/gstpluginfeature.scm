;*=====================================================================*/
;*    .../bigloo/api/gstreamer/src/Llib/gstpluginfeature.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:43:18 2008                          */
;*    Last change :  Tue Nov 15 17:23:42 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GltPluginFeature                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstpluginfeature
   
   (include "gst.sch")
   
   (import  __gstreamer_gstobject)
   
   (export  (class gst-plugin-feature::gst-object
	       (name::string
		  (get (lambda (o)
			  (with-access::gst-plugin-feature o ($builtin)
			     ($gst-plugin-feature-name
				($gst-plugin-feature $builtin)))))
		  (set (lambda (o v)
			  (with-access::gst-plugin-feature o ($builtin)
			     ($gst-plugin-feature-set-name!
				($gst-plugin-feature $builtin) v))
			  v)))
	       (plugin-name::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-plugin-feature o ($builtin)
			     ($gst-plugin-feature-get-plugin-name
				($gst-plugin-feature $builtin))))))
	       (rank::uint
		  (get (lambda (o)
			  (with-access::gst-plugin-feature o ($builtin)
			     ($gst-plugin-feature-rank
				($gst-plugin-feature $builtin)))))
		  (set (lambda (o v)
			  (with-access::gst-plugin-feature o ($builtin)
			     ($gst-plugin-feature-set-rank!
				($gst-plugin-feature $builtin) v))
			  v))))

	    ($make-gst-plugin-feature::obj ::$gst-plugin-feature ::obj))

   (extern  (export $make-gst-plugin-feature "bgl_gst_plugin_feature_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-plugin-feature ...                                     */
;*---------------------------------------------------------------------*/
(define ($make-gst-plugin-feature plugin-feature::$gst-plugin-feature finalizer)
   (instantiate::gst-plugin-feature
      ($builtin ($gst-plugin-feature->object plugin-feature))
      ($finalizer finalizer)))
	    

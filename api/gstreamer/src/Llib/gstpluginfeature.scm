;*=====================================================================*/
;*    .../bigloo/api/gstreamer/src/Llib/gstpluginfeature.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:43:18 2008                          */
;*    Last change :  Mon Feb 11 16:54:23 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
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
			($gst-plugin-feature-name
			 ($gst-plugin-feature
			  (gst-plugin-feature-$builtin o)))))
		(set (lambda (o v)
			($gst-plugin-feature-set-name!
			 ($gst-plugin-feature
			  (gst-plugin-feature-$builtin o)) v)
			v)))
	       (plugin-name::string
		read-only
		(get (lambda (o)
			($gst-plugin-feature-plugin-name
			 ($gst-plugin-feature
			  (gst-plugin-feature-$builtin o))))))
	       (rank::uint
		(get (lambda (o)
			($gst-plugin-feature-rank
			 ($gst-plugin-feature
			  (gst-plugin-feature-$builtin o)))))
		(set (lambda (o v)
			($gst-plugin-feature-set-rank!
			 ($gst-plugin-feature
			  (gst-plugin-feature-$builtin o)) v)
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
	    

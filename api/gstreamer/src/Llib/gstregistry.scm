;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstregistry.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 07:05:08 2008                          */
;*    Last change :  Wed Mar 26 16:06:21 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstRegistry                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstregistry

   (include "gst.sch")

   (import  __gstreamer_gstreamer
	    __gstreamer_gstobject
	    __gstreamer_gstelement
	    __gstreamer_gstpad
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelementfactory
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstplugin)

   (export  (class gst-registry::gst-object)
	    
	    (gst-registry-default::gst-registry)
	    (gst-registry-element-factory-list::pair-nil #!optional registry)
	    (gst-registry-plugin-list::pair-nil #!optional registry)
	    (gst-registry-feature-list-by-plugin::pair-nil ::obj #!optional registry)
	    (gst-registry-find-plugin::obj ::string #!optional registry)
	    (gst-registry-find-feature::obj ::string ::long #!optional registry)))

;*---------------------------------------------------------------------*/
;*    default-registry ...                                             */
;*---------------------------------------------------------------------*/
(define default-registry #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-registry-default ...                                         */
;*---------------------------------------------------------------------*/
(define (gst-registry-default)
   (unless (gst-registry? default-registry)
      (set! default-registry
	    (instantiate::gst-registry
	       ($builtin ($gst-registry->object
			  ($gst-registry-get-default))))))
   default-registry)

;*---------------------------------------------------------------------*/
;*    gst-registry-element-factory-list ...                            */
;*---------------------------------------------------------------------*/
(define (gst-registry-element-factory-list #!optional registry)
   ($gst-registry-get-element-factory-list
    (if (gst-registry? registry)
	($gst-registry (gst-registry-$builtin registry))
	($gst-registry-get-default))))
	    
;*---------------------------------------------------------------------*/
;*    gst-registry-plugin-list ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-registry-plugin-list #!optional registry)
   ($gst-registry-get-plugin-list
    (if (gst-registry? registry)
	($gst-registry (gst-registry-$builtin registry))
	($gst-registry-get-default))))

;*---------------------------------------------------------------------*/
;*    gst-registry-feature-list-by-plugin ...                          */
;*---------------------------------------------------------------------*/
(define (gst-registry-feature-list-by-plugin plugin #!optional registry)
   ($gst-registry-get-feature-list-by-plugin
    (if (gst-registry? registry)
	($gst-registry (gst-registry-$builtin registry))
	($gst-registry-get-default))
    (cond
       ((gst-plugin? plugin) (gst-plugin-name plugin))
       ((string? plugin) plugin)
       (else (bigloo-type-error 'gst-registry-feature-list-by-plugin
				"gst-plugin or string"
				plugin)))))

;*---------------------------------------------------------------------*/
;*    gst-registry-find-plugin ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-registry-find-plugin name #!optional registry)
   (let ((plugin ($gst-registry-find-plugin
		  (if (gst-registry? registry)
		      ($gst-registry (gst-registry-$builtin registry))
		      ($gst-registry-get-default))
		  name)))
      (unless ($gst-plugin-null? plugin)
	 ($make-gst-plugin plugin %gst-object-finalize!))))

;*---------------------------------------------------------------------*/
;*    gst-registry-find-feature ...                                    */
;*---------------------------------------------------------------------*/
(define (gst-registry-find-feature name type #!optional registry)
   (let ((feature ($gst-registry-find-feature
		  (if (gst-registry? registry)
		      ($gst-registry (gst-registry-$builtin registry))
		      ($gst-registry-get-default))
		  name
		  type)))
      (unless ($gst-plugin-feature-null? feature)
	 ($make-gst-plugin-feature feature %gst-object-finalize!))))

;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Jan 25 14:05:08 2009 (serrano)                */
;*    Copyright   :  2001-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_makelib

   (library multimedia)
   
   (import __gstreamer_gstreamer
	   __gstreamer_gsterror
	   __gstreamer_gstobject
	   __gstreamer_gstelement
	   __gstreamer_gstbin
	   __gstreamer_gstbuffer
	   __gstreamer_gstbus
	   __gstreamer_gstpipeline
	   __gstreamer_gstplugin
	   __gstreamer_gstpluginfeature
	   __gstreamer_gstelementfactory
	   __gstreamer_gstregistry
	   __gstreamer_gstpad
	   __gstreamer_gstpadtemplate
	   __gstreamer_gstghostpad
	   __gstreamer_gstmessage
	   __gstreamer_gstcaps
	   __gstreamer_gststructure
	   __gstreamer_gsttypefind
	   __gstreamer_gstparse
	   
	   __gstreamer_multimedia_music)

   (eval   (export-all)

	   (class &gst-error)
	   (class &gst-create-error)
	   
	   (class gst-bin)
	   (class gst-buffer)
	   (class gst-bus)
	   (class gst-caps)
	   (class gst-ghost-pad)
	   (class gst-element)
	   (class gst-element-factory)
	   (class gst-message)
	   (class gst-object)
	   (class gst-pad)
	   (class gst-pipeline)
	   (class gst-plugin)
	   (class gst-plugin-feature)
	   (class gst-registry)
	   (class gst-structure)
	   (class gst-type-find)

	   (class gstmusic)))

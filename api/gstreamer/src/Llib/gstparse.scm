;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstparse.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 14 10:42:12 2008                          */
;*    Last change :  Mon Jan 14 11:32:30 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstParse binding                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstparse
   
   (include "gst.sch")
   
   (import  __gstreamer_gstreamer
	    __gstreamer_gstobject
	    __gstreamer_gstpad
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelement)

   (export  (gst-parse-launch::obj ::bstring)
	    (gst-parse-launchv::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    gst-parse-launch ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-parse-launch string)
   ($gst-parse-launch string))

;*---------------------------------------------------------------------*/
;*    gst-parse-launchv ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-parse-launchv args)
   ($gst-parse-launchv args))

;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstparse.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 14 10:42:12 2008                          */
;*    Last change :  Thu Feb 11 11:26:19 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstParse binding                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstparse
   
   (include "gst.sch")
   
   (use	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelement)

   (import  __gstreamer_gstpad)

   (export  (gst-parse-launch::obj ::bstring . ::obj)
	    (gst-parse-launchv::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    gst-parse-launch ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-parse-launch string . obj)
   (if (null? obj)
       ($gst-parse-launch string)
       ($gst-parse-launch (apply format string obj))))

;*---------------------------------------------------------------------*/
;*    gst-parse-launchv ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-parse-launchv args)
   ($gst-parse-launchv args))

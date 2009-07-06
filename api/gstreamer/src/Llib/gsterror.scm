;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gsterror.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb 11 15:08:52 2008                          */
;*    Last change :  Mon Feb 11 15:10:43 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GSTREAMER error classes.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gsterror

   (export (class &gst-error::&error)
	   (class &gst-create-error::&gst-error)))

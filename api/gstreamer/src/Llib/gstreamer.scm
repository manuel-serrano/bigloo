;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstreamer.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 08:29:37 2007                          */
;*    Last change :  Fri Dec 13 12:04:13 2013 (serrano)                */
;*    Copyright   :  2007-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Init and cleanup of GSTREAMER applications.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstreamer

   (option (set! *dlopen-init-gc* #t))
   
   (library pthread)
   
   (include "gst.sch")

   (use	    __gstreamer_gstobject
	    __gstreamer_gststructure)

   (import  __gstreamer_gstcaps
	    __gstreamer_gstmessage)
   
   (with    __gstreamer_gstobject
	    __gstreamer_gststructure
	    __gstreamer_gstcaps
	    __gstreamer_gstmessage)
	   
   (extern (include "bglgst_config.h")
	   (macro $configure-gstreamer-version::string "BGL_GSTREAMER_VERSION")
	   (macro $configure-gstreamer-audio::bool "BGL_GSTREAMER_HAVE_AUDIO")
	   ($bglgst-use-threads?::bool () "bglgst_use_threadsp")
	   (export %gst-lock! "bgl_gst_lock")
	   (export %gst-unlock! "bgl_gst_unlock")
	   (export %gst-signal "bgl_gst_signal"))
   
   (export (gst-version::string)
	   (%gst-thread-init!)
	   (%gst-lock!)
	   (%gst-unlock!)
	   (%gst-signal)))

;*---------------------------------------------------------------------*/
;*    *gst-mutex* ...                                                  */
;*---------------------------------------------------------------------*/
(define *gst-mutex* (make-mutex 'gstreamer))
(define *gst-condv* (make-condition-variable 'gstreamer))

;*---------------------------------------------------------------------*/
;*    %gst-lock! ...                                                   */
;*---------------------------------------------------------------------*/
(define (%gst-lock!)
   (mutex-lock! *gst-mutex*))

;*---------------------------------------------------------------------*/
;*    %gst-unlock! ...                                                 */
;*---------------------------------------------------------------------*/
(define (%gst-unlock!)
   (mutex-unlock! *gst-mutex*))

;*---------------------------------------------------------------------*/
;*    %gst-signal ...                                                  */
;*---------------------------------------------------------------------*/
(define (%gst-signal)
   (condition-variable-signal! *gst-condv*))

;*---------------------------------------------------------------------*/
;*    *gst-thread* ...                                                 */
;*---------------------------------------------------------------------*/
(define *gst-thread* #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-initializedp ...                                             */
;*---------------------------------------------------------------------*/
(define gst-initializedp #f)

;*---------------------------------------------------------------------*/
;*    %gst-thread-init! ...                                            */
;*---------------------------------------------------------------------*/
(define (%gst-thread-init!)
   (unless ($bglgst-use-threads?)
      (unless (isa? *gst-thread* thread)
	 ;; the thread in charge of executing all callbacks
	 (set! *gst-thread*
	       (instantiate::pthread
		  (name "gst")
		  (body (lambda ()
			   (synchronize *gst-mutex*
			      (let loop ()
				 (condition-variable-wait! *gst-condv* *gst-mutex*)
				 ($gst-invoke-callbacks)
				 (loop)))))))
	 (thread-start! *gst-thread*))))

;*---------------------------------------------------------------------*/
;*    gst-init ...                                                     */
;*---------------------------------------------------------------------*/
(define (gst-init)
   (unless gst-initializedp
      (set! gst-initializedp #t)
      (let ((gargs (member "--" (command-line))))
	 ($gst-init (if (pair? gargs) (cdr gargs) '())))
      (bigloo-configuration-add-entry! 'gstreamer-version
				       $configure-gstreamer-version)
      (bigloo-configuration-add-entry! 'gstreamer-have-audio
				       $configure-gstreamer-audio)))

;*---------------------------------------------------------------------*/
;*    gst-version ...                                                  */
;*---------------------------------------------------------------------*/
(define (gst-version)
   ($gst-version))

;*---------------------------------------------------------------------*/
;*    Force the initialization                                         */
;*---------------------------------------------------------------------*/
(gst-init)

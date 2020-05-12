;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstbus.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 12:10:42 2008                          */
;*    Last change :  Tue Nov 15 11:23:54 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstBus                                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstbus
   
   (include "gst.sch")
   
   (use	    __gstreamer_gstobject
	    __gstreamer_gststructure)

   (import  __gstreamer_gstmessage)
   
   (export  (class gst-bus::gst-object)

	    ($make-gst-bus ::$gst-bus ::obj)

	    (gst-bus-post ::gst-bus ::gst-message)
	    (gst-bus-peek ::gst-bus)
	    (gst-bus-pop ::gst-bus)
;* 	    (gst-bus-pop-filtered ::gst-bus ::$gst-message-type)       */
	    (gst-bus-poll ::gst-bus
			  #!key
			  (types $gst-message-any)
			  (timeout #l-1))
	    (gst-bus-sync-handler-set! ::gst-bus ::procedure))

   (extern  (export $make-gst-bus "bgl_gst_bus_new")))

;*---------------------------------------------------------------------*/
;*    %gst-object-init ::gst-bus ...                                   */
;*---------------------------------------------------------------------*/
(define-method (%gst-object-init o::gst-bus)
   (with-access::gst-bus o ($builtin)
      (when (eq? $builtin ($gst-object-nil))
 	 (set! $builtin ($gst-bus->object ($gst-bus-new))))
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    $make-gst-bus ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-gst-bus bus::$gst-bus finalizer)
   (instantiate::gst-bus
      ($builtin ($gst-bus->object bus))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    gst-bus-post ...                                                 */
;*---------------------------------------------------------------------*/
(define (gst-bus-post o::gst-bus msg::gst-message)
   (with-access::gst-message msg ((mbuiltin $builtin))
      (with-access::gst-bus o ((obuiltin $builtin))
	 ($gst-message-ref! mbuiltin)
	 ($gst-bus-post ($gst-bus obuiltin) mbuiltin))))

;*---------------------------------------------------------------------*/
;*    gst-bus-peek ...                                                 */
;*---------------------------------------------------------------------*/
(define (gst-bus-peek o::gst-bus)
   (with-access::gst-bus o ((obuiltin $builtin))
      (let ((msg::$gst-message ($gst-bus-peek ($gst-bus obuiltin))))
	 (if ($gst-message-null? msg)
	     #f
	     ($make-gst-message msg #t)))))

;*---------------------------------------------------------------------*/
;*    gst-bus-pop ...                                                  */
;*---------------------------------------------------------------------*/
(define (gst-bus-pop o::gst-bus)
   (with-access::gst-bus o ((obuiltin $builtin))
      (let ((msg::$gst-message ($gst-bus-pop ($gst-bus obuiltin))))
	 (if ($gst-message-null? msg)
	     #f
	     ($make-gst-message msg #f)))))

;* {*---------------------------------------------------------------------*} */
;* {*    gst-bus-pop-filtered ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define (gst-bus-pop-filtered o::gst-bus types)                     */
;*    (let ((msg::$gst-message                                         */
;* 	  ($gst-bus-pop-filtered ($gst-bus (gst-bus-$builtin o)) types))) */
;*       (if ($gst-message-null? msg)                                  */
;* 	  #f                                                           */
;* 	  ($make-gst-message msg))))                                   */

;*---------------------------------------------------------------------*/
;*    gst-bus-poll ...                                                 */
;*---------------------------------------------------------------------*/
(define (gst-bus-poll o::gst-bus
		      #!key
		      (types $gst-message-any)
		      (timeout #l-1))
   (let ((tmt (cond
		 ((llong? timeout)
		  timeout)
		 ((fixnum? timeout)
		  (fixnum->llong timeout))
		 (else
		  (bigloo-type-error 'gst-bus-poll "llong" timeout)))))
      (with-access::gst-bus o ((obuiltin $builtin))
	 (let ((msg::$gst-message
		  ($gst-bus-poll ($gst-bus obuiltin) types tmt)))
	    (unless ($gst-message-null? msg)
	       ($make-gst-message msg #t))))))

;*---------------------------------------------------------------------*/
;*    gst-bus-sync-handler-set! ...                                    */
;*---------------------------------------------------------------------*/
(define (gst-bus-sync-handler-set! o handler)
   (with-access::gst-bus o ((obuiltin $builtin))
      ($gst-bus-set-sync-handler! ($gst-bus obuiltin) handler))
   handler)

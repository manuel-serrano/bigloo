;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstpad.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 14:45:56 2008                          */
;*    Last change :  Tue Nov 15 17:32:23 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstPad                                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstpad

   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure
	    __gstreamer_gstelement
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpluginfeature)

   (export  (class gst-pad::gst-object
	       (name::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     ($gst-pad-get-name
				($gst-pad $builtin))))))
	       (direction::symbol
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     ($gst-pad-direction->obj
				($gst-pad-get-direction
				   ($gst-pad $builtin)))))))
	       (parent
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     (let ((p ($gst-pad-get-parent-element
					 ($gst-pad $builtin))))
				(if ($gst-element-null? p)
				    #f
				    ($make-gst-element p %gst-object-finalize!)))))))
	       (caps::gst-caps
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     ($make-gst-caps
				($gst-pad-get-caps ($gst-pad $builtin))
				#t))))
		  (set (lambda (o v)
			  (with-access::gst-pad o ($builtin)
			     (with-access::gst-caps v ((caps-builtin $builtin))
			     ($gst-pad-set-caps! ($gst-pad $builtin)
				caps-builtin))))))
	       (allowed-caps
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     (let ((c ($gst-pad-get-allowed-caps
					 ($gst-pad $builtin))))
				(unless ($gst-caps-null? c)
				   ($make-gst-caps c #t)))))))
	       (negotiated-caps
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     (let ((c ($gst-pad-get-negotiated-caps
					 ($gst-pad $builtin))))
				(unless ($gst-caps-null? c)
				   ($make-gst-caps c #t)))))))
	       (pad-template-caps
		  read-only
		  (get (lambda (o)
			  (with-access::gst-pad o ($builtin)
			     (let ((c ($gst-pad-get-pad-template-caps
					 ($gst-pad $builtin))))
				(unless ($gst-caps-null? c)
				   ($gst-caps-ref! c)
				   ($make-gst-caps c #t))))))))

	    ($make-gst-pad::obj ::$gst-pad ::obj)
	    
	    (gst-pad-link! ::gst-pad ::gst-pad)
	    (gst-pad-can-link?::bool ::gst-pad ::gst-pad)
	    (gst-pad-is-linked?::bool ::gst-pad)
	    (gst-pad-unlink! ::gst-pad ::gst-pad)
	    (gst-pad-add-buffer-probe!::int ::gst-pad ::procedure)
;* 	    (gst-pad-add-data-probe!::int ::gst-pad ::procedure)       */
;* 	    (gst-pad-add-event-probe!::int ::gst-pad ::procedure)      */
	    (gst-pad-remove-buffer-probe! ::gst-pad ::int)
;* 	    (gst-pad-remove-data-probe! ::gst-pad ::int)               */
;* 	    (gst-pad-remove-event-probe! ::gst-pad ::int)              */

	    ($gst-pad-direction->obj::obj link::$gst-pad-direction)
	    ($gst-pad-presence->obj::obj link::$gst-pad-presence))

   (extern  (export $make-gst-pad "bgl_gst_pad_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-pad ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-gst-pad pad::$gst-pad finalizer)
   (instantiate::gst-pad
      ($builtin ($gst-pad->object pad))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    object-display ::gst-pad ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display o::gst-pad . port)
   (with-access::gst-pad o (name direction $builtin)
      (let ((p (if (pair? port) (car port) (current-output-port))))
	 (display "<" p)
	 (display (find-runtime-type o) p)
	 (display " refcount=" p)
	 (display ($gst-object-refcount $builtin) p)
	 (display " name=" p)
	 (display name p)
	 (display " direction=" p)
	 (display direction p)
	 (display ">" p))))

;*---------------------------------------------------------------------*/
;*    $gst-pad-link-return->obj ...                                    */
;*---------------------------------------------------------------------*/
(define ($gst-pad-link-return->obj::obj link::$gst-pad-link-return)
   (cond
      ((eq? link $gst-pad-link-ok) 'ok)
      ((eq? link $gst-pad-link-wrong-hierarchy) 'wrong-hierachy)
      ((eq? link $gst-pad-link-was-linked) 'was-linked)
      ((eq? link $gst-pad-link-wrong-direction) 'wrong-direction)
      ((eq? link $gst-pad-link-noformat) 'noformat)
      ((eq? link $gst-pad-link-nosched) 'nosched)
      ((eq? link $gst-pad-link-refused) 'refused)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    $gst-pad-direction->obj ...                                      */
;*---------------------------------------------------------------------*/
(define ($gst-pad-direction->obj::obj link::$gst-pad-direction)
   (cond
      ((eq? link $gst-pad-unknown) 'unknown)
      ((eq? link $gst-pad-src) 'src)
      ((eq? link $gst-pad-sink) 'sink)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    $gst-pad-presence->obj ...                                       */
;*---------------------------------------------------------------------*/
(define ($gst-pad-presence->obj::obj link::$gst-pad-presence)
   (cond
      ((eq? link $gst-pad-always) 'always)
      ((eq? link $gst-pad-sometimes) 'sometimes)
      ((eq? link $gst-pad-request) 'request)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    gst-pad-link! ...                                                */
;*---------------------------------------------------------------------*/
(define (gst-pad-link! src sink)
   (with-access::gst-pad src ((src-builtin $builtin))
      (with-access::gst-pad sink ((sink-builtin $builtin))
	 (let ((retcode ($gst-pad-link! ($gst-pad src-builtin)
			   ($gst-pad sink-builtin))))
	    (if (eq? retcode $gst-pad-link-ok)
		#t
		(raise (instantiate::&gst-error
			  (proc 'gst-pad-link!)
			  (msg (format "Cannot link pads: ~a"
				  ($gst-pad-link-return->obj retcode)))
			  (obj (cons src sink)))))))))

;*---------------------------------------------------------------------*/
;*    gst-pad-can-link? ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-pad-can-link? src sink)
   (with-access::gst-pad src ((src-builtin $builtin))
      (with-access::gst-pad sink ((sink-builtin $builtin))
	 ($gst-pad-can-link? ($gst-pad src-builtin) ($gst-pad sink-builtin)))))

;*---------------------------------------------------------------------*/
;*    gst-pad-is-linked? ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-pad-is-linked? pad)
   (with-access::gst-pad pad ($builtin)
      ($gst-pad-is-linked? ($gst-pad $builtin))))

;*---------------------------------------------------------------------*/
;*    gst-pad-unlink! ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-pad-unlink! src sink)
   (with-access::gst-pad src ((src-builtin $builtin))
      (with-access::gst-pad sink ((sink-builtin $builtin))
	 ($gst-pad-unlink! ($gst-pad src-builtin) ($gst-pad sink-builtin)))))

;*---------------------------------------------------------------------*/
;*    gst-pad-add-buffer-probe! ...                                    */
;*---------------------------------------------------------------------*/
(define (gst-pad-add-buffer-probe! pad proc)
   (with-access::gst-pad pad ($builtin)
      (if (correct-arity? proc 0)
	  ($gst-pad-add-buffer-probe! ($gst-pad $builtin) proc)
	  (error 'gst-pad-add-buffer-probe! "Arity 0 procedure expected" proc))))

;* {*---------------------------------------------------------------------*} */
;* {*    gst-pad-add-data-probe! ...                                      *} */
;* {*---------------------------------------------------------------------*} */
;* (define (gst-pad-add-data-probe! pad proc)                          */
;*    (if (not (correct-arity? proc) 0)                                */
;*        (error 'gst-pad-add-data-probe! "Arity 0 procedure expected" proc) */
;*        ($gst-pad-add-data-probe! ($gst-pad (gst-pad-$builtin src)) proc))) */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    gst-pad-add-event-probe! ...                                     *} */
;* {*---------------------------------------------------------------------*} */
;* (define (gst-pad-add-event-probe! pad proc)                         */
;*    (if (not (correct-arity? proc) 0)                                */
;*        (error 'gst-pad-add-event-probe! "Arity 0 procedure expected" proc) */
;*        ($gst-pad-add-event-probe! ($gst-pad (gst-pad-$builtin src)) proc))) */

;*---------------------------------------------------------------------*/
;*    gst-pad-remove-buffer-probe! ...                                 */
;*---------------------------------------------------------------------*/
(define (gst-pad-remove-buffer-probe! pad index)
   (with-access::gst-pad pad ($builtin)
      ($gst-pad-remove-buffer-probe! ($gst-pad $builtin) index))
   index)

;* {*---------------------------------------------------------------------*} */
;* {*    gst-pad-remove-data-probe! ...                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define (gst-pad-remove-data-probe! pad index)                      */
;*    ($gst-pad-remove-data-probe! ($gst-pad (gst-pad-$builtin src)) index))) */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    gst-pad-remove-event-probe! ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (define (gst-pad-remove-event-probe! pad index)                     */
;*    ($gst-pad-remove-event-probe! ($gst-pad (gst-pad-$builtin src)) index))) */


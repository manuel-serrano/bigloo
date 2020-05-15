;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstbuffer.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  4 06:19:50 2008                          */
;*    Last change :  Tue Nov 15 11:26:00 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstBuffer                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstbuffer
   
   (include "gst.sch")
   
   (use	    __gstreamer_gsterror)

   (import  __gstreamer_gststructure)

   (extern  (macro $string-to-bstring-len::bstring
	       (::string ::long) "string_to_bstring_len")
	    (export $make-gst-buffer "bgl_gst_buffer_new"))
   
   (export  (class gst-buffer
	       (%gst-buffer-init)
	       ($builtin::$gst-buffer (default ($gst-buffer-nil)))
	       ($finalizer::obj read-only (default #f))
	       (size::long
		read-only
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-get-size $builtin)))))
	       (data::string
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-get-string $builtin))))
		(set (lambda (o v)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-set-string! $builtin v))
			o)))
	       (dts::llong
		read-only
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-dts $builtin)))))
	       (pts::llong
		read-only
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-pts $builtin)))))
	       (timestamp::llong
		read-only
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-dts-or-pts $builtin)))))
	       (duration::llong
		read-only
		(get (lambda (o)
			(with-access::gst-buffer o ($builtin)
			   ($gst-buffer-duration $builtin))))))

	    (%gst-buffer-init ::gst-buffer)
	    ($make-gst-buffer::obj ::$gst-buffer ::obj)))

;*---------------------------------------------------------------------*/
;*    $make-gst-buffer ...                                             */
;*---------------------------------------------------------------------*/
(define ($make-gst-buffer buffer::$gst-buffer finalizer)
   (instantiate::gst-buffer
      ($builtin buffer)
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    %gst-buffer-init ::gst-buffer ...                                */
;*---------------------------------------------------------------------*/
(define (%gst-buffer-init o::gst-buffer)
   (with-access::gst-buffer o ($builtin $finalizer)
      (when ($gst-buffer-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-buffer-init)
		   (msg "Illegal gst-buffer")
		   (obj o))))
      (cond
	 ((procedure? $finalizer)
	  ;; user finalizer
	  ($gst-add-finalizer! o $finalizer))
	 ($finalizer
	  ;;; regular unref finalizer
	  ($gst-add-finalizer! o (lambda (o)
				    (with-access::gst-buffer o ($builtin)
				       ($gst-buffer-unref! $builtin))))))
      o))

;*---------------------------------------------------------------------*/
;*    gst-buffer->string ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-buffer->string o::gst-buffer)
   (with-access::gst-buffer o (data size)
      ($string-to-bstring-len data size)))
   

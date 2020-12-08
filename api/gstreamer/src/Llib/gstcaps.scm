;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstcaps.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  4 06:24:04 2008                          */
;*    Last change :  Tue Nov 15 11:51:40 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstCaps                                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstcaps
   
   (include "gst.sch")
   
   (use	    __gstreamer_gsterror)

   (import  __gstreamer_gststructure)
   
   (export  (class gst-caps
	       (%gst-caps-init)
	       ($builtin::$gst-caps (default ($gst-caps-nil)))
	       ($finalizer::obj read-only
		  (default (lambda (o)
			      (with-access::gst-caps o ($builtin)
				 ($gst-caps-unref! $builtin)))))
	       (size::long
		read-only
		(get (lambda (o)
			(with-access::gst-caps o ($builtin)
			   ($gst-caps-get-size $builtin))))))

	    (%gst-caps-init ::gst-caps)
	    ($make-gst-caps ::$gst-caps ::obj)
	    (gst-caps-new-simple::gst-caps ::bstring . args)
	    
	    (gst-caps-structure::gst-structure ::gst-caps ::long)
	    (gst-caps-always-compatible?::bool ::gst-caps ::gst-caps)
	    (gst-caps-append!::gst-caps ::gst-caps ::gst-caps)
	    (gst-caps-merge!::gst-caps ::gst-caps ::gst-caps)
	    (gst-caps-append-structure!::gst-caps ::gst-caps ::gst-structure)
	    (gst-caps-merge-structure!::gst-caps ::gst-caps ::gst-structure)
	    (gst-caps-remove-structure!::gst-caps ::gst-caps ::int)

	    (gst-caps-to-string::bstring ::gst-caps)
	    (gst-caps-from-string::gst-caps ::bstring))
   
   (extern  (export $make-gst-caps "bgl_gst_caps_new")))

;*---------------------------------------------------------------------*/
;*    %gst-caps-init ::gst-caps ...                                    */
;*---------------------------------------------------------------------*/
(define (%gst-caps-init o::gst-caps)
   (with-access::gst-caps o ($builtin $finalizer)
      (when ($gst-caps-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-caps-init)
		   (msg "Illegal gst-caps")
		   (obj o))))
      (cond
	 ((procedure? $finalizer)
	  ($gst-add-finalizer! o $finalizer))
	 ($finalizer
	   ;;;
	  ($gst-add-finalizer! o (lambda (o)
				    (with-access::gst-caps o ($builtin)
				       ($gst-caps-unref! $builtin))))))
      o))

;*---------------------------------------------------------------------*/
;*    $make-gst-caps ...                                               */
;*---------------------------------------------------------------------*/
(define ($make-gst-caps caps::$gst-caps finalizer)
   (instantiate::gst-caps
      ($builtin caps)
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::gst-caps . port)
   (let ((p (if (pair? port) (car port) (current-output-port))))
      (display "<" p)
      (display (find-runtime-type o) p)
      (display " refcount=" p)
      (with-access::gst-caps o ($builtin size)
	 (display ($gst-object-refcount ($gst-caps->object $builtin)) p)
	 (display " size=" p)
	 (display size p))
      (display ">" p)))

;*---------------------------------------------------------------------*/
;*    gst-caps-new-simple ...                                          */
;*---------------------------------------------------------------------*/
(define (gst-caps-new-simple media-type . args)
   ;; check the arguments validiy
   (let loop ((a args))
      (cond
	 ((null? a)
	  (let ((finalizer (lambda (o)
			      (with-access::gst-caps o ($builtin)
				 ($gst-caps-unref! $builtin)))))
	     ($gst-caps-new-simple media-type args finalizer)))
	 ((not (keyword? (car a)))
	  (bigloo-type-error 'gst-caps-new-simple 'keyword (car a)))
	 ((null? (cdr a))
	  (error 'gst-caps-new-simple "Missing value for attribute" (car a)))
	 (else
	  (loop (cddr a))))))
;
;*---------------------------------------------------------------------*/
;*    gst-caps-structure ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-caps-structure caps index)
   (with-access::gst-caps caps ($builtin)
      (instantiate::gst-structure
	 ($builtin ($gst-caps-get-structure $builtin index)))))

;*---------------------------------------------------------------------*/
;*    gst-caps-always-compatible? ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-caps-always-compatible? caps1 caps2)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      (with-access::gst-caps caps2 ((builtin2 $builtin))
	 ($gst-caps-is-always-compatible? builtin1 builtin2))))
	       
;*---------------------------------------------------------------------*/
;*    gst-caps-append! ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-caps-append! caps1 caps2)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      (with-access::gst-caps caps2 ((builtin2 $builtin))
	 ($gst-caps-append builtin1 builtin2)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-merge! ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-caps-merge! caps1 caps2)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      (with-access::gst-caps caps2 ((builtin2 $builtin))
	 ($make-gst-caps ($gst-caps-merge builtin1 builtin2) #t))))

;*---------------------------------------------------------------------*/
;*    gst-caps-append-structure! ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-caps-append-structure! caps1 s)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      (with-access::gst-structure s ((builtin2 $builtin))
	 ($gst-caps-append-structure builtin1 builtin2)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-merge-structure! ...                                    */
;*---------------------------------------------------------------------*/
(define (gst-caps-merge-structure! caps1 s)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      (with-access::gst-structure s ((builtin2 $builtin))
	 ($make-gst-caps ($gst-caps-merge-structure builtin1 builtin2) #t))))

;*---------------------------------------------------------------------*/
;*    gst-caps-remove-structure! ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-caps-remove-structure! caps1 i)
   (with-access::gst-caps caps1 ((builtin1 $builtin))
      ($gst-caps-remove-structure builtin1 i))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-to-string ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-caps-to-string caps)
   (with-access::gst-caps caps ($builtin)
      ($gst-caps-to-string $builtin)))

;*---------------------------------------------------------------------*/
;*    gst-caps-from-string ...                                         */
;*---------------------------------------------------------------------*/
(define (gst-caps-from-string string)
   ($make-gst-caps ($gst-caps-from-string string) #f))
   
   

;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstcaps.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  4 06:24:04 2008                          */
;*    Last change :  Mon Feb 11 15:11:29 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstCaps                                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstcaps
   
   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gststructure)
   
   (export  (class gst-caps
	       (%gst-caps-init)
	       ($builtin::$gst-caps (default ($gst-caps-nil)))
	       ($finalizer::obj read-only (default (lambda (o)
						      ($gst-caps-unref!
						       (gst-caps-$builtin o)))))
	       (size::long
		read-only
		(get (lambda (o)
			($gst-caps-get-size
			 ($gst-caps
			  (gst-caps-$builtin o)))))))

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
	  ($gst-add-finalizer! o (lambda (o)
				    ($gst-caps-unref!
				     (gst-caps-$builtin o))))))
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
      (display ($gst-object-refcount (gst-caps-$builtin o)) p)
      (display " size=" p)
      (display (gst-caps-size o) p)
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
			      ($gst-caps-unref! (gst-caps-$builtin o)))))
	     ($gst-caps-new-simple media-type args finalizer)))
	 ((not (keyword? (car a)))
	  (bigloo-type-error 'gst-caps-new-simple 'keyword (car a)))
	 ((null? (cdr a))
	  (error 'gst-caps-new-simple "Miising value for attribute" (car a)))
	 (else
	  (loop (cddr a))))))
;
;*---------------------------------------------------------------------*/
;*    gst-caps-structure ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-caps-structure caps index)
   (instantiate::gst-structure
      ($builtin ($gst-caps-get-structure
		 ($gst-caps (gst-caps-$builtin caps)) index))))

;*---------------------------------------------------------------------*/
;*    gst-caps-always-compatible? ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-caps-always-compatible? caps1 caps2)
   ($gst-caps-is-always-compatible?
    ($gst-caps (gst-caps-$builtin caps1))
    ($gst-caps (gst-caps-$builtin caps2))))
	       
;*---------------------------------------------------------------------*/
;*    gst-caps-append! ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-caps-append! caps1 caps2)
   ($gst-caps-append
    ($gst-caps (gst-caps-$builtin caps1))
    ($gst-caps (gst-caps-$builtin caps2)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-merge! ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-caps-merge! caps1 caps2)
   ($gst-caps-merge
    ($gst-caps (gst-caps-$builtin caps1))
    ($gst-caps (gst-caps-$builtin caps2)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-append-structure! ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-caps-append-structure! caps1 s)
   ($gst-caps-append-structure
    ($gst-caps (gst-caps-$builtin caps1))
    ($gst-structure (gst-structure-$builtin s)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-merge-structure! ...                                    */
;*---------------------------------------------------------------------*/
(define (gst-caps-merge-structure! caps1 s)
   ($gst-caps-merge-structure
    ($gst-caps (gst-caps-$builtin caps1))
    ($gst-structure (gst-structure-$builtin s)))
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-remove-structure! ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-caps-remove-structure! caps1 i)
   ($gst-caps-remove-structure ($gst-caps (gst-caps-$builtin caps1)) i)
   caps1)

;*---------------------------------------------------------------------*/
;*    gst-caps-to-string ...                                           */
;*---------------------------------------------------------------------*/
(define (gst-caps-to-string caps)
   ($gst-caps-to-string ($gst-caps (gst-caps-$builtin caps))))

;*---------------------------------------------------------------------*/
;*    gst-caps-from-string ...                                         */
;*---------------------------------------------------------------------*/
(define (gst-caps-from-string string)
   ($make-gst-caps ($gst-caps-from-string string) #f))
   
   

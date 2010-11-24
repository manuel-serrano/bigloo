;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/src/Llib/gstobject.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 16:06:35 2007                          */
;*    Last change :  Fri Nov 19 10:51:36 2010 (serrano)                */
;*    Copyright   :  2007-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstObject wrapper                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstobject

   (library pthread)
   
   (include "gst.sch")
   
   (import  __gstreamer_gsterror)
    
   (export  (abstract-class gst-object
	       (%gst-object-init)
	       ($builtin::$gst-object (default (%$gst-object-nil)))
	       ($finalizer::obj read-only (default #f))
	       (%closures::pair-nil (default '())))

	    (%$gst-object-nil::$gst-object)

	    (generic %gst-object-init ::gst-object)

	    (%gst-object->gobject::$gst-object ::gst-object)

	    (%gst-object-ref! ::gst-object)
	    (%gst-object-unref! ::gst-object)

	    (%gst-object-init-debug ::obj)
	    (%gst-object-finalize-debug ::obj)
	    
	    (%gst-object-finalize-closures! ::gst-object)
	    (%gst-object-finalize! ::gst-object)

	    (gst-object-property-list::pair-nil ::gst-object)
	    (gst-object-property ::gst-object ::keyword)
	    (gst-object-property-set! ::gst-object ::keyword ::obj)

	    (gst-object-connect! ::gst-object ::bstring ::procedure)
	    (closure-gcmark! ::procedure)
	    (closure-gcunmark! ::procedure))

   (extern  (export gst-object? "bgl_gst_objectp")
	    (export %gst-object->gobject "bgl_gst_object_to_gstobject")
	    (export closure-gcmark! "bgl_closure_gcmark")
	    (export closure-gcunmark! "bgl_closure_gcunmark")))

;*---------------------------------------------------------------------*/
;*    %$gst-object-nil ...                                             */
;*---------------------------------------------------------------------*/
(define (%$gst-object-nil::$gst-object)
   ($gst-object-nil))

;*---------------------------------------------------------------------*/
;*    %gst-object->gobject ...                                         */
;*---------------------------------------------------------------------*/
(define (%gst-object->gobject o::gst-object)
   (gst-object-$builtin o))

;*---------------------------------------------------------------------*/
;*    *gst-object-debug-mutex* ...                                     */
;*---------------------------------------------------------------------*/
(define *gst-object-debug-mutex* (make-mutex))
(define *gst-object-debug-count* 0)

;*---------------------------------------------------------------------*/
;*    %gst-object-init-debug ...                                       */
;*---------------------------------------------------------------------*/
(define (%gst-object-init-debug o)
   (with-lock *gst-object-debug-mutex*
      (lambda ()
	 (set! *gst-object-debug-count* (+fx 1 *gst-object-debug-count*))
	 (display "gst-object-init (" (current-error-port))
	 (display *gst-object-debug-count* (current-error-port))
	 (display "): " (current-error-port))))
   (display (find-runtime-type o) (current-error-port))
   (pragma "fprintf( stderr, \" o=%p builtin=%p refcount=%d\", $1, ((BgL_gstzd2objectzd2_bglt)$1)->BgL_z42builtinz42, ((GObject *)(((BgL_gstzd2objectzd2_bglt)$1)->BgL_z42builtinz42))->ref_count )" o)
   (newline (current-error-port)))

;*---------------------------------------------------------------------*/
;*    %gst-object-finalize-debug ...                                   */
;*---------------------------------------------------------------------*/
(define (%gst-object-finalize-debug o)
   (when (> (bigloo-debug) (gst-debug-level))
      (with-lock *gst-object-debug-mutex*
	 (lambda ()
	    (set! *gst-object-debug-count* (+fx -1 *gst-object-debug-count*))
	    (display "gst-object-unref! (" (current-error-port))
	    (display *gst-object-debug-count* (current-error-port))
	    (display "): " (current-error-port))))
      (display (find-runtime-type o) (current-error-port))
      (pragma "fprintf( stderr, \" o=%p builtin=%p refcount=%d -> %d\",
 $1, ((BgL_gstzd2objectzd2_bglt)$1)->BgL_z42builtinz42, ((GObject *)((BgL_gstzd2objectzd2_bglt)$1)->BgL_z42builtinz42)->ref_count, ((GObject *)((BgL_gstzd2objectzd2_bglt)$1)->BgL_z42builtinz42)->ref_count  -1 ), puts( \"\" )" o)
      #unspecified))

;*---------------------------------------------------------------------*/
;*    %gst-object-init ::gst-object ...                                */
;*---------------------------------------------------------------------*/
(define-generic (%gst-object-init o::gst-object)
   (with-access::gst-object o ($builtin $finalizer)
      (when ($gst-element-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-object-init)
		   (msg "Illegal gst-object")
		   (obj o))))
      (when (> (bigloo-debug) (gst-debug-level)) (%gst-object-init-debug o))
      (cond
	 ((procedure? $finalizer)
	  ;; user finalizer
	  ($gst-add-finalizer! o $finalizer))
	 ($finalizer
	  ;; regular unref finalizer
	  ($gst-add-finalizer! o %gst-object-finalize!)))
      o))

;*---------------------------------------------------------------------*/
;*    object-display ...                                               */
;*---------------------------------------------------------------------*/
(define-method (object-display o::gst-object . port)
   (let ((p (if (pair? port) (car port) (current-output-port))))
      (display "<" p)
      (display (find-runtime-type o) p)
      (display " refcount=" p)
      (display ($gst-object-refcount (gst-object-$builtin o)) p)
      (display ">" p)))

;*---------------------------------------------------------------------*/
;*    %gst-object-unref! ...                                           */
;*---------------------------------------------------------------------*/
(define (%gst-object-unref! o)
   ($gst-object-unref! (gst-object-$builtin o)))

;*---------------------------------------------------------------------*/
;*    %gst-object-ref! ...                                             */
;*---------------------------------------------------------------------*/
(define (%gst-object-ref! o)
   ($gst-object-ref! (gst-object-$builtin o)))

;*---------------------------------------------------------------------*/
;*    %gst-object-finalize-closures! ...                               */
;*---------------------------------------------------------------------*/
(define (%gst-object-finalize-closures! o)
   (with-access::gst-object o (%closures)
      (for-each closure-gcunmark! %closures)))

;*---------------------------------------------------------------------*/
;*    %gst-object-finalize! ...                                        */
;*---------------------------------------------------------------------*/
(define (%gst-object-finalize! o)
   (when (> (bigloo-debug) (gst-debug-level)) (%gst-object-finalize-debug o))
   (%gst-object-finalize-closures! o)
   (%gst-object-unref! o))

;*---------------------------------------------------------------------*/
;*    gst-object-property-list ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-object-property-list o::gst-object)
   ($gst-object-property-list (gst-object-$builtin o)))

;*---------------------------------------------------------------------*/
;*    gst-object-property ...                                          */
;*---------------------------------------------------------------------*/
(define (gst-object-property o::gst-object prop)
   ($gst-object-get-property (gst-object-$builtin o) (keyword->string! prop)))

;*---------------------------------------------------------------------*/
;*    gst-object-property-set! ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-object-property-set! o::gst-object prop val)
   ($gst-object-set-property!
    (gst-object-$builtin o) (keyword->string! prop) val))

;*---------------------------------------------------------------------*/
;*    gst-object-connect! ...                                          */
;*---------------------------------------------------------------------*/
(define (gst-object-connect! el name proc)
   (with-access::gst-object el (%closures)
      (set! %closures (cons proc %closures)))
   (closure-gcmark! proc)
   ($gst-object-connect! (gst-object-$builtin el) name proc))

;*---------------------------------------------------------------------*/
;*    *gcmark-mutex* ...                                               */
;*---------------------------------------------------------------------*/
(define *gcmark-mutex* (make-mutex))

;*---------------------------------------------------------------------*/
;*    *gcmark-closure* ...                                             */
;*---------------------------------------------------------------------*/
(define *gcmark-closure* '())

;*---------------------------------------------------------------------*/
;*    closure-gcmark! ...                                              */
;*---------------------------------------------------------------------*/
(define (closure-gcmark! proc)
   (mutex-lock! *gcmark-mutex*)
   (set! *gcmark-closure* (cons proc *gcmark-closure*))
   (when (> (bigloo-debug) (gst-debug-level))
      (with-lock *gst-object-debug-mutex*
	 (lambda ()
	    (tprint "closure-gcmark: " (length *gcmark-closure*)))))
   (mutex-unlock! *gcmark-mutex*))

;*---------------------------------------------------------------------*/
;*    closure-gcunmark! ...                                            */
;*---------------------------------------------------------------------*/
(define (closure-gcunmark! proc)
   (mutex-lock! *gcmark-mutex*)
   (set! *gcmark-closure* (remq! proc *gcmark-closure*))
   (when (> (bigloo-debug) (gst-debug-level))
      (with-lock *gst-object-debug-mutex*
	 (lambda ()
	    (tprint "closure-gcunmark: " (length *gcmark-closure*)))))
   (mutex-unlock! *gcmark-mutex*))

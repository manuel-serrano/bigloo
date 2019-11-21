;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gstelement.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 30 15:46:10 2007                          */
;*    Last change :  Tue Nov 15 16:56:12 2011 (serrano)                */
;*    Copyright   :  2007-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstElement wrapper                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstelement

   (include "gst.sch")

   (use	    __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gststructure
	    __gstreamer_gstcaps)

   (import  __gstreamer_gstpluginfeature
	    __gstreamer_gstelementfactory
	    __gstreamer_gstpad
	    __gstreamer_gstreamer)

   (extern  (macro %gst-lock!::obj () "bgl_gst_lock")
	    (macro %gst-unlock!::obj () "bgl_gst_unlock")
	    (export $gst-state->obj "bgl_gst_state_to_obj")
	    (export $make-gst-element "bgl_gst_element_new"))

   (export  (class gst-element::gst-object
	       (element-factory::gst-element-factory
		  read-only
		  (get
		     (lambda (o)
			(with-access::gst-element o ($builtin)
			   ($make-gst-element-factory
			      ($gst-element-get-factory
				 ($gst-element $builtin))
			      #f)))))
	       (interface-list::pair-nil
		  read-only
		  (get
		     (lambda (o)
			(with-access::gst-element o ($builtin)
			   ($gst-element-interface-list
			      ($gst-element $builtin))))))
	       (name::string
		  (get
		     (lambda (o)
			(with-access::gst-element o ($builtin)
			   ($gst-element-get-name
			      ($gst-element $builtin)))))
		  (set
		     (lambda (o v)
			(with-access::gst-element o ($builtin)
			   ($gst-element-set-name!
			      ($gst-element $builtin) v))))))

	    ($make-gst-element ::$gst-element ::obj)

	    (gst-element-state::symbol ::gst-element #!optional (timeout #l0))
	    (gst-element-state-set!::symbol ::gst-element ::symbol)    
	    (gst-element-pad::obj ::gst-element ::bstring)
	    (gst-element-add-pad! ::gst-element ::gst-pad)
	    (gst-element-compatible-pad::obj ::gst-element ::gst-pad ::gst-caps)

	    (gst-element-query-position::llong ::gst-element)
	    (gst-element-query-duration::llong ::gst-element)
	    (gst-element-seek::bool ::gst-element ::llong)
	    
	    (gst-element-link! ::gst-element ::gst-element . els)
	    (gst-element-link-filtered! ::gst-element ::gst-element ::gst-caps)
	    (gst-element-link-mime! ::gst-element ::gst-element ::bstring . ::obj)
	    (gst-element-unlink! ::gst-element ::gst-element . els)

	    ($gst-state->obj::symbol ::$gst-state)))
   
;*---------------------------------------------------------------------*/
;*    $make-gst-element ...                                            */
;*---------------------------------------------------------------------*/
(define ($make-gst-element element::$gst-element finalizer::obj)
   (instantiate::gst-element
      ($builtin ($gst-element->object element))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    object-display ::gst-element ...                                 */
;*---------------------------------------------------------------------*/
(define-method (object-display o::gst-element . port)
   (with-access::gst-element o (name)
      (let ((p (if (pair? port) (car port) (current-output-port))))
	 (display "<" p)
	 (display (find-runtime-type o) p)
	 (display " refcount=" p)
	 (with-access::gst-object o ($builtin)
	    (display ($gst-object-refcount $builtin) p))
	 (display " name=" p)
	 (display name p)
	 (display ">" p))))

;*---------------------------------------------------------------------*/
;*    gst-element-query-position ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-element-query-position el::gst-element)
   (with-access::gst-element el ($builtin)
      ($gst-element-query-position ($gst-element $builtin))))

;*---------------------------------------------------------------------*/
;*    gst-element-query-duration ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-element-query-duration el::gst-element)
   (with-access::gst-element el ($builtin)
      ($gst-element-query-duration ($gst-element $builtin))))

;*---------------------------------------------------------------------*/
;*    gst-element-seek ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-element-seek el::gst-element v)
   (with-access::gst-element el ($builtin)
      ($gst-element-seek-simple
	 ($gst-element $builtin)
	 $gst-format-time (bit-or $gst-seek-flag-flush $gst-seek-flag-key-unit) v)))

;*---------------------------------------------------------------------*/
;*    gst-element-link! ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-element-link! el0::gst-element el1::gst-element . els)
   
   (define (link! src dst)
      (if (isa? dst gst-element)
	  (with-access::gst-element src ((src-builtin $builtin))
	     (with-access::gst-element dst ((dst-builtin $builtin))
		(unless ($gst-element-link! ($gst-element src-builtin)
			   ($gst-element dst-builtin))
		   (raise (instantiate::&gst-error
			     (proc 'gst-element-link!)
			     (msg "Element cannot be linked")
			     (obj (list src dst)))))))
	  (raise (instantiate::&gst-error
		    (proc 'gst-element-link!)
		    (msg "Illegal element ")
		    (obj dst)))))
      
   (link! el0 el1)
   (let loop ((src el1)
	      (els els))
      (when (pair? els)
	 (link! src (car els))
	 (loop (car els) (cdr els))))
   
   #unspecified)

;*---------------------------------------------------------------------*/
;*    gst-element-link-filtered! ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-element-link-filtered! e0 e1 caps)
   (with-access::gst-element e0 ((e0-builtin $builtin))
      (with-access::gst-element e1 ((e1-builtin $builtin))
	 (with-access::gst-caps caps ((caps-builtin $builtin))
	    (unless ($gst-element-link-filtered! ($gst-element e0-builtin)
		       ($gst-element e1-builtin)
		       caps-builtin)
	       (raise (instantiate::&gst-error
			 (proc 'gst-element-link-filtered!)
			 (msg "Element cannot be linked")
			 (obj (list e0 e1 caps)))))))))

;*---------------------------------------------------------------------*/
;*    gst-element-link-mime! ...                                       */
;*---------------------------------------------------------------------*/
(define (gst-element-link-mime! e0 e1 mime-type . props)
   (let ((caps (apply gst-caps-new-simple mime-type props)))
      (gst-element-link-filtered! e0 e1 caps)))

;*---------------------------------------------------------------------*/
;*    gst-element-link! ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-element-unlink! el0::gst-element el1::gst-element . els)
   
   (define (unlink! src dst)
      (with-access::gst-element src ((src-builtin $builtin))
	 (with-access::gst-element dst ((dst-builtin $builtin))
	    ($gst-element-unlink!
	       ($gst-element src-builtin) ($gst-element dst-builtin)))))
   
   (unlink! el0 el1)
   (let loop ((src el1)
	      (els els))
      (when (pair? els)
	 (unlink! src (car els))
	 (loop (car els) (cdr els))))
   
   #unspecified)

;*---------------------------------------------------------------------*/
;*    $gst-state ...                                                   */
;*---------------------------------------------------------------------*/
(define ($gst-state::$gst-state state::symbol)
   (case state
      ((void-pending) $gst-state-void-pending)
      ((null) $gst-state-null)
      ((ready) $gst-state-ready)
      ((paused) $gst-state-paused)
      ((playing) $gst-state-playing)
      (else (raise (instantiate::&gst-error
		      (proc '$gst-state)
		      (msg "Illegal state")
		      (obj state))))))

;*---------------------------------------------------------------------*/
;*    $gst-state->obj ...                                              */
;*---------------------------------------------------------------------*/
(define ($gst-state->obj::symbol state::$gst-state)
   (cond
      ((=fx state $gst-state-void-pending) 'void-pending)
      ((=fx state $gst-state-null) 'null)
      ((=fx state $gst-state-ready) 'ready)
      ((=fx state $gst-state-paused) 'paused)
      ((=fx state $gst-state-playing) 'playing)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    $gst-state-change-return->obj ...                                */
;*---------------------------------------------------------------------*/
(define ($gst-state-change-return->obj::obj state::$gst-state-change-return)
   (cond
      ((eq? state $gst-state-change-failure) 'failure)
      ((eq? state $gst-state-change-success) 'success)
      ((eq? state $gst-state-change-async) 'async)
      ((eq? state $gst-state-change-no-preroll) 'no-preroll)
      (else 'unknown)))

;*---------------------------------------------------------------------*/
;*    gst-element-state-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (gst-element-state-set! el state)
   (%gst-lock!)
   (%gst-thread-init!)
   ($gst-invoke-finalizers)
   (%gst-unlock!)
   (with-access::gst-element el ($builtin)
      ($gst-state-change-return->obj
	 ($gst-element-set-state! ($gst-element $builtin)
	    ($gst-state state)))))

;*---------------------------------------------------------------------*/
;*    gst-element-state ...                                            */
;*---------------------------------------------------------------------*/
(define (gst-element-state el #!optional (timeout #l0))
   (with-access::gst-element el ($builtin)
      ($gst-state-change-return->obj
	 ($gst-element-get-state ($gst-element $builtin)
	    0 0
	    (if (<=llong timeout #l0)
		$gst-clock-time-none 
		timeout)))))

;*---------------------------------------------------------------------*/
;*    gst-element-pad ...                                              */
;*---------------------------------------------------------------------*/
(define (gst-element-pad el name)
   (with-access::gst-element el ((el-builtin $builtin))
      (let* (($el::$gst-element ($gst-element el-builtin))
	     ($spad ($gst-element-get-static-pad $el name)))
	 (if ($gst-pad-null? $spad)
	     (let (($rpad ($gst-element-get-request-pad $el name)))
		(unless ($gst-pad-null? $rpad)
		   (instantiate::gst-pad
		      ($builtin ($gst-element->object $rpad))
		      ($finalizer (lambda (o)
				     (with-access::gst-element o ((o-builtin $builtin))
					(%gst-object-finalize-closures! o)
					($gst-element-release-request-pad!
					   ($gst-element el-builtin)
					   ($gst-pad o-builtin))))))))
	     (instantiate::gst-pad
		($builtin ($gst-element->object $spad))
		($finalizer %gst-object-finalize!))))))

;*---------------------------------------------------------------------*/
;*    gst-element-add-pad! ...                                         */
;*---------------------------------------------------------------------*/
(define (gst-element-add-pad! el pad)
   (with-access::gst-element el ((el-builtin $builtin))
      (with-access::gst-pad pad ((pad-builtin $builtin))
	 (unless ($gst-element-add-pad! ($gst-element el-builtin)
		    ($gst-pad pad-builtin))
	    (raise (instantiate::&gst-error
		      (proc 'gst-element-add-pad!)
		      (msg "Cannot add pad")
		      (obj (list el pad))))))))

;*---------------------------------------------------------------------*/
;*    gst-element-compatible-pad ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-element-compatible-pad el pad caps)
   (with-access::gst-element el ((el-builtin $builtin))
      (with-access::gst-pad pad ((pad-builtin $builtin))
	 (with-access::gst-caps caps ((caps-builtin $builtin))
	    (let ((pad::$gst-pad ($gst-element-get-compatible-pad
				    ($gst-element el-builtin)
				    ($gst-pad pad-builtin)
				    caps-builtin)))
	       (unless ($gst-pad-null? pad)
		  (instantiate::gst-pad
		     ($builtin ($gst-element->object pad))
		     ($finalizer %gst-object-finalize!))))))))


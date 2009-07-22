;*=====================================================================*/
;*    .../bigloo/api/fthread/examples/mtracker/mtracker.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Wed Jul 22 16:03:10 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple module used to test the fair thread implementation.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module mouse-tracker
   (library biglook fthread)
   (main    main)
   (static  (class atom
	       (atom-initialize!)
	       (%ci (default #unspecified))
	       (color (default *white*))
	       x y)))

;*---------------------------------------------------------------------*/
;*    make-thread ...                                                  */
;*---------------------------------------------------------------------*/
(define-expander make-thread
   (lambda (x e)
      (match-case x
	 ((make-thread ?body)
	  (e `(instantiate::fthread (body ,body)) e))
	 ((make-thread ?body ?name)
	  (e `(instantiate::fthread (body ,body) (name ,name)) e))
	 (else
	  (error 'make-thread "Illegal thread" x)))))

;*---------------------------------------------------------------------*/
;*    Global configuration ...                                         */
;*---------------------------------------------------------------------*/
(define *width* 300)
(define *height* 300)
(define *canvas* #unspecified)

(define *mouse-events* '())
(define *old-mouse* #f)
(define *speed* 2)
(define *thickness* 20)

(define *mode* 'flat)

(define *latency* 10)

(define *white* (instantiate::rgb-color
		   (red #xff)
		   (green #xff)
		   (blue #xff)))

(define *iteration* -1)

;*---------------------------------------------------------------------*/
;*    atom-initialize! ...                                             */
;*---------------------------------------------------------------------*/
(define (atom-initialize! a::atom)
   (with-access::atom a (color %ci x y)
      (set! %ci (instantiate::canvas-rectangle
		   (canvas *canvas*)
		   (x x)
		   (y y)
		   (width *thickness*)
		   (height *thickness*)
		   (color color)))))

;*---------------------------------------------------------------------*/
;*    make-mouse-tracker-env ...                                       */
;*---------------------------------------------------------------------*/
(define (make-mouse-tracker-env)
   (make-ftenv2d (+fx 1 (/fx *width* *thickness*))
		 (+fx 1 (/fx *height* *thickness*))))

;*---------------------------------------------------------------------*/
;*    motion-event ...                                                 */
;*---------------------------------------------------------------------*/
(define (motion-event e)
   (with-access::event e (widget x y)
      (let* ((ev-x (/fx x *thickness*))
	     (ev-y (/fx y *thickness*))
	     (sgn (cons ev-x ev-y)))
	 (if (equal? sgn *old-mouse*)
	     (set! *mouse-events* '())
	     (let ((lst (list sgn)))
		(if (eq? *mode* 'cross)
		    (begin
		       (if (< ev-y (/fx *height* *thickness*))
			   (set! lst (cons (cons ev-x (+fx ev-y 1)) lst)))
		       (if (> ev-y 0)
			   (set! lst (cons (cons ev-x (-fx ev-y 1)) lst)))
		       (if (< ev-x (/fx *width* *thickness*))
			   (set! lst (cons (cons (+fx ev-x 1) ev-y) lst)))
		       (if (> ev-x 0)
			   (set! lst (cons (cons (-fx ev-x 1) ev-y) lst)))))
		(set! *mouse-events* lst)
		(set! *old-mouse* sgn))))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; parse the command line
   (args-parse (cdr argv)
      (("--help" (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      ((("-w" "--width") ?w (help "Set canvas width"))
       (set! *width* (string->integer w)))
      ((("-h" "--height") ?h (help "Set canvas height"))
       (set! *height* (string->integer h)))
      ((("-t" "--thickness") ?t (help "Cell thickness"))
       (set! *thickness* (string->integer t)))
      ((("-l" "--latency") ?t (help "Latency"))
       (set! *latency* (string->integer t)))
      (("-s" ?speed (help "Set speed"))
       (set! *speed* (string->integer speed)))
      (("-c" (help "Set cross mode"))
       (set! *mode* 'cross))
      (else
       (set! *iteration* (string->number else))))
   ;; allocate the global canvas
   (let* ((win (instantiate::window
		  (padding 2)))
	  (canvas (instantiate::canvas
		     (parent `(,win :expand #t :fill both))
		     (width *width*)
		     (height *height*)
		     (event (instantiate::event-handler
			       (motion motion-event)))))
	  (col (instantiate::rgb-color
		  (red #xff))))
      ;; store the canvas
      (set! *canvas* canvas)
      ;; initialize the scheduler
      (let ((scdl (make-scheduler (make-mouse-tracker-env))))
	 (start-all-thread! scdl *width* *height*)
	 ;; start reacting
	 (let loop ()
	    (if (not (= *iteration* 0))
		(begin
		   (set! *iteration* (- *iteration* 1))
		   (for-each (lambda (pos)
				(scheduler-broadcast! scdl pos col))
			     *mouse-events*)
		   (scheduler-react! scdl)
		   (set! *mouse-events* '())
		   (after *speed* loop)))))))

;*---------------------------------------------------------------------*/
;*    start-all-thread! ...                                            */
;*---------------------------------------------------------------------*/
(define (start-all-thread! scdl width height)
   (define (colorize atom col)
      (with-access::atom atom (%ci)
	 (canvas-item-color-set! %ci col)))
   (define (atom-body atom x y)
      (let* ((ev-x (/fx x *thickness*))
	     (ev-y (/fx y *thickness*))
	     (evt (cons ev-x ev-y)))
	 (let loop ()
	    (colorize atom (thread-await! evt))
	    (thread-sleep! *latency*)
	    (colorize atom *white*)
	    (loop))))
   (let loop ((w 0))
      (if (<fx w width)
	  (begin
	     (let liip ((h 0))
		(if (>=fx h height)
		    (loop (+fx w *thickness*))
		    (let* ((atom (instantiate::atom
				    (x w)
				    (y h)))
			   (th (make-thread (lambda () (atom-body atom w h)))))
		       (thread-start! th scdl)
		       (liip (+fx h *thickness*)))))))))
		    
		 

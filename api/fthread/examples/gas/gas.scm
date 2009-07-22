;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/fthread/examples/gas/gas.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Mon Jun 29 09:50:36 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple module used to test the fair thread implementation.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gas
   (library biglook fthread)
   (main    main)
   (static  (class atom
	       (atom-initialize!)
	       (id (default (gensym)))
	       (%ci (default #unspecified))
	       (color (default *color*))
	       (oldx::long (default (random *width*)))
	       (oldy::long (default (random *height*)))
	       (dx::long (default 0))
	       (dy::long (default 0)))))

;*---------------------------------------------------------------------*/
;*    Global configuration ...                                         */
;*---------------------------------------------------------------------*/
(define *width* 300)
(define *height* 300)
(define *canvas* #unspecified)
(define *status-gauge* #unspecified)
(define *status-lbl* #unspecified)
(define *colors* (let ((l (list
			   (instantiate::rgb-color
			      (red #xff))
			   (instantiate::rgb-color
			      (green #xbb))
			   (instantiate::rgb-color
			      (blue #xff))
			   (instantiate::rgb-color
			      (red #xb3)
			      (green #x1f)
			      (blue #xff))
			   (instantiate::rgb-color
			      (red #xff)
			      (green #xdd)
			      (blue #x31)))))
		    (set-cdr! (last-pair l) l)
		    l))
(define *color* (car *colors*))

(define *atoms* '())
(define *atom-number* 0)
(define *ill-atom-number* 0)

(define *event-width* 10)

(define *speed* 10)

;*---------------------------------------------------------------------*/
;*    atom-initialize! ...                                             */
;*---------------------------------------------------------------------*/
(define (atom-initialize! a::atom)
   (set! *atom-number* (+fx 1 *atom-number*))
   (set! *ill-atom-number* (+fx 1 *ill-atom-number*))
   (set! *atoms* (cons a *atoms*))
   (refresh-status-gauge!)
   (with-access::atom a (color %ci oldx oldy dx dy)
      (set! %ci (instantiate::canvas-ellipse
		   (canvas *canvas*)
		   (x oldx)
		   (y oldy)
		   (width 5)
		   (height 5)
		   (color color)
		   (outline color)))
      ;; set the direction of the atom
      (let loop ()
	 (set! dx (- (random 10) 5))
	 (set! dy (- (random 10) 5))
	 (if (and (=fx dx 0) (=fx dy 0))
	     (loop)))))

;*---------------------------------------------------------------------*/
;*    *scheduler* ...                                                  */
;*---------------------------------------------------------------------*/
(define *scheduler* #unspecified)

;*---------------------------------------------------------------------*/
;*    make-gas-env ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-gas-env)
   (make-ftenv2d (+fx 1 (/fx *width* *event-width*))
		 (+fx 1 (/fx *height* *event-width*))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   ;; parse the command line
   (args-parse (cdr argv)
      ((("-h" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      (("--width" ?w (help "Set canvas width"))
       (set! *width* (string->integer w)))
      (("--height" ?h (help "Set canvas height"))
       (set! *height* (string->integer h)))
      (("-g" (help "Set debug mode"))
       (bigloo-debug-set! (+fx (bigloo-debug) 1)))
      (else
       (error "gas" "Unknown argument" else)))
   ;; allocate the global canvas
   (let* ((win (instantiate::window
		  (padding 2)))
	  (panel (make-icobj-panel win))
	  (canvas (instantiate::canvas
		     (parent `(,win :expand #t :fill both))
		     (width *width*)
		     (height *height*)))
	  (status (make-status-panel win)))
      ;; initialize the scheduler
      (set! *scheduler* (make-scheduler (make-gas-env)))
      ;; store the canvas
      (set! *canvas* canvas)
      ;; start reacting
      (let loop ()
	 (scheduler-react! *scheduler*)
	 (after *speed* loop))))

;*---------------------------------------------------------------------*/
;*    make-icobj-panel ...                                             */
;*---------------------------------------------------------------------*/
(define (make-icobj-panel parent)
   (let* ((box (instantiate::box
		  (parent parent)
		  (padding 2)
		  (orientation 'horizontal)))
	  (lbl (instantiate::label
		  (parent box)
		  (text "Add...")))
	  (add1 (instantiate::button
		   (parent box)
		   (text "1")
		   (command (lambda (b) (add-atoms 1)))))
	  (add10 (instantiate::button
		    (parent box)
		    (text "10")
		    (command (lambda (b) (add-atoms 10)))))
	  (add100 (instantiate::button
		     (parent box)
		     (text "100")
		     (command (lambda (b) (add-atoms 100)))))
	  (add1000 (instantiate::button
		      (parent box)
		      (text "1000")
		      (command (lambda (b) (add-atoms 1000)))))
	  (color (instantiate::button
		    (parent `(,box :expand #t :fill x))
		    (text "Change color...")
		    (command (lambda (b)
				(with-access::gauge *status-gauge* (value)
				   (set! value 0))
				(set! *colors* (cdr *colors*))
				(set! *color* (car *colors*))
				(set! *ill-atom-number*
				      (apply + (map (lambda (a)
						       (if (eq? (atom-color a)
								*color*)
							   1
							   0))
						    *atoms*)))
				(refresh-status-gauge!))))))
      box))

;*---------------------------------------------------------------------*/
;*    make-status-panel ...                                            */
;*---------------------------------------------------------------------*/
(define (make-status-panel parent)
   (let ((b (instantiate::box
	       (parent parent)
	       (orientation 'horizontal)
	       (padding 2)
	       (border-width 2))))
      (set! *status-gauge*
	    (instantiate::gauge
	       (parent b)
	       (style '(value))
	       (text "Contamination %")
	       (height 20)
	       (value 0)))
      (set! *status-lbl*
	    (instantiate::label
	       (parent b)
	       (text "Atoms: 0 / 0"))))
   (instantiate::scale
      (orientation 'horizontal)
      (parent parent)
      (from 2)
      (to 100)
      (value *speed*)
      (show-value? #t)
      (tooltips "Speed")
      (command (lambda (e) (set! *speed* (scale-value (event-widget e)))))))

;*---------------------------------------------------------------------*/
;*    add-atoms ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-atoms num)
   (define (add-atom atom)
      (define (collision atom2)
	 (if (not (eq? atom atom2))
	     (with-access::atom atom (color %ci)
		;; avoid multiple contamination of the same object
		;; at the same instant
		(if (not (eq? color *color*))
		    (begin
		       (set! *ill-atom-number* (+fx 1 *ill-atom-number*))
		       (refresh-status-gauge!)
		       (set! color *color*)
		       (with-access::canvas-ellipse %ci (color outline)
			  (set! color *color*)
			  (set! outline *color*)))))))
      (define (move)
	 (with-access::atom atom (dx dy oldx oldy %ci)
	    (if (< oldx 0) (set! dx (absfx dx)))
	    (if (> oldx *width*) (set! dx (- (absfx dx))))
	    (if (< oldy 0) (set! dy (absfx dy)))
	    (if (> oldy *height*) (set! dy (- (absfx dy))))
	    (set! oldx (+ oldx dx))
	    (set! oldy (+ oldy dy))
	    (canvas-item-move %ci dx dy)))
      (let* ((prgm (lambda ()
		      (let loop ()
			 (move)
			 (with-access::atom atom (oldx oldy color)
			    (let* ((ev-x (/fx oldx *event-width*))
				   (ev-y (/fx oldy *event-width*))
				   (sgn (cons ev-x ev-y)))
			       (if (eq? color *color*)
				   (begin
				      (broadcast! sgn atom)
				      (thread-yield!))
				   (for-each collision
					     (thread-get-values! sgn)))))
			 (loop))))
	     (th (instantiate::fthread (body prgm))))
	 (thread-cleanup-set! th (lambda (v) (destroy (atom-%ci atom))))
	 (thread-start! th *scheduler*)))
   (do ((j 0 (+fx j 1)))
       ((=fx j num) #unspecified)
       (add-atom (instantiate::atom))))

;*---------------------------------------------------------------------*/
;*    refresh-status-gauge! ...                                        */
;*---------------------------------------------------------------------*/
(define (refresh-status-gauge!)
   (with-access::gauge *status-gauge* (value)
      (set! value
	    (inexact->exact
	     (*fl 100. (/fl (exact->inexact *ill-atom-number*)
			    (exact->inexact *atom-number*))))))
   (with-access::label *status-lbl* (text)
      (set! text (string-append "Atoms: "
				(number->string *ill-atom-number*)
				" / "
				(number->string *atom-number*)))))

   

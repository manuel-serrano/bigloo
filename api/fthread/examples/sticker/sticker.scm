;*=====================================================================*/
;*    .../project/bigloo/api/fthread/examples/sticker/sticker.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 15 14:35:26 2002                          */
;*    Last change :  Wed Jul 22 16:05:03 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A demonstration of walkers in a ground                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sticker
   (library biglook fthread)
   (main    main)
   (static  (class sticker
	       tx::int
	       ty::int
	       x::int
	       y::int
	       (%ci read-only (default #unspecified)))))

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
;*    *ground* ...                                                     */
;*---------------------------------------------------------------------*/
;; window configuration
(define *transient* #f)
(define *winx* #f)
(define *winy* #f)

;; ground size
(define *ground-width* 400)
(define *ground-height* 400)

;; sticker size
(define *thickness* 10)

;; stickers environnment to store positions
(define *env* (make-env))

;; speed
(define *speed* 10)

;; GUI objects
(define *canvas* #f)
(define *status-lbl* #f)
(define *stickers* 0)

;; Global scheduler
(define *scheduler* #f)

;; colors
(define *green* (instantiate::rgb-color (green #xff)))
(define *yellow* (instantiate::rgb-color (red #xff) (green #xec) (blue #x18)))
(define *orange* (instantiate::rgb-color (red #xff) (green #x7f) (blue #x17)))
(define *red* (instantiate::rgb-color (red #xff)))

;*---------------------------------------------------------------------*/
;*    make-env ...                                                     */
;*---------------------------------------------------------------------*/
(define (make-env)
   (make-ftenv2d (+fx 1 (/fx *ground-width* *thickness*))
		 (+fx 1 (/fx *ground-height* *thickness*))))

;*---------------------------------------------------------------------*/
;*    add-stickers ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-stickers num)
   (let loop ((i num))
      (if (> i 0)
	  (begin
	     (add-sticker!)
	     (loop (- i 1))))))

;*---------------------------------------------------------------------*/
;*    ->pos ...                                                        */
;*---------------------------------------------------------------------*/
(define (->pos x y)
   (cons (/fx x *thickness*) (/fx y *thickness*)))

;*---------------------------------------------------------------------*/
;*    add-sticker! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-sticker!)
   (set! *stickers* (+fx 1 *stickers*))
   (with-access::label *status-lbl* (text)
      (set! text (string-append "Stickers: " (number->string *stickers*))))
   ;; we search the first available slot
   (let loop ((i 0))
      (let liip ((j 0))
	 (let ((pos (->pos i j)))
	    (cond
	       ((not (ftenv-lookup *env* pos))
		(let* ((c (instantiate::rgb-color
			     (red (random 255))
			     (green (random 255))
			     (blue (random 255))))
		       (w (instantiate::sticker
			     (x i)
			     (y j)
			     (tx (- *ground-width* *thickness*))
			     (ty j)
			     (%ci (instantiate::canvas-ellipse
				     (canvas *canvas*)
				     (x i)
				     (y j)
				     (width 5)
				     (height 5)
				     (color c)
				     (outline c))))))
		   (ftenv-bind! *env* pos w)
		   (let ((th (make-thread (make-stick w))))
		      (thread-cleanup-set! th (lambda (v)
						 (destroy (sticker-%ci w))))
		      (thread-start! th *scheduler*))))
	       ((<fx j (- *ground-height* *thickness*))
		(liip (+fx j *thickness*)))
	       ((<fx i (- *ground-width* *thickness*))
		(loop (+fx i *thickness*)))
	       (else
		(error "add-sticker!" "No more rooms!" #f)))))))

;*---------------------------------------------------------------------*/
;*    move-sticker ...                                                 */
;*---------------------------------------------------------------------*/
(define (move-sticker sticker)
   (with-access::sticker sticker (x y tx ty %ci)
      (define (delta tx x)
	 (cond
	    ((>fx tx x)
	     (cond
		((>fx (-fx tx 40) x) 4)
		((>fx (-fx tx 20) x) 3)
		((>fx (-fx tx 10) x) 2)
		(else 1)))
	    ((<fx tx x)
	     (cond
		((<fx (-fx tx 30) x) -4)
		((<fx (-fx tx 20) x) -3)
		((<fx (-fx tx 10) x) -2)
		(else -1)))
	    (else 0)))
      (let ((old-pos (->pos x y)))
	 (let liip ((dx (delta tx x))
		    (dy (delta ty y)))
	    (if (not (and (=fx dx 0) (=fx dy 0)))
		(let ((new-pos (->pos (+fx x dx) (+fx y dy))))
		   (cond
		      ((or (equal? old-pos new-pos)
			   (not (ftenv-lookup *env* new-pos)))
		       (ftenv-bind! *env* old-pos #f)
		       (ftenv-bind! *env* new-pos sticker)
		       (set! x (+fx x dx))
		       (set! y (+fx y dy))
		       (canvas-item-move %ci dx dy))
		      ((=fx dy 0)
		       (liip (+fx dx (if (<fx dx 0) 1 -1)) dy))
		      ((=fx dx 0)
		       (liip dx (+fx dy (if (<fx dy 0) 1 -1))))
		      (else
		       (if (=fx (random 2) 0)
			   (liip dx (+fx dy (if (<fx dy 0) 1 -1)))
			   (liip (+fx dx (if (<fx dx 0) 1 -1)) dy))))))))))

;*---------------------------------------------------------------------*/
;*    colorize-sticker ...                                             */
;*---------------------------------------------------------------------*/
(define (colorize-sticker sticker)
   (with-access::sticker sticker (x y tx ty %ci)
      (with-access::canvas-ellipse %ci (color outline)
	 (let ((d (inexact->exact
		   (sqrt (+ (expt (abs (- tx x)) 2)
			    (expt (abs (- ty y)) 2))))))
	    (cond
	       ((>=fx d 200)
		(set! color *green*)
		(set! outline *green*))
	       ((>=fx d 100)
		(set! color *yellow*)
		(set! outline *yellow*))
	       ((>=fx d 50)
		(set! color *orange*)
		(set! outline *orange*))
	       (else
		(set! color *red*)
		(set! outline *red*)))))))

;*---------------------------------------------------------------------*/
;*    make-stick ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-stick sticker)
   (lambda ()
      (let loop ()
	 (move-sticker sticker)
	 (colorize-sticker sticker)
	 (let ((val (thread-await! 'target 1)))
	    (if (pair? val)
		(begin
		   (with-access::sticker sticker (tx ty)
		      (set! tx (car val))
		      (set! ty (cdr val)))
		   (thread-yield!))))
	 (loop))))

;*---------------------------------------------------------------------*/
;*    motion-event ...                                                 */
;*---------------------------------------------------------------------*/
(define (motion-event e)
   (with-access::event e (widget x y)
      (scheduler-broadcast! *scheduler* 'target (cons x y))))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (args-parse (cdr argv)
      ((("-h" "--help") (help "This help message"))
       (args-parse-usage #f)
       (exit 0))
      ((("-s" "--speed") ?speed (help "Speed"))
       (set! *speed* (string->integer speed)))
      ((("-t" "--transient") (help "Transient window"))
       (set! *transient* #t))
      ((("-g" "-geometry" "--geometry") ?geo (help "Set the window geometry"))
       (let ((r (pregexp-match "([0-9]+)x([0-9]+)[+]([0-9]+)[+]([0-9]+)" geo)))
	  (if (pair? r)
	      (begin
		 (set! *ground-width* (string->integer (cadr r)))
		 (set! *ground-height* (string->integer (caddr r)))
		 (set! *winx* (string->integer (cadddr r)))
		 (set! *winy* (string->integer (car (cddddr r)))))
	      (let ((r (pregexp-match "([0-9]+)x([0-9]+)" geo)))
		 (if (pair? r)
		     (begin
			(set! *ground-width* (string->integer (cadr r)))
			(set! *ground-height* (string->integer (caddr r)))))))))
      (else
       (error "stick" "Illegal parameter" else)))
   ;; allocate the global canvas
   (let* ((win (instantiate::window
		  (transient *transient*)
		  (padding 2)))
	  (panel (make-sticker-panel win))
	  (canvas (instantiate::canvas
		     (parent `(,win :expand #t :fill both))
		     (width *ground-width*)
		     (height *ground-height*)
		     (event (instantiate::event-handler
			       (motion motion-event)))))
	  (status (make-status-panel win)))
      (if *winx* (window-x-set! win *winx*))
      (if *winy* (window-y-set! win *winy*))
      ;; initialize the scheduler
      (set! *scheduler* (make-scheduler))
      ;; store the canvas
      (set! *canvas* canvas)
      ;; start reacting
      (let loop ((i 0))
	 (scheduler-react! *scheduler*)
	 (after *speed* (lambda () (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    make-sticker-panel ...                                           */
;*---------------------------------------------------------------------*/
(define (make-sticker-panel parent)
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
		   (command (lambda (b) (add-stickers 1)))))
	  (add10 (instantiate::button
		    (parent box)
		    (text "10")
		    (command (lambda (b) (add-stickers 10)))))
	  (add100 (instantiate::button
		     (parent box)
		     (text "100")
		     (command (lambda (b) (add-stickers 100))))))
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
      (set! *status-lbl*
	    (instantiate::label
	       (parent b)
	       (text "Stickers: 0"))))
   (instantiate::scale
      (orientation 'horizontal)
      (parent parent)
      (from 2)
      (to 100)
      (value *speed*)
      (show-value? #t)
      (tooltips "Speed")
      (command (lambda (e) (set! *speed* (scale-value (event-widget e)))))))


		   
   

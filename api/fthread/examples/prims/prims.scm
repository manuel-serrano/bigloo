;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/examples/prims/prims.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 13 14:24:10 2001                          */
;*    Last change :  Sat Jan 23 19:42:04 2010 (serrano)                */
;*    Copyright   :  2001-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Prim numbers demonstration                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module oicobjs
   (library biglook fthread)
   (main    main)
   (static  (class num
	       (num-initialize!)
	       (id::symbol read-only (default (gensym)))
	       (%ci (default #unspecified))
	       (thread (default #f))
	       (pray (default #f))
	       (prim (default #f))
	       (value::int (default 0))
	       (oldx::long (default (random *width*)))
	       (oldy::long (default (random *height*)))
	       (dx::long (default 0))
	       (dy::long (default 0)))))

;*---------------------------------------------------------------------*/
;*    Global configuration ...                                         */
;*---------------------------------------------------------------------*/
(define *transient* #f)
(define *winx* #f)
(define *winy* #f)
(define *width* 600)
(define *height* 500)
(define *canvas* #unspecified)
(define *status-lbl* #unspecified)

(define *live* 0)
(define *multiple* 0)

(define *speed* 16)
(define *play* #t)

(define *color* (instantiate::rgb-color
		   (red #xff)))

;*---------------------------------------------------------------------*/
;*    num-direction-reset! ...                                         */
;*---------------------------------------------------------------------*/
(define (num-direction-reset! a)
   (with-access::num a (dx dy)
      (let loop ()
	 (set! dx (- (random 6) 3))
	 (set! dy (- (random 6) 3))
	 (if (and (=fx dx 0) (=fx dy 0))
	     (loop)))))
	 
;*---------------------------------------------------------------------*/
;*    num-initialize! ...                                              */
;*---------------------------------------------------------------------*/
(define (num-initialize! a::num)
   (set! *live* (+fx 1 *live*))
   (refresh-status!)
   (with-access::num a (value %ci oldx oldy)
      (set! %ci (instantiate::canvas-text
		   (canvas *canvas*)
		   (x oldx)
		   (y oldy)
		   (text (integer->string value))))
      (num-direction-reset! a)))
		
;*---------------------------------------------------------------------*/
;*    Reactive control                                                 */
;*---------------------------------------------------------------------*/
(define *scheduler* #unspecified)

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
      ((("-t" "--transient") (help "Transient window"))
       (set! *transient* #t))
      ((("-g" "-geometry" "--geometry") ?geo (help "Set the window geometry"))
       (let ((r (pregexp-match "([0-9]+)x([0-9]+)[+]([0-9]+)[+]([0-9]+)" geo)))
	  (if (pair? r)
	      (begin
		 (set! *width* (string->integer (cadr r)))
		 (set! *height* (string->integer (caddr r)))
		 (set! *winx* (string->integer (cadddr r)))
		 (set! *winy* (string->integer (car (cddddr r)))))
	      (let ((r (pregexp-match "([0-9]+)x([0-9]+)" geo)))
		 (if (pair? r)
		     (begin
			(set! *width* (string->integer (cadr r)))
			(set! *height* (string->integer (caddr r)))))))))
     (else
      '_))
   (let* ((win (if (pair? (command-line))
		   (instantiate::window
		      (transient *transient*)
		      (padding 2))
		   (instantiate::applet)))
	  (fr (instantiate::frame
		 (parent win)
		 (border-width 2)))
	  (panel (make-icobj-panel fr))
	  (canvas (instantiate::canvas
		     (parent `(,fr :expand #t :fill both))
		     (width *width*)
		     (height *height*)
		     (event (instantiate::event-handler
			       (configure configure-canvas)))))
	  (status (make-status-panel fr)))
   (if *winx* (window-x-set! win *winx*))
   (if *winy* (window-y-set! win *winy*))
   ;; store the canvas
   (set! *canvas* canvas)
   ;; init the scheduler
   (set! *scheduler* (make-scheduler))
   ;; start reacting
   (let loop ()
      (if *play* (scheduler-react! *scheduler*))
      (after *speed* loop))))

;*---------------------------------------------------------------------*/
;*    configure-canvas ...                                             */
;*---------------------------------------------------------------------*/
(define (configure-canvas e)
   (with-access::canvas (event-widget e) (width height scroll-width scroll-height)
      (set! scroll-width width)
      (set! scroll-height height)
      (set! *width* width)
      (set! *height* height)))
   
;*---------------------------------------------------------------------*/
;*    make-icobj-panel ...                                             */
;*---------------------------------------------------------------------*/
(define (make-icobj-panel parent)
   (let* ((box (instantiate::box
		  (parent parent)
		  (padding 2)
		  (orientation 'horizontal)))
	  (add1 (instantiate::button
		   (parent box)
		   (text "1")
		   (command (lambda (b) (add-nums 1)))))
	  (add10 (instantiate::button
		    (parent box)
		    (text "10")
		    (command (lambda (b) (add-nums 10)))))
	  (add100 (instantiate::button
		     (parent box)
		     (text "100")
		     (command (lambda (b) (add-nums 100)))))
	  (add1000 (instantiate::button
		      (parent box)
		      (text "1000")
		      (command (lambda (b) (add-nums 1000)))))
	  (cbut (instantiate::check-button
		   (parent box)
		   (text "Run/Pause")
		   (on #t)
		   (command (lambda (e)
			       (with-access::check-button (event-widget e) (on)
				  (set! *play* on)))))))
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
	       (text "Nums: 0 / 0"))))
   (instantiate::scale
      (orientation 'horizontal)
      (parent parent)
      (from 2)
      (to 20)
      (value *speed*)
      (show-value? #t)
      (tooltips "Speed")
      (command (lambda (e)
		  (set! *speed* (scale-value (event-widget e)))))))

;*---------------------------------------------------------------------*/
;*    kill-num ...                                                     */
;*---------------------------------------------------------------------*/
(define (kill-num num)
   (set! *live* (-fx *live* 1))
   (set! *multiple* (-fx *multiple* 1))
   (refresh-status!)
   (destroy (num-%ci num))
   (num-%ci-set! num #f))

;*---------------------------------------------------------------------*/
;*    move-num ...                                                     */
;*---------------------------------------------------------------------*/
(define (move-num num)
   (with-access::num num (dx dy oldx oldy %ci pray)
      (if (not pray)
	  (begin
	     (if (< oldx 0) (set! dx (absfx dx)))
	     (if (> oldx *width*) (set! dx (- (absfx dx))))
	     (if (< oldy 0) (set! dy (absfx dy)))
	     (if (> oldy *height*) (set! dy (- (absfx dy))))
	     (set! oldx (+ oldx dx))
	     (set! oldy (+ oldy dy))
	     (canvas-item-move %ci dx dy))
	  (if (and pray (not (num-%ci pray)))
	      (set! pray #f)))))

;*---------------------------------------------------------------------*/
;*    detect ...                                                       */
;*---------------------------------------------------------------------*/
(define (detect num num2)
   (if (and (not (eq? num num2))
	    (or (not (num-pray num))
		(eq? (num-pray num) num2))
	    (> (num-value num2) (num-value num))
	    (= (modulofx (num-value num2) (num-value num)) 0))
       (begin
	  (if (not (num-prim num2))
	      (begin
		 (set! *multiple* (+fx 1 *multiple*))
		 (num-prim-set! num2 #t)
		 (canvas-item-color-set! (num-%ci num2) *color*)
		 (refresh-status!)))
	  (num-pray-set! num num2)
	  (let ((px (num-oldx num2))
		(py (num-oldy num2)))
	     (with-access::num num (oldx oldy dx dy %ci)
		(cond
		   ((< px oldx)
		    (let ((d (- px oldx)))
		       (set! dx (if (> d -4) d -3))))
		   ((> px oldx)
		    (let ((d (- px oldx)))
		       (set! dx (if (< d 4) d 3))))
		   (else
		    (set! dx 0)))
		(cond
		   ((< py oldy)
		    (let ((d (- py oldy)))
		       (set! dy (if (> d -4) d -3))))
		   ((> py oldy)
		    (let ((d (- py oldy)))
		       (set! dy (if (< d 4) d 3))))
		   (else
		    (set! dy 0)))
		(set! oldx (+ oldx dx))
		(set! oldy (+ oldy dy))
		(canvas-item-move %ci dx dy)
		(if (and (<=fx (absfx (-fx (+fx dx oldx) px))
			       (maxfx 3 dx))
			 (<=fx (absfx (-fx (+fx dy oldy) py))
			       (maxfx 3 dy)))
		    (begin
		       (num-direction-reset! num)
		       (thread-terminate! (num-thread num2)))))))))

;*---------------------------------------------------------------------*/
;*    make-num-prgm ...                                                */
;*---------------------------------------------------------------------*/
(define (make-num-prgm num)
   (lambda ()
      (let loop ()
	 (move-num num)
	 (broadcast! 'num-here num)
	 (for-each (lambda (num2) (detect num num2))
		   (thread-get-values! 'num-here))
	 (loop))))

;*---------------------------------------------------------------------*/
;*    add-num ...                                                      */
;*---------------------------------------------------------------------*/
(define (add-num n)
   (let* ((num (instantiate::num (value n)))
	  (prgm (make-num-prgm num))
	  (th (instantiate::fthread (body prgm))))
      (thread-cleanup-set! th (lambda (v) (kill-num num)))
      (num-thread-set! num th)
      (thread-start! th *scheduler*)))

;*---------------------------------------------------------------------*/
;*    add-nums ...                                                     */
;*---------------------------------------------------------------------*/
(define add-nums
   (let ((num 1))
      (lambda (v)
	 (do ((j 0 (+fx j 1)))
	     ((=fx j v) #unspecified)
	     (set! num (+fx 1 num))
	     (add-num num)))))

;*---------------------------------------------------------------------*/
;*    refresh-status! ...                                              */
;*---------------------------------------------------------------------*/
(define (refresh-status!)
   (with-access::label *status-lbl* (text)
      (set! text (string-append "Live Numbers: "
				(number->string *live*)
				" Multiples: " 
				(number->string *multiple*)))))


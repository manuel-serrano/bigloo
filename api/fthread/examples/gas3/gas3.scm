;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/examples/gas3/gas3.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Mon Jun 29 09:54:49 2009 (serrano)                */
;*    Copyright   :  2002-09 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A simple module used to test the fair thread implementation.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gas3
   (library biglook fthread)
   (main    main)
   (static  (class atom
               (atom-initialize!)
               (%ci (default #unspecified))
               (id::int (default (get-atom-ident)))
	       (sgn read-only (default (cons 0 0)))
               (color (default #f))
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
(define *width* 300)
(define *height* 300)
(define *canvas* #unspecified)
(define *canvas2* #unspecified)

(define *atoms* '())
(define *atom-number* 0)
(define *ill-atom-number* 0)

(define *atom-size* 20)

(define *speed* 20)

;*---------------------------------------------------------------------*/
;*    atom-initialize! ...                                             */
;*---------------------------------------------------------------------*/
(define (atom-initialize! a::atom)
   (with-access::atom a (color id %ci oldx oldy dx dy)
      (set! color (get-color id))
      (set! %ci (instantiate::canvas-ellipse
                   (canvas *canvas*)
                   (x oldx)
                   (y oldy)
                   (width *atom-size*)
                   (height *atom-size*)
                   (color color)
                   (outline color)))
      (atom-direction-set! a)))

;*---------------------------------------------------------------------*/
;*    atom-direction-set! ...                                          */
;*---------------------------------------------------------------------*/
(define (atom-direction-set! atom)
   (with-access::atom atom (dx dy)
      (let loop ()
	 (set! dx (-fx (random 10) 5))
	 (set! dy (-fx (random 10) 5))
	 (if (and (=fx dx 0) (=fx dy 0))
	     (loop)))))

;*---------------------------------------------------------------------*/
;*    get-atom-ident ...                                               */
;*---------------------------------------------------------------------*/
(define get-atom-ident
   (let ((id -1))
      (lambda ()
	 (set! id (+fx 1 id))
	 id)))

;*---------------------------------------------------------------------*/
;*    get-color ...                                                    */
;*---------------------------------------------------------------------*/
(define get-color
   (let ((l (list (instantiate::rgb-color
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
      (lambda (n)
	 (list-ref l (remainderfx n 10)))))

;*---------------------------------------------------------------------*/
;*    *scheduler* ...                                                  */
;*---------------------------------------------------------------------*/
(define *scheduler* #unspecified)

;*---------------------------------------------------------------------*/
;*    make-gas-scheduler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-gas-scheduler)
   (let ((env (make-ftenv2d
               (+fx 1 (/fx *width* *atom-size*))
               (+fx 1 (/fx *height* *atom-size*)))))
      (make-scheduler env)))

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
      (("--size" ?s (help "Set atom size"))
       (set! *atom-size* (string->integer s)))
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
      (("--speed" ?speed (help "Speed of the animation"))
       (set! *speed* (string->integer speed)))
      (("--dbg" (help "Set debug mode"))
       (bigloo-debug-set! (+fx (bigloo-debug) 1)))
      (else
       '_))
   ;; allocate the global canvas
   (let* ((win (instantiate::window
                  (transient *transient*)
                  (padding 2)))
          (fr (instantiate::frame
               (parent `(,win :expand #t :fill both))
               (border-width 2)))
          (panel (make-icobj-panel fr))
          (canvas (instantiate::canvas
		     (width *width*)
		     (height *height*)
                     (parent `(,fr :expand #t :fill both)))))
      (if *winx* (window-x-set! win *winx*))
      (if *winy* (window-y-set! win *winy*))
      ;; initialize the scheduler
      (set! *scheduler* (make-gas-scheduler))
      ;; store the canvas
      (set! *canvas* canvas)
      (legend-canvas (instantiate::frame
			(parent `(,win :expand #t :fill both))
			(border-width 2)))
      ;; start reacting
      (let loop ()
         (scheduler-react! *scheduler*)
         (after *speed* loop))))

;*---------------------------------------------------------------------*/
;*    legend-canvas ...                                                */
;*---------------------------------------------------------------------*/
(define (legend-canvas parent)
   (set! *canvas2* (instantiate::canvas
		      (width *width*)
		      (height (+fx 5 *atom-size*))
		      (parent `(,parent :expand #t :fill both))))
   (let loop ((i 0))
      (if (<fx i 19)
	  (begin
	     (instantiate::canvas-ellipse
		(canvas *canvas2*)
		(x (+fx 2 (*fx i (+fx 2 *atom-size*))))
		(y 2)
		(width *atom-size*)
		(height *atom-size*)
		(color (get-color i))
		(outline (get-color i)))
	     (loop (+fx i 1))))))
   
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
          (add40 (instantiate::button
		    (parent box)
		    (text "40")
		    (command (lambda (b) (add-atoms 40)))))
	  (add100 (instantiate::button
                     (parent box)
                     (text "100")
                     (command (lambda (b) (add-atoms 100))))))
      box))

;*---------------------------------------------------------------------*/
;*    make-atom-prgm ...                                               */
;*---------------------------------------------------------------------*/
(define (make-atom-prgm a1)
   (define (collision a2)
      (if (not (eq? a1 a2))
	  (let ((id2 (atom-id a2))
		(id1 (atom-id a1)))
	     (if (>fx id2 id1)
		 (let ((nc (canvas-ellipse-color (atom-%ci a2)))
		       (el (atom-%ci a1)))
		    (atom-id-set! a1 id2)
		    (with-access::canvas-ellipse el (color outline)
		       (set! color nc)
		       (set! outline nc))))
	     '(atom-direction-set! a1))))
   (define (move)
      (with-access::atom a1 (dx dy oldx oldy %ci)
	 (if (< oldx 0) (set! dx (absfx dx)))
	 (if (> oldx *width*) (set! dx (- (absfx dx))))
	 (if (< oldy 0) (set! dy (absfx dy)))
	 (if (> oldy *height*) (set! dy (- (absfx dy))))
	 (set! oldx (+ oldx dx))
	 (set! oldy (+ oldy dy))
	 (canvas-item-move %ci dx dy)))
   (define (make-evt)
      (with-access::atom a1 (oldx oldy)
	 (cons (/fx oldx *atom-size*) (/fx oldy *atom-size*))))
   (lambda ()
      (let loop ()
	 (move)
	 (let ((evt (make-evt)))
	    (broadcast! evt a1)
	    (for-each collision (thread-get-values! evt)))
	 (loop))))

;*---------------------------------------------------------------------*/
;*    add-atoms ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-atoms num)
   (define (add-atom atom)
      (let ((prgm (make-atom-prgm atom)))
         (thread-start! (instantiate::fthread (body prgm)) *scheduler*)))
   (do ((j 0 (+fx j 1)))
       ((=fx j num) #unspecified)
       (add-atom (instantiate::atom))))



   

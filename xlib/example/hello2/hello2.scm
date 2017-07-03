;; another hello world (more sophisticated than tue usual one), gently
;; provided by David Fox
(module hello
   (include "xlib.sch"))

(define (bit-or* . args)
  (if (null? args) 0 (bit-or (car args) (apply bit-or* (cdr args)))))

;; A wrapper around XLookupString.  Usage:
;;
;;    (x:LookupString event)
;;    (x:LookupString event keysym)
;;    (x:LookupString event keysym status)

(define x:LookupString 
  (let ((Xlookupstring-buffer (make-string 50)))
    (lambda (event . opt)
      (let* ((keysym (if (pair? opt)
			 (car opt)
			 ;; This is how you make a null pointer.
			 (pragma::KeySym* "(KeySym*)0")))
	     (status (if (= (length opt) 2)
			 (cadr opt)
			 (pragma::XComposeStatus* "(XComposeStatus*)0")))
	     (result (XLookupString (XEvent*-xkey event)
				    xlookupstring-buffer 50
				    keysym status)))
	(if (not (null? opt))
	    (list (substring xlookupstring-buffer 0 result)
		  (if (car opt) keysym #f))
	    (substring xlookupstring-buffer 0 result))))))

(let* ((hello "Hello, World")
       (hi "Hi!")
       (dpy (let ((x (XOpenDisplay "")))
	      (if (display*-null? x)
		  (error "hello-world" "DISPLAY is not defined" #t))
	      x))
       (screen (XDefaultScreen dpy))
       (background (XWhitePixel dpy screen))
       (foreground (XBlackPixel dpy screen))
       (window (XCreateSimpleWindow dpy (XDefaultRootWindow dpy)
				    200 300 350 250 5 foreground background))
       (gc (XCreateGC dpy window 0 (make-XGCValues*)))
       (event (make-XEvent*)))

  (XStoreName dpy window "Hello, World in bigloo using Xlib")
  (XSetIconName dpy window "hello")
  (XSetBackground dpy gc background)
  (XSetForeground dpy gc foreground)
  (XSelectInput dpy window 
		(bit-or* ButtonPressMask KeyPressMask ExposureMask))
  (XMapRaised dpy window)
  (let loop ()
    (XNextEvent dpy event)
    (cond
     ((eq? (XEvent*-type event) Expose)
      (XDrawImageString
       (XExposeEvent*-display (XEvent*-xexpose event))
       (XExposeEvent*-window (XEvent*-xexpose event))
       gc 50 50 hello (string-length hello))
      (loop))
     ((eq? (XEvent*-type event) mappingnotify)
      (XRefreshKeyboardMapping (XEvent*-xmapping event))
      (loop))
     ((eq? (XEvent*-type event) ButtonPress)
      (let ((event (XEvent*-xbutton event)))
	(XDrawImageString
	 (XButtonEvent*-display event)
	 (XButtonEvent*-window event) gc
	 (XButtonEvent*-x event) (XButtonEvent*-y event)
	 hi (string-length hi))
	(loop)))
     ((and (eq? (XEvent*-type event) keypress)
	   (equal? (x:LookupString event) "q"))
      (XFreeGC dpy gc)
      (XDestroyWindow dpy window)
      (XCloseDisplay dpy))
     (else (loop)))))

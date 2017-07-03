;*=====================================================================*/
;*    serrano/prgm/project/bigloo/xlib/example/hello/xhw1.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  5 10:26:41 1995                          */
;*    Last change :  Wed Jun 25 08:27:28 1997 (serrano)                */
;*    -------------------------------------------------------------    */
;*    MIT standard X11R5 distribution xhw1.c example.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hello
   (main hello-world)
   
   (foreign (type int*    (pointer int)    "int *")
	    (type string* (pointer string) "char **")
	    
	    (macro int*    &int    (int)    "&")
	    (macro string* &string (string) "&"))
   
   (include "../../xlib.sch"))

;*---------------------------------------------------------------------*/
;*    usage ...                                                        */
;*---------------------------------------------------------------------*/
(define (usage)
   (print "usage: hello [-help] [-bg <color>] [-fg <color>] [-font <name>] [-display <name>] [-string <str>]")
   (exit -1))

;*---------------------------------------------------------------------*/
;*    *executable-name* ...                                            */
;*---------------------------------------------------------------------*/
(define *executable-name* "")

;*---------------------------------------------------------------------*/
;*    *string* ...                                                     */
;*---------------------------------------------------------------------*/
(define *string*       "Hello World")
(define *default-font* "fixed")
(define *font*         #f)
(define *fg*           #f)
(define *bg*           #f)
(define *bd*           #f)
(define *display*      (let ((name (getenv "DISPLAY")))
			  (if (string? name)
			      name
			      "0")))

;*---------------------------------------------------------------------*/
;*    parse-args! ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-args! argv)
   (let loop ((argv argv))
      (cond
	 ((null? argv)
	  #f)
	 ((string=? (car argv) "-help")
	  (usage))
	 ((string=? (car argv) "-string")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *string* (cadr argv))
		 (loop (cddr argv)))))
	 ((string=? (car argv) "-font")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *font* (cadr argv))
		 (loop (cddr argv)))))
	 ((string=? (car argv) "-display")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *display* (cadr argv))
		 (loop (cddr argv)))))
	 ((string=? (car argv) "-fg")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *fg* (cadr argv))
		 (loop (cddr argv)))))
	 ((string=? (car argv) "-bg")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *bg* (cadr argv))
		 (loop (cddr argv)))))
	 ((string=? (car argv) "-bd")
	  (if (null? (cdr argv))
	      (error "hello"
		     (string-append (car argv) " requires one argument")
		     argv)
	      (begin
		 (set! *bd* (cadr argv))
		 (loop (cddr argv)))))
	 (else
	  (loop (cdr argv))))))
   
;*---------------------------------------------------------------------*/
;*    fontsize ...                                                     */
;*---------------------------------------------------------------------*/
(define-struct fontsize fth ftw)

;*---------------------------------------------------------------------*/
;*    pixelcolor ...                                                   */
;*---------------------------------------------------------------------*/
(define-struct pixelcolor bd bg fg bw)

;*---------------------------------------------------------------------*/
;*    open-display ...                                                 */
;*---------------------------------------------------------------------*/
(define (open-display dpname)
   (let ((dpy (xopendisplay dpname)))
      (if (display*-null? dpy)
	  (error "hello" "Can't open display" #f)
	  dpy)))

;*---------------------------------------------------------------------*/
;*    get-font-name ...                                                */
;*---------------------------------------------------------------------*/
(define (get-font-name dpy)
   (if (string? *font*)
       *font*
       (let ((fontname (xgetdefault dpy *executable-name* "font")))
	  (if (string-null? fontname)
	      *default-font*
	      fontname))))

;*---------------------------------------------------------------------*/
;*    get-font-struct ...                                              */
;*---------------------------------------------------------------------*/
(define (get-font-struct dpy fontname)
   (let ((fontstruct (xloadqueryfont dpy fontname)))
      (if (foreign-null? fontstruct)
	  (error "hello"
		 (string-append "display `"
				(displaystring dpy)
				"' doesn't know font")
		 fontname)
	  fontstruct)))
   
;*---------------------------------------------------------------------*/
;*    get-font-size ...                                                */
;*---------------------------------------------------------------------*/
(define (get-font-size fontstruct)
   (let* ((xcs (xfontstruct*-max_bounds fontstruct)))
      (let ((fth (+ (xcharstruct*-ascent xcs)
		    (xcharstruct*-descent xcs)))
	    (ftw (xcharstruct*-width xcs)))
	 (fontsize fth ftw))))

;*---------------------------------------------------------------------*/
;*    get-pixelcolor ...                                               */
;*---------------------------------------------------------------------*/
(define (get-pixelcolor dpy cmap)
   (let ((color (make-xcolor*)))
      (let ((bd (let ((tempstr (if (string? *bd*)
				   *bd*
				   (xgetdefault dpy
						*executable-name*
						"bordercolor"))))
		   (if (or (string-null? tempstr)
			   (=fx (xparsecolor dpy cmap tempstr color) 0)
			   (=fx (xalloccolor dpy cmap color) 0))
		       (whitepixel dpy (defaultscreen dpy))
		       (xcolor*-pixel color))))
	    (bg (let ((tempstr (if (string? *bg*)
				   *bg*
				   (xgetdefault dpy
						*executable-name*
						"background"))))
		   (if (or (string-null? tempstr)
			   (=fx (xparsecolor dpy cmap tempstr color) 0)
			   (=fx (xalloccolor dpy cmap color) 0))
		       (blackpixel dpy (defaultscreen dpy))
		       (xcolor*-pixel color))))
	    (fg (let ((tempstr (if (string? *fg*)
				   *fg*
				   (xgetdefault dpy
						*executable-name*
						"foreground"))))
		   (if (or (string-null? tempstr)
			   (=fx (xparsecolor dpy cmap tempstr color) 0)
			   (=fx (xalloccolor dpy cmap color) 0))
		       (whitepixel dpy (defaultscreen dpy))
		       (xcolor*-pixel color))))
	    (bw (let ((tempstr (xgetdefault dpy
					    *executable-name*
					    "border")))
		   (if (string-null? tempstr)
		       1
		       (string->integer tempstr)))))
	 (pixelcolor bd bg fg bw))))

;*---------------------------------------------------------------------*/
;*    get-xsizehints ...                                               */
;*---------------------------------------------------------------------*/
(define (get-xsizehints dpy fontstruct fsize pxcolor)
   (let ((geomspec (xgetdefault dpy *executable-name* "geometry"))
	 (pad      1))
      (if (string-null? geomspec)
	  (let ((xsh (make-xsizehints*)))
	     (xsizehints*-flags-set!  xsh (bit-or PPosition PSize))
	     (let* ((height (+fx (fontsize-fth fsize) (*fx 2 pad)))
		    (width  (xtextwidth fontstruct
					*string*
					(+fx (string-length *string*)
					     (*fx pad 2))))
		    (x     (/fx (-fx (displaywidth dpy (defaultscreen dpy))
				     width)
				2))
		    (y     (/fx (-fx (displayheight dpy (defaultscreen dpy))
				     height)
				2)))
		(xsizehints*-height-set! xsh height)
		(xsizehints*-width-set!  xsh width)
		(xsizehints*-x-set!      xsh x)
		(xsizehints*-y-set!      xsh y)
		xsh))
	  (let* ((xsh     (make-xsizehints*))
		 (bw      (pixelcolor-bw pxcolor))
		 (ftw     (fontsize-ftw fsize))
		 (fth     (fontsize-fth fsize))
		 (bitmask (xgeometry dpy
				     (defaultscreen dpy)
				     geomspec
				     ""
				     bw
				     ftw
				     fth
				     pad
				     pad
				     (&int (xsizehints*-x xsh))
				     (&int (xsizehints*-y xsh))
				     (&int (xsizehints*-width xsh))
				     (&int (xsizehints*-height xsh)))))
	     (if (>fx (bit-and bitmask (bit-or Xvalue YValue)) 0)
		 (xsizehints*-flags-set! xsh (bit-or (xsizehints*-flags xsh)
						     USPosition)))
	     (if (>fx (bit-and bitmask (bit-or WidthValue HeightValue)) 0)
		 (xsizehints*-flags-set! xsh (bit-or (xsizehints*-flags xsh)
						     USSize)))
	     xsh))))

;*---------------------------------------------------------------------*/
;*    create-window ...                                                */
;*---------------------------------------------------------------------*/
(define (create-window dpy xsh pxcolor)
   (xcreatesimplewindow dpy
			(defaultrootwindow dpy)
			(xsizehints*-x xsh)
			(xsizehints*-y xsh)
			(xsizehints*-width xsh)
			(xsizehints*-height xsh)
			(pixelcolor-bw pxcolor)
			(pixelcolor-bd pxcolor)
			(pixelcolor-bg pxcolor)))

;*---------------------------------------------------------------------*/
;*    get-xswa ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-xswa dpy)
   (let ((xswa (make-xsetwindowattributes*)))
      (xsetwindowattributes*-colormap-set!    xswa
					      (defaultcolormap
						 dpy
						 (defaultscreen dpy)))
      (xsetwindowattributes*-bit_gravity-set! xswa
					      centergravity)
      xswa))

;*---------------------------------------------------------------------*/
;*    get-gcv ...                                                      */
;*---------------------------------------------------------------------*/
(define (get-gcv dpy fontstruct pxcolor)
   (let ((gcv (make-xgcvalues*)))
      (xgcvalues*-font-set! gcv (xfontstruct*-fid fontstruct))
      (xgcvalues*-foreground-set! gcv (pixelcolor-fg pxcolor))
      (xgcvalues*-background-set! gcv (pixelcolor-bg pxcolor))
      gcv))

;*---------------------------------------------------------------------*/
;*    get-gc ...                                                       */
;*---------------------------------------------------------------------*/
(define (get-gc dpy win gcv)
   (xcreategc dpy win (bit-or (bit-or gcfont gcforeground) gcbackground) gcv))

;*---------------------------------------------------------------------*/
;*    loop ...                                                         */
;*---------------------------------------------------------------------*/
(define (loop dpy event fontstruct gc win)
   (print "looping...")
   (let loop ()
      (xnextevent dpy event)
      (if (and (=fx (xevent*-type event) expose)
	       (=fx (xexposeevent*-count (xevent*-xexpose event)) 0))
	  (let ((xwa (make-xwindowattributes*)))
	     (let laap ((test (xchecktypedevent dpy expose event)))
		(if (=fx test 0)
		    'done
		    (laap (xchecktypedevent dpy expose event))))
	     (if (=fx (xgetwindowattributes dpy win xwa) 0)
		 'done
		 (let ((x (/fx (-fx (xwindowattributes*-width xwa)
				    (xtextwidth fontstruct
						*string*
						(string-length *string*)))
			       2))
		       (y (/fx (-fx (+fx (xwindowattributes*-height xwa)
					 (xcharstruct*-ascent
					  (xfontstruct*-max_bounds
					   fontstruct)))
				    (xcharstruct*-descent
				     (xfontstruct*-max_bounds
				      fontstruct)))
			       2)))
		    (xclearwindow dpy win)
		    (xdrawstring dpy win gc x y *string*
				 (string-length *string*))))))
      (loop)))

;*---------------------------------------------------------------------*/
;*    hello-world ...                                                  */
;*---------------------------------------------------------------------*/
(define (hello-world argv)
   (parse-args! argv)
   (set! *executable-name* (executable-name))
   (let* ((dpname  *display*)
	  (dpy     (open-display dpname))
	  (fname   (get-font-name dpy))
	  (fstruct (get-font-struct dpy fname))
	  (fsize   (get-font-size fstruct))
	  (cmap    (defaultcolormap dpy (defaultscreen dpy)))
	  (pxcolor (get-pixelcolor dpy cmap))
	  (xsh     (get-xsizehints dpy fstruct fsize pxcolor))
	  (win     (create-window dpy xsh pxcolor)))
      (print "dpname : " dpname)
      (print "dpy    : " dpy)
      (print "fname  : " fname)
      (print "fstruct: " fstruct)
      (print "fize   : " fsize)
      (print "cmap   : " cmap)
      (print "pxcolor: " pxcolor)
      (print "xsh    : " xsh)
      (print "window : " win)
      (xsetstandardproperties dpy
			      win
			      *string*
			      *string*
			      None
			      (&string *executable-name*)
			      1
			      xsh)
      (let ((xwmh (xwmhints* (bit-or InputHint StateHint)
			     False
			     NormalState
			     0
			     0
			     0 0
			     0
			     0)))
	 (print "xwmh   : " xwmh)
	 (xsetwmhints dpy win xwmh)
	 (let* ((xswa  (get-xswa dpy))
		(gcv   (get-gcv dpy fstruct pxcolor))
		(gc    (get-gc dpy win gcv))
		(event (make-xevent*)))
	    (print "xswa   : " xswa)
	    (print "gcv    : " gcv)
	    (print "gc     : " gc)
	    (print "event  : " event)
	    (xselectinput dpy win exposuremask)
	    (xmapwindow dpy win)
	    (loop dpy event fstruct gc win)))))


	  



;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/color.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 19 07:15:05 2009                          */
;*    Last change :  Thu Jan 27 10:26:52 2011 (serrano)                */
;*    Copyright   :  2009-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Color operations.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-color
   
   (export (make-hex-color::bstring ::int ::int ::int)
	   (parse-hex-color ::bstring)
	   (parse-web-color ::bstring)
	   (hsv->rgb ::int ::int ::int)
	   (rgb->hsv ::int ::int ::int)
	   (hsl->rgb ::int ::int ::int)
	   (rgb->hsl ::int ::int ::int)))

;*---------------------------------------------------------------------*/
;*    css2-colors ...                                                  */
;*    -------------------------------------------------------------    */
;*    Official CSS2 colors.                                            */
;*---------------------------------------------------------------------*/
(define css2-colors
   '(("maroon" #x80 #x00 #x00)
     ("red" #xff #x00 #x00)
     ("orange" #xff #xa5 #x00)
     ("yellow" #xff #xff #x00)
     ("olive" #x80 #x80 #x00)
     ("purple" #x80 #x00 #x80)
     ("fuchsia" #xff #x00 #xff)
     ("white" #xff #xff #xff)
     ("lime" #x00 #xff #x00)
     ("green" #x00 #x80 #x00)
     ("navy" #x00 #x00 #x80)
     ("blue" #x00 #x00 #xff)
     ("aqua" #x00 #xff #xff)
     ("teal" #x00 #x80 #x80)
     ("black" #x00 #x00 #x00)
     ("silver" #xc0 #xc0 #xc0)
     ("gray" #x80 #x80 #x80)))
     
;*---------------------------------------------------------------------*/
;*    integer->string-2 ...                                            */
;*---------------------------------------------------------------------*/
(define (integer->string-2 res o n)
   (define (digit->char d)
      (string-ref "0123456789abcdef" d))
   (if (>=fx n 16)
       (begin
	  (string-set! res o (digit->char (bit-rsh n 4)))
	  (string-set! res (+fx o 1) (digit->char (bit-and n 15))))
       (string-set! res (+fx o 1) (digit->char n))))

;*---------------------------------------------------------------------*/
;*    make-hex-color ...                                               */
;*---------------------------------------------------------------------*/
(define (make-hex-color r g b)
   (let ((res (make-string 7 #\0)))
      (string-set! res 0 #\#)
      (integer->string-2 res 1 r)
      (integer->string-2 res 3 g)
      (integer->string-2 res 5 b)
      res))

;*---------------------------------------------------------------------*/
;*    raise-color-error ...                                            */
;*---------------------------------------------------------------------*/
(define (raise-color-error color)
   (raise (instantiate::&io-parse-error
	     (proc "parse-web-color")
	     (msg "Illegal color")
	     (obj color))))

;*---------------------------------------------------------------------*/
;*    parse-hex-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-hex-color color)
   
   (define (char->int c)
      (cond
	 ((and (char>=? c #\0) (char<=? c #\9))
	  (*fx 16 (-fx (char->integer c) (char->integer #\0))))
	 ((and (char>=? c #\a) (char<=? c #\f))
	  (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
	 ((and (char>=? c #\A) (char<=? c #\F))
	  (*fx 16 (+fx 10 (-fx (char->integer c) (char->integer #\A)))))
	 (else
	  (raise-color-error color))))

   (cond
      ((or (<fx (string-length color) 4)
	   (not (char=? (string-ref color 0) #\#)))
       (raise-color-error color))
      ((=fx (string-length color) 7)
       (values (string->integer (substring color 1 3) 16)
	       (string->integer (substring color 3 5) 16)
	       (string->integer (substring color 5 7) 16)))
      ((=fx (string-length color) 4)
       (values (char->int (string-ref color 1))
	       (char->int (string-ref color 2))
	       (char->int (string-ref color 3))))
      (else
       (raise-color-error color))))

;*---------------------------------------------------------------------*/
;*    parse-rgb-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-rgb-color c)
   (cond
      ((pregexp-match "rgb([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*,[ ]*([0-9]+)[ ]*)" c)
       =>
       (lambda (m)
	  (values (string->number (cadr m))
		  (string->number (caddr m))
		  (string->number (cadddr m)))))
      ((pregexp-match "rgb([ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*)" c)
       =>
       (lambda (m)
	  (values (* 255 (/ (string->number (cadr m)) 100))
		  (* 255 (/ (string->number (caddr m)) 100))
		  (* 255 (/ (string->number (cadddr m)) 100)))))
      (else
       (raise-color-error c))))
			 
;*---------------------------------------------------------------------*/
;*    parse-hsl-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-hsl-color c)
   (cond
      ((pregexp-match "hsl([ ]*([0-9]+)[ ]*,[ ]*([0-9]+)%[ ]*,[ ]*([0-9]+)%[ ]*)" c)
       =>
       (lambda (m)
	  (hsl->rgb (string->integer (cadr m))
		    (string->integer (caddr m))
		    (string->integer (cadddr m)))))
      (else
       (raise-color-error c))))
			 
;*---------------------------------------------------------------------*/
;*    parse-web-color ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-web-color color)
   (cond
      ((=fx (string-length color) 0)
       (raise-color-error color))
      ((char=? (string-ref color 0) #\#)
       (parse-hex-color color))
      ((substring-at? color "rgb(" 0)
       (parse-rgb-color color))
      ((substring-at? color "hsl(" 0)
       (parse-hsl-color color))
      (else
       (let ((val (assoc color css2-colors)))
	  (if val
	      (apply values (cdr val))
	      (raise-color-error color))))))
   
;*---------------------------------------------------------------------*/
;*    hsv->rgb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hsv->rgb h s v)
   (let ((r 0)
	 (g 0)
	 (b 0))
      (if (> s 0)
	  (let* ((h/60 (/fl (fixnum->flonum h) 60.))
		 (fh/60 (floor h/60))
		 (hi (modulo (flonum->fixnum fh/60) 6))
		 (f (-fl h/60 fh/60))
		 (s/100 (/fl (fixnum->flonum s) 100.))
		 (v/100 (/fl (fixnum->flonum v) 100.))
		 (p (flonum->fixnum
		     (*fl 255. (*fl v/100 (-fl 1. s/100)))))
		 (q (flonum->fixnum
		     (*fl 255. (*fl v/100 (-fl 1. (*fl f s/100))))))
		 (t (flonum->fixnum
		     (*fl 255. (* v/100 (-fl 1. (*fl (-fl 1. f) s/100))))))
		 (v*255 (flonum->fixnum
			 (roundfl (*fl v/100 255.))))
		 (r 0)
		 (g 0)
		 (b 0))
	     (case hi
		((0) (set! r v*255) (set! g t) (set! b p))
		((1) (set! r q) (set! g v*255) (set! b p))
		((2) (set! r p) (set! g v*255) (set! b t))
		((3) (set! r p) (set! g q) (set! b v*255))
		((4) (set! r t) (set! g p) (set! b v*255))
		((5) (set! r v*255) (set! g p) (set! b q)))
	     (values r g b))
	  (let ((v (flonum->fixnum
		    (roundfl (*fl (/fl (fixnum->flonum v) 100.) 255.)))))
	     (values v v v)))))

;*---------------------------------------------------------------------*/
;*    h ...                                                            */
;*---------------------------------------------------------------------*/
(define (h max::double min::double r::double g::double b::double)
   (cond
      ((=fl max min)
       0)
      ((=fl max r)
       (modulofx
	(flonum->fixnum
	 (roundfl (+fl (*fl 60. (/fl (-fl g b) (-fl max min))) 360.))) 360))
      ((=fl max g)
       (flonum->fixnum
	(roundfl (+fl (* 60. (/fl (-fl b r) (-fl max min))) 120.))))
      (else
       (flonum->fixnum
	(roundfl (+fl (*fl 60. (/fl (-fl r g) (-fl max min))) 240.))))))

;*---------------------------------------------------------------------*/
;*    rgb->hsv ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb->hsv r g b)
   (define (s max::double min::double r::double g::double b::double)
      (if (=fl max 0.)
	  0
	  (flonum->fixnum (roundfl (* 100 (/ (- max min) max))))))
   (let* ((r (/fl (fixnum->flonum r) 255.))
	  (g (/fl (fixnum->flonum g) 255.))
	  (b (/fl (fixnum->flonum b) 255.))
	  (max (max r g b))
	  (min (min r g b)))
      (values (h max min r g b)
	      (s max min r g b)
	      (flonum->fixnum (roundfl (* 100 max))))))

;*---------------------------------------------------------------------*/
;*    hsl->rgb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hsl->rgb h s l)
   (define (tc t)
      (cond
	 ((<fl t .0) (+ t 1.))
	 ((>fl t 1.) (- t 1.))
	 (else t)))
   (define (colorc t p q)
      (let ((v (cond
		  ((<fl t (/fl 1. 6.))
		   (+fl p (*fl (-fl q p) (*fl 6. t))))
		  ((<fl t 0.5)
		   q)
		  ((<fl t (/fl 2. 3.))
		   (+fl p (*fl (-fl q p) (*fl 6. (-fl (/fl 2. 3.) t)))))
		  (else
		   p))))
	 (flonum->fixnum (roundfl (*fl 255. v)))))
   (if (= s 0)
       (let ((v (flonum->fixnum (roundfl (*fl (/ l 100.) 255.)))))
	  (values v v v))
       (let* ((l/100 (/fl (fixnum->flonum l) 100.))
	      (s/100 (/fl (fixnum->flonum s) 100.))
	      (q (if (<fx l 50)
		     (*fl l/100 (+fl 1. s/100))
		     (+fl l/100 (- s/100 (*fl l/100 s/100)))))
	      (p (-fl (*fl 2. l/100) q))
	      (hk (/fl (fixnum->flonum h) 360.))
	      (tcr (tc (+fl hk (/fl 1. 3.))))
	      (tcg (tc hk))
	      (tcb (tc (-fl hk (/fl 1. 3.)))))
	  (values (colorc tcr p q) (colorc tcg p q) (colorc tcb p q)))))
	 
;*---------------------------------------------------------------------*/
;*    rgb->hsl ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb->hsl r g b)
   (define (s max::double min::double r::double g::double b::double l::double)
      (cond
	 ((= max min)
	  0)
	 ((<=fl l 0.5)
	  (flonum->fixnum
	   (roundfl (*fl 100. (/fl (-fl max min) (+fl max min))))))
	 (else
	  (flonum->fixnum
	   (roundfl (*fl 100. (/fl (-fl max min) (-fl 2. (+fl max min)))))))))
   (let* ((r (/fl (fixnum->flonum r) 255.))
	  (g (/fl (fixnum->flonum g) 255.))
	  (b (/fl (fixnum->flonum b) 255.))
	  (max (maxfl r g b))
	  (min (minfl r g b))
	  (l (/fl (+fl max min) 2.)))
      (values (h max min r g b)
	      (s max min r g b l)
	      (flonum->fixnum (roundfl (*fl 100. l))))))


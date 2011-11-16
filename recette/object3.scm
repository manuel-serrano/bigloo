(module object3
   (import (object1 "object1.scm")
	   (object2 "object2.scm"))
   (export (class foo3::foo2
	      t)
	   (class bar4::bar3
	      t))
   (export (final-class 3point
	      (x (default 0))
	      (y (default 0)))
           (wide-class 3pointc::3point
	      (color (default 'black)))
           (wide-class 3point3::3point
	      (z (default 0)))
	   (3do-pointc::3pointc ::3point)
	   (3foo::3pointc ::3point)
	   (wxws::int)))

;*---------------------------------------------------------------------*/
;*    Widening and casts                                               */
;*---------------------------------------------------------------------*/
(define (3do-pointc::3pointc p::3point)
   (widen!::3pointc p
      (color 'white)))

;*---------------------------------------------------------------------*/
;*    3foo ...                                                         */
;*---------------------------------------------------------------------*/
(define (3foo::3pointc p::3point)
   (3do-pointc p))

;*---------------------------------------------------------------------*/
;*    wxws ...                                                         */
;*---------------------------------------------------------------------*/
(define (wxws)
   (let ((s (instantiate::wx (x 2))))
      (widen!::ws s (y 9))
      (with-access::ws s (y) y)))



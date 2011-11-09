(module object3-sans
   (import (object1-sans "object1-sans.scm")
	   (object2-sans "object2-sans.scm"))
   (export (class foo3-sans::foo2-sans
	      t)
	   (class bar4-sans::bar3-sans
	      t))
   (export (final-class 3point-sans
	      (x (default 0))
	      (y (default 0)))
           (wide-class 3pointc-sans::3point-sans
	      (color (default 'black)))
           (wide-class 3point3-sans::3point-sans
	      (z (default 0)))
	   (3do-pointc-sans::3pointc-sans ::3point-sans)
	   (3foo-sans::3pointc-sans ::3point-sans)
	   (wxws-sans::int)))

;*---------------------------------------------------------------------*/
;*    Widening and casts                                               */
;*---------------------------------------------------------------------*/
(define (3do-pointc-sans::3pointc-sans p::3point-sans)
   (widen!::3pointc-sans p
      (color 'white)))

;*---------------------------------------------------------------------*/
;*    3foo ...                                                         */
;*---------------------------------------------------------------------*/
(define (3foo-sans::3pointc-sans p::3point-sans)
   (3do-pointc-sans p))

;*---------------------------------------------------------------------*/
;*    wxws-sans ...                                                    */
;*---------------------------------------------------------------------*/
(define (wxws-sans)
   (let ((s (instantiate::wx-sans (x 2))))
      (widen!::ws-sans s (y 9))
      (with-access::ws-sans s (y) y)))



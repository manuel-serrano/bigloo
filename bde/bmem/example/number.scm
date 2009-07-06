;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/example/number.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 24 08:59:38 2003                          */
;*    Last change :  Thu Apr 24 09:40:50 2003 (serrano)                */
;*    Copyright   :  2003 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Classes implementing numbers                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module number
   (export (abstract-class nb)
	   (class Integer::nb (value::int read-only))
	   (class Real::nb (value::double read-only))
	   (generic +nb::nb ::nb ::nb)
	   (generic *nb::nb ::nb ::nb)
	   (generic -nb::nb ::nb ::nb)
	   (generic <nb::bool ::nb ::nb)
	   (generic =nb::bool ::nb ::nb)))

;*---------------------------------------------------------------------*/
;*    object-display ::Integer ...                                     */
;*---------------------------------------------------------------------*/
(define-method (object-display nb::Integer . port)
   (apply display (Integer-value nb) port))

;*---------------------------------------------------------------------*/
;*    object-display ::Real ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display nb::Real . port)
   (apply display (Real-value nb) port))

;*---------------------------------------------------------------------*/
;*    operators ::number ...                                           */
;*---------------------------------------------------------------------*/
(define-generic (+nb n1::nb n2::nb))
(define-generic (*nb n1::nb n2::nb))
(define-generic (-nb n1::nb n2::nb))
(define-generic (<nb n1::nb n2::nb))
(define-generic (=nb n1::nb n2::nb))

;*---------------------------------------------------------------------*/
;*    +nb ::Integer ...                                                */
;*---------------------------------------------------------------------*/
(define-method (+nb n1::Integer n2)
   (with-access::Integer n1 (value)
      (if (Integer? n2)
	  (instantiate::Integer
	     (value (+fx value (Integer-value n2))))
	  (instantiate::Real
	     (value (+fl (exact->inexact value) (Real-value n2)))))))

;*---------------------------------------------------------------------*/
;*    +nb ::Real ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (+nb n1::Real n2)
   (with-access::Real n1 (value)
      (if (Real? n2)
	  (instantiate::Real
	     (value (+fl value (Real-value n2))))
	  (instantiate::Real
	     (value (+fl value (exact->inexact (Integer-value n2))))))))

;*---------------------------------------------------------------------*/
;*    *nb ::Integer ...                                                */
;*---------------------------------------------------------------------*/
(define-method (*nb n1::Integer n2)
   (with-access::Integer n1 (value)
      (if (Integer? n2)
	  (instantiate::Integer
	     (value (*fx value (Integer-value n2))))
	  (instantiate::Real
	     (value (*fl (exact->inexact value) (Real-value n2)))))))

;*---------------------------------------------------------------------*/
;*    *nb ::Real ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (*nb n1::Real n2)
   (with-access::Real n1 (value)
      (if (Real? n2)
	  (instantiate::Real
	     (value (*fl value (Real-value n2))))
	  (instantiate::Real
	     (value (*fl value (exact->inexact (Integer-value n2))))))))

;*---------------------------------------------------------------------*/
;*    -nb ::Integer ...                                                */
;*---------------------------------------------------------------------*/
(define-method (-nb n1::Integer n2)
   (with-access::Integer n1 (value)
      (if (Integer? n2)
	  (instantiate::Integer
	     (value (-fx value (Integer-value n2))))
	  (instantiate::Real
	     (value (-fl (exact->inexact value) (Real-value n2)))))))

;*---------------------------------------------------------------------*/
;*    -nb ::Real ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (-nb n1::Real n2)
   (with-access::Real n1 (value)
      (if (Real? n2)
	  (instantiate::Real
	     (value (-fl value (Real-value n2))))
	  (instantiate::Real
	     (value (-fl value (exact->inexact (Integer-value n2))))))))

;*---------------------------------------------------------------------*/
;*    <nb ::Integer ...                                                */
;*---------------------------------------------------------------------*/
(define-method (<nb n1::Integer n2)
   (with-access::Integer n1 (value)
      (if (Integer? n2)
	  (<fx value (Integer-value n2))
	  (<fl (exact->inexact value) (Real-value n2)))))

;*---------------------------------------------------------------------*/
;*    <nb ::Real ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (<nb n1::Real n2)
   (with-access::Real n1 (value)
      (if (Real? n2)
	  (<fl value (Real-value n2))
	  (<fl value (exact->inexact (Integer-value n2))))))

;*---------------------------------------------------------------------*/
;*    ~fl ...                                                          */
;*---------------------------------------------------------------------*/
(define (~fl::bool d1::double d2::double)
   (<fl (absfl (-fl d1 d2)) 0.000000001))

;*---------------------------------------------------------------------*/
;*    =nb ::Integer ...                                                */
;*---------------------------------------------------------------------*/
(define-method (=nb n1::Integer n2)
   (with-access::Integer n1 (value)
      (if (Integer? n2)
	  (=fx value (Integer-value n2))
	  (~fl (exact->inexact value) (Real-value n2)))))

;*---------------------------------------------------------------------*/
;*    =nb ::Real ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (=nb n1::Real n2)
   (with-access::Real n1 (value)
      (if (Real? n2)
	  (~fl value (Real-value n2))
	  (~fl value (exact->inexact (Integer-value n2))))))

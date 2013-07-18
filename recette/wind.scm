;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/wind.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar  8 19:31:00 1998                          */
;*    Last change :  Thu Jul 18 12:56:11 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Wind test (dynamic-wind and unwind-protect).                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module wind
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-wind)))

;*---------------------------------------------------------------------*/
;*    A global variable                                                */
;*---------------------------------------------------------------------*/
(define *kont* #unspecified)

;*---------------------------------------------------------------------*/
;*    test-unwind ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-unwind)
   (let ((x 3))
      (call/cc (lambda (exit)
		  (unwind-protect
		     (begin
			(set! x (+ 1 x))
			(exit (begin (set! x (+ 1 x)) x))
			(set! x (+ 10 x)))
		     (set! x (+ 1 x)))))
      x))

;*---------------------------------------------------------------------*/
;*    test-wind ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-wind)
   (let ((exg ($get-exitd-top)))
      (test-module "wind" "wind.scm")
      (test "unwind-protect" (test-unwind) 6)
      (when-call/cc (test-wind2))
      (test "dynamic-wind" exg ($get-exitd-top))
      (test "dynamic-wind/raise.0" (wind-raise-0) 1)
      (test "dynamic-wind/raise.1" (wind-raise-1) 1)
      (test "dynamic-wind/raise.2" (wind-raise-2) 1)
      (test "dynamic-wind/raise.3" (wind-raise-3) 1)
      (test "dynamic-wind/raise.4" (wind-raise-4) 1)
      (test "dynamic-wind/raise.5" (wind-raise-5) 1)))

;*---------------------------------------------------------------------*/
;*    wind-raise-0 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-0)
   (let ((x 0))
      (with-handler
	 (lambda (e)
	    x)
	 (unwind-protect
	    (set! x 2)
	    (set! x 1)))
      x))

;*---------------------------------------------------------------------*/
;*    wind-raise-1 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-1)
   (let ((x 0))
      (with-handler
	 (lambda (e)
	    x)
	 (unwind-protect
	    (raise 1)
	    (set! x 1)))
      x))

;*---------------------------------------------------------------------*/
;*    wind-raise-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-2)
   (let ((x 0))
      (with-exception-handler
	 (lambda (e)
	    x)
	 (lambda ()
	    (unwind-protect
	       (raise 1)
	       (set! x 1))))
      x))

;*---------------------------------------------------------------------*/
;*    wind-raise-3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-3)
   (let ((x 0))
      (bind-exit (ext)
	 (with-exception-handler
	    (lambda (e)
	       (ext x))
	    (lambda ()
	       (unwind-protect
		  (raise 1)
		  (set! x 1)))))
      x))

;*---------------------------------------------------------------------*/
;*    wind-raise-4 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-4)
   (let ((x 0))
      (with-handler
	 (lambda (e)
	    x)
	 (begin
	    (unwind-protect
	       (raise 1)
	       (set! x 1))
	    (set! x 2)))
      x))

;*---------------------------------------------------------------------*/
;*    wind-raise-5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (wind-raise-5)
   (let ((x 0))
      (with-handler
	 (lambda (e)
	    x)
	 (begin
	    (unwind-protect
	       (raise 1)
	       (set! x 1))
	 (set! x 2)))
      x))

;*---------------------------------------------------------------------*/
;*    unwind-nesting                                                   */
;*---------------------------------------------------------------------*/
(define (unwind-nesting init)
   (set! unwind-nesting-l init)
   (with-handler
      (lambda (e)
	 #f)
      (unwind-nesting-foo))
   unwind-nesting-l)

(define unwind-nesting-l '())


(define (unwind-nesting-foo)
   (unwind-protect
      (unwind-nesting-bar)
      (set! unwind-nesting-l (cons 'foo unwind-nesting-l))))

(define (unwind-nesting-bar)
   (unwind-protect
      (unwind-nesting-gee)
      (set! unwind-nesting-l (cons 'bar unwind-nesting-l))))

(define (unwind-nesting-gee)
   (unwind-protect
      (unwind-nesting-hux)
      (set! unwind-nesting-l (cons 'gee unwind-nesting-l))))

(define (unwind-nesting-hux)
   (unwind-protect
      (unwind-nesting-mee)
      (set! unwind-nesting-l (cons 'hux unwind-nesting-l))))

(define (unwind-nesting-mee)
   (error 1 2 3))

;*---------------------------------------------------------------------*/
;*    test-wind2 ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-wind2)
   (let ((ex ($get-exitd-top)))
      (test "unwind-protect"
	    (let* ((x 10)
		   (value (unwind-protect
			     (call/cc (lambda (exit)
					 (begin
					    (set! x (+ 1 x))
					    (set! *kont* exit)
					    (set! x (+ 1 x)))))
			     (set! x (+ 1 x)))))
	       (if (not (eq? value 4))
		   (*kont* 4)
		   x))
	    14)
      (test "unwind-protect" ex ($get-exitd-top))
      (test "unwind-protect"
	    (let* ((x 10)
		   (value (unwind-protect
			     (call/cc (lambda (exit)
					 (begin
					    (set! x (+ 1 x))
					    (set! *kont* exit)
					    (set! x (+ 1 x)))))
			     (set! x (+ 1 x)))))
	       (if (not (eq? value 4))
		   (*kont* 4)
		   x))
	    14)
      (test "unwind-protect" ex ($get-exitd-top)))
   (test "dynamic-wind"
	 (let ((path '())
	       (c    #f))
	    (let ((add (lambda (s) (set! path (cons s path)))))
	       (dynamic-wind
		  (lambda () (add 'connect))
		  (lambda ()
		     (add (call/cc
			   (lambda (c0)
			      (set! c c0)
			      'talk1))))
		  (lambda ()
		     (add 'disconnect)))
	       (if (< (length path) 4)
		   (c 'talk2)
		   (reverse path))))
	 (let ((path '())
	       (c    #f))
	    (let ((add (lambda (s) (set! path (cons s path)))))
	       (dynamic-wind
		  (lambda () (add 'connect))
		  (lambda ()
		     (add (call/cc
			   (lambda (c0)
			      (set! c c0)
			      'talk1))))
		  (lambda ()
		     (add 'disconnect)))
	       (if (< (length path) 4)
		   (c 'talk2)
		   (reverse path)))))
   (test "dynamic-wind"
	 (let ((path '())
	       (c    #f))
	    (let ((add (lambda (s) (set! path (cons s path)))))
	       (dynamic-wind
		  (lambda () (add 'connect))
		  (lambda ()
		     (add (call/cc
			   (lambda (c0)
			      (set! c c0)
			      'talk1))))
		  (lambda ()
		     (add 'disconnect)))
	       (if (< (length path) 4)
		   (c 'talk2)
		   (reverse path))))
	 '(connect talk1 disconnect connect talk2 disconnect))
   (test "unwind-nesting" (unwind-nesting '()) '(foo bar gee hux)))


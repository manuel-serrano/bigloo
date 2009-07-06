;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/optim.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 16:04:55 1998                          */
;*    Last change :  Wed Dec 26 16:44:00 2007 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `storage analysis tests'                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module optim
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-optim))
   (option  (bigloo-debug-set! 0)
	    (set! *unsafe-library* #t)
	    (set! *unsafe-arity*   #t)
	    (set! *unsafe-type*    #t)
	    (set! *unsafe-eval*    #t)
	    (set! *unsafe-struct*  #t)
	    (set! *unsafe-range*   #t)
	    (set! *unsafe-version* #t)
	    (set! *optim* 6)))

;*---------------------------------------------------------------------*/
;*    get-while ...                                                    */
;*---------------------------------------------------------------------*/
(define (get-while fn)
   (cond ((eof-object? (peek-char)) '())
	 ((fn (peek-char))
	  (let ((c (read-char)))
	     (cons c (get-while fn))))
	 (else '())))

;*---------------------------------------------------------------------*/
;*    get-while2 ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-while2 fn)
   (cond ((eof-object? (peek-char)) '())
	 ((fn (peek-char))
	  (let ((c (read-char)))
	     (cons (get-while2 fn) c)))
	 (else '())))

;*---------------------------------------------------------------------*/
;*    bad-coerce ...                                                   */
;*---------------------------------------------------------------------*/
(define (bad-coerce)
   (let ((result 'good))
      (unless (let ((c (cons '() 1)))
		 (and (pair? c) (fixnum? (cdr c))))
	 (set! result 'bad))
      result))

;*---------------------------------------------------------------------*/
;*    test-optim ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-optim)
   (test-module "optim" "optim.scm")
   (test "beta reduce"
	 (with-input-from-string "testing aaaa..."
	    (lambda ()
	       (get-while (lambda (char) (not (eq? char #\a))))))
	 '(#\t #\e #\s #\t #\i #\n #\g #\space))
   (test "beta reduce"
	 (with-input-from-string "testing aaaa..."
	    (lambda ()
	       (get-while2 (lambda (char) (not (eq? char #\a))))))
	 '((((((((() . #\space) . #\g) . #\n) . #\i) . #\t) . #\s) . #\e) . #\t))
   (test "static coerce" (bad-coerce) 'good))

   

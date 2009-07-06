;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/error.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 16 15:41:47 1993                          */
;*    Last change :  Tue Dec  5 08:54:22 2006 (serrano)                */
;*                                                                     */
;*    On test le fonctionnement des `error-handler'                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module error
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-error)))

;*---------------------------------------------------------------------*/
;*    gee ...                                                          */
;*---------------------------------------------------------------------*/
(define (gee::obj x)
   (if x
       '()
       (begin (list 'this 'function 'shoult 'not 'be 'inlined) 1)))

;*---------------------------------------------------------------------*/
;*    handler ...                                                      */
;*---------------------------------------------------------------------*/
(define (handler a b c d)
   (a #f))

;*---------------------------------------------------------------------*/
;*    try-test ...                                                     */
;*---------------------------------------------------------------------*/
(define (try-test)
   (let ((handler1
	  (lambda (w x y z) (w #t)))
	 (handler2
	  (lambda (w x y z) (w #f))))
      (try
       (begin
	  (try
	   (error 'error 1 1)
	   handler2)
	  (error 'error 2 2))
       handler1)))

;*---------------------------------------------------------------------*/
;*    try-test-2 ...                                                   */
;*---------------------------------------------------------------------*/
(define (try-test-2)
   (try (try (car 1)
	     (lambda (escape obj proc msg)
		(error obj proc msg)))
	(lambda (escape obj proc msg)
	   (escape #t))))

;*---------------------------------------------------------------------*/
;*    side-effect ...                                                  */
;*---------------------------------------------------------------------*/
(define (side-effect x)
   (let ((y x))
      (try (begin
	      (set! y 7)
	      (error 1 2 3)
	      4)
	   (lambda (a b c d)
	      (a 3)))))

;*---------------------------------------------------------------------*/
;*    error-port ...                                                   */
;*---------------------------------------------------------------------*/
(define (error-port)
   (let ((p (open-output-string)))
      (with-error-to-port p
			  (lambda ()
			     (display 5 (current-error-port))))
      (close-output-port p)))

;*---------------------------------------------------------------------*/
;*    test-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-error)
   (test-module "error" "error.scm")
   (test "type error (car)" (try (car (gee #f)) handler) #f)
   (test "type error (integer)" (try (=fx (gee #f) 7) handler) #f)
   (test "type error (string)" (try (string-ref (gee #f) (gee #f)) handler) #f)
   (test "type error (integer?)" (try (integer? (string-length (gee #f))) handler) #f)
   (test "type error (pair?)" (try (pair? (string-length (gee #f))) handler) #f)
   (test "try" (try-test) #t)
   (test "try-2" (try-test-2) #t)
   (test "side effect" (side-effect 3) 3)
   (test "error port" (error-port) "5")
   (test "with-exception-handler.1"
	 (with-exception-handler
	    (lambda (e) e)
	    (lambda () 1))
	 1)
   (test "with-exception-handler.2"
	 (bind-exit (exit)
	    (with-exception-handler
	       (lambda (e) (exit e))
	       (lambda () (raise 1))))
	 1)
   (test "with-exception-handler.3"
	 (bind-exit (exit)
	    (with-exception-handler
	       (lambda (e)
		  (with-exception-handler
		     (lambda (e)
			(exit e))
		     (lambda ()
			(raise (+ 1 e)))))
	       (lambda () (raise 1))))
	 2)
   (test "with-exception-handler.4"
	 (bind-exit (exit)
	    (with-exception-handler
	       (lambda (e)
		  (with-exception-handler
		     (lambda (e)
			(exit e))
		     (lambda ()
			(raise (+ 1 e)))))
	       (lambda () (exit 1))))
	 1)
   (test "with-exception-handler.5"
	 (begin
	    (bind-exit (exit)
	       (with-exception-handler
		  (lambda (e)
		     (with-exception-handler
			(lambda (e)
			   (exit e))
			(lambda ()
			   (raise (+ 1 e)))))
		  (lambda () (exit 1))))
	    (bind-exit (exit)
	       (with-exception-handler
		  (lambda (e)
		     (exit 1))
		  (lambda ()
		     (raise 1)))))
	 1)
   (test "with-exception-handler.6"
	 (bind-exit (exit)
	    (with-exception-handler
	       (lambda (e)
		  (exit (+ 1 e)))
	       (lambda ()
		  (raise (bind-exit (exit2)
			    (with-exception-handler
			       (lambda (e) 23)
			       (lambda () (exit2 1))))))))
	 2)
   (test "with-exception-handler.1-eval"
	 (eval '(with-exception-handler
		   (lambda (e) e)
		   (lambda () 1)))
	 1)
   (test "with-exception-handler.2-eval"
	 (eval '(bind-exit (exit)
		   (with-exception-handler
		      (lambda (e) (exit e))
		      (lambda () (raise 1)))))
	 1)
   (test "with-exception-handler.3-eval"
	 (eval '(bind-exit (exit)
		   (with-exception-handler
		      (lambda (e)
			 (with-exception-handler
			    (lambda (e)
			       (exit e))
			    (lambda ()
			       (raise (+ 1 e)))))
		      (lambda () (raise 1)))))
	 2)
   (test "with-exception-handler.4-eval"
	 (eval '(bind-exit (exit)
		   (with-exception-handler
		      (lambda (e)
			 (with-exception-handler
			    (lambda (e)
			       (exit e))
			    (lambda ()
			       (raise (+ 1 e)))))
		      (lambda () (exit 1)))))
	 1)
   (test "with-exception-handler.5-eval"
	 (eval '(begin
		   (bind-exit (exit)
		      (with-exception-handler
			 (lambda (e)
			    (with-exception-handler
			       (lambda (e)
				  (exit e))
			       (lambda ()
				  (raise (+ 1 e)))))
			 (lambda () (exit 1))))
		   (bind-exit (exit)
		      (with-exception-handler
			 (lambda (e)
			    (exit 1))
			 (lambda ()
			    (raise 1))))))
	 1)
   (test "with-exception-handler.6-eval"
	 (eval '(bind-exit (exit)
		   (with-exception-handler
		      (lambda (e)
			 (exit (+ 1 e)))
		      (lambda ()
			 (raise (bind-exit (exit2)
				   (with-exception-handler
				      (lambda (e) 23)
				      (lambda () (exit2 1)))))))))
	 2)
   (test "with-handler.1"
	 (with-handler
	    (lambda (e) e)
	    1)
	 1)
   (test "with-handler.2"
	 (bind-exit (exit)
	    (with-handler
	       (lambda (e) (exit e))
	       (raise 1)))
	 1)
   (test "with-handler.3"
	 (bind-exit (exit)
	    (with-handler
	       (lambda (e)
		  (with-handler
		     (lambda (e)
			(exit e))
		     (raise (+ 1 e))))
	       (raise 1)))
	 2)
   (test "with-handler.4"
	 (bind-exit (exit)
	    (with-handler
	       (lambda (e)
		  (with-handler (lambda (e) (exit e))
				(raise (+ 1 e))))
	       (exit 1)))
	 1)
   (test "with-handler.5"
	 (begin
	    (bind-exit (exit)
	       (with-handler
		  (lambda (e)
		     (with-handler (lambda (e) (exit e))
				   (raise (+ 1 e))))
		  (exit 1)))
	    (bind-exit (exit)
	       (with-handler
		  (lambda (e) (exit 1))
		  (raise 1))))
	 1)
   (test "with-handler.6"
	 (bind-exit (exit)
	    (with-handler
	       (lambda (e) (exit (+ 1 e)))
	       (raise (bind-exit (exit2)
			 (with-handler (lambda (e) 23) (exit2 1))))))
	 2)
   (test "with-handler.7"
	 (with-handler
	    (lambda (e)
	       (+ 1 e))
	    (with-handler
	       (lambda (e)
		  (raise (+ 1 e)))
	       (raise 1)))
	 3)
   (test "with-handler.8"
	 (with-handler
	    (lambda (e)
	       (+ 1 e))
	    (with-handler
	       (lambda (e)
		  (with-handler
		     (lambda (e)
			(raise (+ 1 e)))
		     (raise (+ 1 e))))
	       (raise 1)))
	 4)
   (test "with-handler.1-eval"
	 (eval '(with-handler
		   (lambda (e) e)
		   1))
	 1)
   (test "with-handler.2-eval"
	 (eval '(bind-exit (exit)
		   (with-handler
		      (lambda (e) (exit e))
		      (raise 1))))
	 1)
   (test "with-handler.3-eval"
	 (eval '(bind-exit (exit)
		   (with-handler
		      (lambda (e)
			 (with-handler
			    (lambda (e)
			       (exit e))
			    (raise (+ 1 e))))
		      (raise 1))))
	 2)
   (test "with-handler.4-eval"
	 (eval '(bind-exit (exit)
		   (with-handler
		      (lambda (e)
			 (with-handler (lambda (e) (exit e))
				       (raise (+ 1 e))))
		      (exit 1))))
	 1)
   (test "with-handler.5-eval"
	 (eval '(begin
		   (bind-exit (exit)
		      (with-handler
			 (lambda (e)
			    (with-handler (lambda (e) (exit e))
					  (raise (+ 1 e))))
			 (exit 1)))
		   (bind-exit (exit)
		      (with-handler
			 (lambda (e) (exit 1))
			 (raise 1)))))
	 1)
   (test "with-handler.6-eval"
	 (eval '(bind-exit (exit)
		   (with-handler
		      (lambda (e) (exit (+ 1 e)))
		      (raise (bind-exit (exit2)
				(with-handler
				   (lambda (e) 23) (exit2 1)))))))
	 2))
   

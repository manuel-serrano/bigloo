;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/bigloo/recette/read.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 22 16:17:03 1992                          */
;*    Last change :  Mon Oct  8 08:31:43 2018 (serrano)                */
;*                                                                     */
;*    Un essai de reader                                               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module read
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-read)))
 
;*---------------------------------------------------------------------*/
;*    read-test ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-test port)
   (let ((npair   0)
	 (nint    0)
	 (nstring 0)
	 (nvector 0)
	 (nchar   0)
	 (nsymbol 0))
      (let loop ((sexp   (read port))
		 (n      0))
	 (if (eof-object? sexp)
	     (list nsymbol nchar nvector nstring nint npair n)
	     (begin
		(cond
		   ((pair? sexp)
		    (set! npair (+ 1 npair)))
		   ((integer? sexp)
		    (set! nint (+ 1 nint)))
		   ((string? sexp)
		    (set! nstring (+ 1 nstring)))
		   ((vector? sexp)
		    (set! nvector (+ 1 nvector)))
		   ((char? sexp)
		    (set! nchar (+ 1 nchar)))
		   (else
		    (set! nsymbol (+ 1 nsymbol))))
		(loop (read port)
		      (+ 1 n)))))))

;*---------------------------------------------------------------------*/
;*    test-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-read)
   (test-module "read" "read.scm")
   (test "test" (call-with-input-file "misc/input.txt" read-test)
	 '(16 2 2 5 2 7 34))
   (test "read" (cdr '((foo . bar) . hux)) 'hux)
   (test "circular"
	 (caddr (read (open-input-string "((1 . #0=(-6 7)) 5 . #0#)"))) -6)
   (test "read-C-string"
	 (call-with-input-string "\"foo\n\tbar\"" read)
	 "foo\n\tbar")
   (test "read-string" (call-with-input-string "#\\\"str\"" read) #\"))
	    
		   


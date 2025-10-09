;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/rgc_eval.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 28 08:54:00 1994                          */
;*    Last change :  Thu Oct  9 15:09:36 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    On test rgc sous eval                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module rgc-eval
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-rgc-eval)))
	    
;*---------------------------------------------------------------------*/
;*    Le test                                                          */
;*---------------------------------------------------------------------*/
(define (rgc-eval-test)
   (eval '(begin (define *char* 0)
		 (define *line* 0)
		 (define *word* 0)
		 (define g
		    (regular-grammar ()
		       ((+ #\Newline)
			(set! *char* (+fx *char* (the-length)))
			(set! *line* (+fx *line* (the-length)))
			'keep)
		       ((+ (in #\space #\tab))
			(set! *char* (+fx *char* (the-length)))
			'keep)
		       ((+ (out #\newline #\space #\tab))
			(set! *char* (+fx *char* (the-length)))
			(set! *word* (+fx 1 *word*))
			'keep)
		       (else
			(the-failure))))))
   #t)
 
;*---------------------------------------------------------------------*/
;*    test-rgc-eval ...                                                */
;*---------------------------------------------------------------------*/
(define (test-rgc-eval)
   (test-module "rgc-eval" "rgc-eval.scm")
   (test "rgc-eval.1" (rgc-eval-test) #t)
   (test "rgc-eval.2"
	 (eval '(let ((p (open-input-string #"toto \ntiti \ntutu")))
		   (let loop ((exp (read/rp g p)))
		      (if (eof-object? exp)
			  (list *char* *line* *word*)
			  (loop (read/rp g p))))))
	 '(16 2 3)))
 
 

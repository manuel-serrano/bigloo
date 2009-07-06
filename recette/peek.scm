;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/peek.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Mike Levy                                         */
;*    Creation    :  Sat Apr 17 09:05:31 1999                          */
;*    Last change :  Thu Sep 23 19:05:56 2004 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Testing peek-char                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module peek
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-peek)))

;; test the peek-char bug by reading a writing then reading a long file
;; 
(define idx 0)
(define step 1)

;; create a file of n 'a'
(define (create-file temporary-file n)
  (with-output-to-file temporary-file
		      (lambda ()
			(let loop ((n n))
			  (if (> n 0)
			      (begin
				(display #\a)
				(loop (- n 1))))))))

;; an extra peek-char for each read-char
(define (read-char-bis port)
  (set! idx (+ 1 idx))
  (if (= (remainder idx step) 0)
      (peek-char port))
  (read-char port))

;; read the file in parallel by 2 ports and report if it differs with itself
(define (read-twice-then-delete file)
   (let ((p1 (open-input-file file))
	 (p2 (open-input-file file)))
      (unwind-protect
	 (let loop ((n 0)
		    (c1 (read-char p1))
		    (c2 (read-char-bis p2)))
	    (if (and (eq? c1 c2)
		     (not (eof-object? c1))
		     (not (eof-object? c2)))
		(loop (+ n 1)
		      (read-char p1)
		      (read-char-bis p2))
		(if (not (eq? c1 c2))
		    #f
		    n)))
	 (begin
	    (close-input-port p1)
	    (close-input-port p2)))))

(define (do-test-peek nb step)
   (let ((tmp-file "tmp-peek"))
      (create-file tmp-file nb)
      (unwind-protect
	 (read-twice-then-delete tmp-file)
	 (delete-file tmp-file))))

(define (do-test-filepos n)
   (let ((p (open-input-string "foo bar gee")))
      (let loop ((n n))
	 (when (> n 0)
	    (read-char p)
	    (peek-char p)
	    (loop (- n 1))))
      (let ((r (input-port-position p)))
	 (close-input-port p)
	 r)))
		 
(define (test-peek)
   (test-module "peek" "peek.scm")
   (let ((num 66000))
      (test "peek" (do-test-peek num 1) num)
      (test "filepos" (do-test-filepos 5) 5)))

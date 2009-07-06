;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/fringe.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 21 16:01:48 1992                          */
;*    Last change :  Mon Feb 28 15:57:17 2005 (serrano)                */
;*                                                                     */
;*    On test same fringe (de luis mateu)                              */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module fringe
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-fringe)))

;*---------------------------------------------------------------------*/
;*    test-fringe ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-fringe)
   (when-call/cc
    (begin
       (test-module "fringe" "fringe.scm")
       (test "call/cc(fringe)" (test_fringe same-fringe 10 1) #t))))

(define (make-tree n)
  (cond
     ((= n 0) 1)
     (else
        (let ((mid (/fx n 2)))
           (cons (make-tree mid) (make-tree (- (- n 1) mid)))))))

(define (test_fringe comparator size count)
  (let ((ta (make-tree size))
        (tb (make-tree (+ size 1))))
     (let loop ((idx 1))
       (if (<= idx count)
         (begin
           (if (not (comparator ta tb)) #f)
           (loop (+ idx 1)))))
     #t))

(define *result* 'void) ;; emulates multiple value returns

(define (same-fringe ta tb)
   (labels ((walk (tree k)
		  (cond
		     ((not (pair? tree))
		      (set! *result* tree)
		      (call/cc k) )
		     (else
		      (walk (cdr tree) (walk (car tree) k)) ) ) )
	    (compare-fringe (ka kb)
			    (let* ((new-ka (call/cc ka))
				   (leaf-ta *result*)
				   (new-kb (call/cc kb))
				   (leaf-tb *result*))
			       (cond
				  ((not (eq? leaf-ta leaf-tb))  #f )
				  ((null? leaf-ta)              #t)
				  (else
				   (compare-fringe new-ka new-kb) ) ) ) ) )

  (compare-fringe
    (call/cc ;; one tree walker
      (lambda (k)
        (let ((last-k (walk ta k)))
          (set! *result* '())
	  (last-k '()) ) ) )
    (call/cc ;; the other tree walker
      (lambda (k)
        (let ((last-k (walk tb k)))
          (set! *result* '())
          (last-k '()) ) ) ) ) ) )



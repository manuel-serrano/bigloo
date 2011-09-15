;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/hash.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 26 11:50:50 1994                          */
;*    Last change :  Thu Sep 15 16:06:33 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    On test les tables de hash                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module hash
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-hash)))

;*---------------------------------------------------------------------*/
;*    test1-new ...                                                    */
;*---------------------------------------------------------------------*/
(define (test1-new)
   (let ((table (make-hashtable 2)))
      (let loop ((i 1024))
	 (if (=fx i 0)
	     (and (=fx (hashtable-get table "object834") 834)
		  (=fx (hashtable-get table "object835") 835)
		  (=fx (hashtable-get table "object836") 836)
		  (=fx (hashtable-get table "object837") 837)
		  (not (=fx (hashtable-get table "object134") 834))
		  (not (=fx (hashtable-get table "object135") 835))
		  (not (=fx (hashtable-get table "object136") 836))
		  (not (=fx (hashtable-get table "object137") 837)))
	     (begin
		(hashtable-put! table
				(string-append "object" (number->string i))
				i)
		(loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    test2-new ...                                                    */
;*---------------------------------------------------------------------*/
(define (test2-new)
   (let ((table (make-hashtable)))
      (let loop ((i 10024))
	 (if (=fx i 0)
	     (and (= (hashtable-get table "object5834") 5834)
		  (= (hashtable-get table "object5835") 5835)
		  (= (hashtable-get table "object5836") 5836)
		  (= (hashtable-get table "object5837") 5837)
		  (not (= (hashtable-get table "object9134") 5834))
		  (not (= (hashtable-get table "object9135") 5835))
		  (not (= (hashtable-get table "object9136") 5836))
		  (not (= (hashtable-get table "object9137") 5837)))
	     (begin
		(hashtable-put! table
				(string-append "object"
					       (number->string i))
			       i)
		(loop (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    test3-new ...                                                    */
;*---------------------------------------------------------------------*/
(define (test3-new)
   (let ((table (make-hashtable)))
      (hashtable-put! table 123 123)
      (let ((v (hashtable-get table 123)))
	 (if (and (fixnum? v) (=fx v 123))
	     (begin
		(hashtable-remove! table 123)
		(if (not (hashtable-get table 123))
		    #t
		    #f))
	     #f))))

;*---------------------------------------------------------------------*/
;*    test4-new ...                                                    */
;*---------------------------------------------------------------------*/
(define (test4-new)
   (let ((table (make-hashtable))
	 (cell (cons 1 2)))
      (hashtable-put! table "toto" 1)
      (hashtable-put! table 'toto 2)
      (hashtable-put! table 123 3)
      (hashtable-put! table cell 5)
      (hashtable-put! table cell 4)
      (and (=fx (hashtable-get table "toto") 1)
	   (=fx (hashtable-get table 'toto) 2)
	   (=fx (hashtable-get table 123) 3)
	   (=fx (hashtable-get table cell) 4)
	   (begin
	      (hashtable-remove! table "toto")
	      (hashtable-remove! table 'toto)
	      (hashtable-remove! table 123)
	      (hashtable-remove! table cell)
	      (and (not (hashtable-get table "toto"))
		   (not (hashtable-get table 'toto))
		   (not (hashtable-get table 123))
		   (not (hashtable-get table cell)))))))

;*---------------------------------------------------------------------*/
;*    test5 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test5)
   (let* ((d (current-date))
	  (s1::obj (date->seconds d))
	  (s2::obj (date->seconds d))
	  (l1::obj (string->llong "124"))
	  (l2::obj (string->llong "124"))
	  (t (create-hashtable :eqtest equal?)))
      (hashtable-put! t s1 'foo)
      (hashtable-put! t l1 'bar)
      (list (not (eq? s1 s2))
	    (eq? (hashtable-get t s2) 'foo)
	    (not (eq? l1 l2))
	    (eq? (hashtable-get t l2) 'bar))))

;*---------------------------------------------------------------------*/
;*    test-update ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-update)
   (let ((table (make-hashtable)))
      (hashtable-update! table 'foo (lambda (x) (+ 1 x)) 1)
      (hashtable-put! table 'bar 1)
      (let ((r1 (hashtable-get table 'foo)))
	 (hashtable-update! table 'foo (lambda (x) (+ 1 x)) 1)
	 (hashtable-update! table 'bar (lambda (x) (+ 1 x)) 1)
	 (let ((r2 (hashtable-get table 'foo))
	       (r3 (hashtable-get table 'bar)))
	    (and (=fx r1 1) (=fx r2 2) (=fx r3 2))))))

;*---------------------------------------------------------------------*/
;*    test-override ...                                                */
;*---------------------------------------------------------------------*/
(define (test-override)
   (let ((ht (make-hashtable)))
      (and (= 1 (hashtable-put! ht 'a 1))
	   (= 1 (hashtable-put! ht 'a 2))
	   (= 2 (hashtable-put! ht 'a 3)))))

;*---------------------------------------------------------------------*/
;*    test-hash ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-hash)
   (test-module "hash" "hash.scm")
   (test "hash" (test1-new) #t)
   (test "hash" (test2-new) #t)
   (test "remove" (test3-new) #t)
   (test "heterogeneous" (test4-new) #t)
   (test "update" (test-update) #t)
   (test "elong/llong" (test5) '(#t #t #t #t))
   (test "override" (test-override) #t))


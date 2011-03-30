;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/external_c.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 19 15:38:42 1992                          */
;*    Last change :  Wed Mar 30 18:28:00 2011 (serrano)                */
;*                                                                     */
;*    Le fichier principale de test des foreign                        */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module external
   (include "test.sch") 
   (import  (bis big-file "big_file.scm")
	    (main "main.scm"))
   (export  (test-external))
   (extern  (macro sprn::int (::string ::string . ::long) "sprintf"))
   (foreign (string hux (string) "hux")
            (int var "var")
	    (include "c-file.h")
	    (type el (struct (int key "key") (el* next "next")) "struct el")
	    (type tab (pointer int) "int *")
	    (int sum-el (el*) "sum_el")
	    (el*  define-el (int) "define_el")
	    (double sum-tab (tab int) "sum_tab")
	    (el*  make-dummy-el () "make_dummy_el")
	    
	    (type fun (function int (int)) "int (*$)(int)")
	    (type fun2 fun "int (*$)(int)")
	    (fun f1 "f1")
	    (fun f2 "f1")
	    
	    (type p0 (pointer int) "int *")
	    (type p1 (array int) "int $[22]")
	    
	    (type s0 (struct (int key "key") (el* next "next")) "struct el")
	    (type s1* s0* "EL *")
	    
	    (type pt (struct (int x "x") (int y "y")) "Point")
	    (type pt-foo* pt* "Point *")
	    (type pt-foo-2* pt-foo* "Point *")
	    (type pt-bar pt* "Point *")
	    (type pt-bar* (pointer pt-bar) "Point **")
	    
	    (type ->int (function int ()) "int (*$)()")
	    (type ->int-array-128 (array ->int) "int (*$[ 128 ])()")
	    (type ->int* (pointer ->int) "int (*(*$))()")

	    (type s (struct (int x "x") (int y "y")) "struct s")
	    (type a (array s) "struct s[ 10 ]")))

;*---------------------------------------------------------------------*/
;*    foo ... ...                                                      */
;*---------------------------------------------------------------------*/
(define (foo x)
   (extern-bar x))
 
;*---------------------------------------------------------------------*/
;*    boo                                                              */
;*---------------------------------------------------------------------*/
(define (boo s)
   (hux s))
 
;*---------------------------------------------------------------------*/
;*    booboo                                                           */
;*---------------------------------------------------------------------*/
(define (booboo f)
   0 1 2 3 4 5 6 7 8 9 0
   0 1 2 3 4 5 6 7 8 9 0
   0 1 2 3 4 5 6 7 8 9 0
   0 1 2 3 4 5 6 7 8 9 0
   0 1 2 3 4 5 6 7 8 9 0
   0 1 2 3 4 5 6 7 8 9 0
   (f  "toto n'est pas content"))
 
;*---------------------------------------------------------------------*/
;*    test-struct ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-struct n)
   (let ((head (make-el*)))
      (el*-key-set! head 0)
      (el*-next-set! head (make-null-el*))
      (let loop ((n  n)
		 (c  head))
	 (if (= n 0)
	     (sum-el c)
	     (let ((new (make-el*)))
		(el*-key-set!  new n)
		(el*-next-set! new c)
		(loop (- n 1) new))))))

;*---------------------------------------------------------------------*/
;*    test-array ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-array n)
   (let ((tab (make-tab (+ (* 2 n) 1))))
      (let loop ((i (- n)))
	 (if (> i n)
	     (sum-tab tab (+ (* 2 n) 1))
	     (begin
		(tab-set! tab (+ i n) i)
		(loop (+ i 1)))))))

;*---------------------------------------------------------------------*/
;*    lux ...                                                          */
;*---------------------------------------------------------------------*/
(define (lux pred? el)
   (pred? el))

;*---------------------------------------------------------------------*/
;*    test-function ...                                                */
;*---------------------------------------------------------------------*/
(define (test-function)
   (define (a-test f)
      (and (fun? f)
	   (=fx (fun-call f 5) 9)
	   (fun2? f)
	   (=fx (fun2-call f 5) 9)))
   (and (a-test f1) (a-test f2)))

;*---------------------------------------------------------------------*/
;*    test-pointer ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-pointer)
   (define (a-test maker)
      (let ((p (maker)))
	 (if (and (p0? p) (p1? p))
	     (begin
		(p0-set! p 0 -99)
		(p1-set! p 1 99)
		(=fx (+fx (p0-ref p 0) (p1-ref p 1)) 0))
	     #f)))
   (and (a-test (lambda () (make-p0 3))) (a-test make-p1)))

;*---------------------------------------------------------------------*/
;*    test-struct* ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-struct*)
   (let ((s0 (make-s0*))
	 (s1 (make-s1*)))
      (if (and (s0*? s0) (s1*? s0) (s0*? s1) (s1*? s1))
	  (begin
	     (s0*-key-set! s0 4)
	     (s0*-key-set! s1 -4)
	     (if (=fx 0 (+fx (s0*-key s0) (s0*-key s1)))
		 (begin
		    (s1*-key-set! s0 4)
		    (s1*-key-set! s1 -4)
		    (=fx 0 (+fx (s0*-key s0) (s0*-key s1))))
		 #f))
	  #f)))

;*---------------------------------------------------------------------*/
;*    test-alias1 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-alias1)
   (=fx (pt*-x (pt-foo* 4 5)) (pt-foo*-y (pt* 5 4))))

;*---------------------------------------------------------------------*/
;*    test-alias2 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-alias2)
   (=fx (pt*-x (pt-foo-2* 4 5)) (pt-foo-2*-y (pt* 5 4))))

;*---------------------------------------------------------------------*/
;*    test-alias3 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-alias3)
   (=fx (pt*-x (pt-bar 4 5)) (pt-bar-y (pt* 5 4))))

;*---------------------------------------------------------------------*/
;*    test-struct-array ...                                            */
;*---------------------------------------------------------------------*/
(define (test-struct-array)
   (let* ((a  (make-a))
	  (s0 (a-ref a 0))
	  (ref a-ref)
	  (set a-set!))
      (s*-x-set! s0 4)
      (s*-y-set! s0 5)
      (a-set! a 1 s0)
      (+fx (+fx (s*-x (a-ref a 0)) (s*-y (a-ref a 0)))
	   (+fx (s*-x (ref a 1)) (s*-y (ref a 1))))))

;*---------------------------------------------------------------------*/
;*    pragma-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define (pragma-set!::double)
   (let ((num::double 0.0))
      (pragma "$1 = 10.0;" num)
      num))

;*---------------------------------------------------------------------*/
;*    test-external ...                                                */
;*---------------------------------------------------------------------*/
(define (test-external)
   (test-module "external" "external-c.scm")
   (test "foreign"      var 9)
   (test "foreign"      (begin (set! var (+ 1 var)) var) 10)
   (test "foreign"      (foo 4) 5)
   (test "foreign"      (boo "toto n'est pas content") "TOTO EST CONTENT")
   (test "closure"      (booboo hux) "TOTO EST CONTENT")
   (test "foreign"      (bis 5) 6)
   (test "struct"       (test-struct 5) (+ 0 1 2 3 4 5))
   (test "struct-ref"   (el*-key (define-el 5)) 5)
   (test "array-ref"    (tab-ref (let ((tab (make-tab 5)))
				    (tab-set! tab 0 5)
				    tab)
				 0) 5)
   (test "array"        (inexact->exact (test-array 10)) 0)
   (test "eq?"          (=el*? (make-dummy-el) (make-dummy-el)) #t)
   (test "foreign-eq?"  (let ((o (make-dummy-el)))
			   (foreign-eq? o o))
	 #t)
   (test "foreign-eq?"  (foreign-eq? (make-dummy-el) (make-tab 5)) #f)
   (test "eqv?"         (eqv? (make-dummy-el) (make-dummy-el)) #t)
   (test "equal?"       (equal? (make-dummy-el) (make-dummy-el)) #t)
   (test "checker"      (el*? (make-el*)) #t)
   (test "checker"      (let ((x::obj 7)) (el*? x)) #f)
   (test "checker"      (lux el*? (make-el*)) #t)
   (test "checker"      (lux el*? 7) #f)
   (test "function"     (test-function) #t)
   (test "pointer"      (test-pointer) #t)
   (test "struct*"      (test-struct*) #t)
   (test "alias"        (test-alias1) #t)
   (test "alias"        (test-alias2) #t)
   (test "alias"        (test-alias3) #t)
   (test "struct array" (test-struct-array) 18)
   (test "n-ary"        (let ((m 10)
			      (s (make-string 11)))
			   (sprn s #"%d + 1 = %d" m (+fx 1 m))
			   s)
	 "10 + 1 = 11")
   (test "pragma" (pragma-set!) 10.0))




		    
		     
	       

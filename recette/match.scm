;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/match.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 10 16:37:46 1992                          */
;*    Last change :  Mon Jul 31 16:51:39 2006 (serrano)                */
;*                                                                     */
;*    Un essai de match-case                                           */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module match
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-match)
            (uncompilable x)))

;*---------------------------------------------------------------------*/
;*    match-test ...                                                   */
;*---------------------------------------------------------------------*/
(define (match-test)
   (let ((l '(let ((x 6))
		(+ x 5))))
      (match-case l
	 ((let ?bindings . ?body)
	  'ok)
	 (else
	  (print 'error)))))
 
;*---------------------------------------------------------------------*/
;*    match-test-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (match-test-2 x)
   (match-case x
      ;; 1- on definit une lambda typee
      ((?- ((?type ?name) . ?args) . ?body)
       'do-define-lambda)
      ;; 1- on definit une lambda non typee
      ((or (?- (?name . ?args) . ?body)
	   (?- ?name (lambda ?args . ?body)))
       'do-define-lambda)
      ;; 2- on definit une valeur non typee
      ((?- ?name . (?value . ()))
       'do-define-value)
      ;; 2b- on definit une valeur typee
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    match-test-3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (match-test-3 x)
   (match-case x
      ((foo bar)
       x))
   #t)

;*---------------------------------------------------------------------*/
;*    match-test-4 ...                                                 */
;*---------------------------------------------------------------------*/
(define (match-test-4 x)
   (match-case x
      (((and ?let-part (let ?- ?body)) . ?args)
       'let)
      (((and ?x (labels ?- ?body)) . ?args)
       'app)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    match-test-5 ...                                                 */
;*---------------------------------------------------------------------*/
(define (match-test-5 x)
   (match-case x
      ((atom ?-)
       'atom)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    Une structure pour tester atom                                   */
;*---------------------------------------------------------------------*/
(define-struct s x) 

;*---------------------------------------------------------------------*/
;*    Des tests sur le filtrage des structures                         */
;*---------------------------------------------------------------------*/
(define-struct int-point x y)
(define-struct real-point x y)

(define (is-in-circle? r p)
   (match-case p
      (#{int-point ?x ?y}
       (<=fx (+fx (*fx x x) (*fx y y)) (*fx r r)))
      (#{real-point ?x ?y}
       (<= (+fl (*fl x x) (*fl y y)) (* r r)))
      (else
       (error "is-in-circle?" "argument not a point" p))))

(define p-int (make-int-point))

(int-point-x-set! p-int 4)
(int-point-y-set! p-int -4)

(define p-real (make-real-point))

(real-point-x-set! p-real 4.4)
(real-point-y-set! p-real -4.4)

;; fonction non compilable avec bigloo1.6
(define (uncompilable exp)
   exp)
;*    (match-case exp                                                  */
;*       ((toto ??- ti ti . ?-)                                        */
;*        1)))                                                         */

;*---------------------------------------------------------------------*/
;*    match-eq ...                                                     */
;*---------------------------------------------------------------------*/
(define (match-eq l)
   (match-case l
      ((toto "toto") 'toto)
      ((toto toto) 'toto-bis)
      ((toto 4) 4)
      ((toto 3.3) 33)
      (else 'else)))

;*---------------------------------------------------------------------*/
;*    match-non-linear ...                                             */
;*---------------------------------------------------------------------*/
(define (match-non-linear exp)
   (match-case exp
      (((and ?x (? integer?)) (and ?x (? integer?))) 1)
      (((* ?x) (* ?x)) 2)
      (else 3)))

;*---------------------------------------------------------------------*/
;*    match-not ...                                                    */
;*---------------------------------------------------------------------*/
(define (match-not exp)
   (match-case exp
      (((and ?expr (? symbol?)))
       1)
      (((and ?expr (? list?) (not ())))
       2)
      ((not foo)
       3)
      (else
       4)))

;*---------------------------------------------------------------------*/
;*    match-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (match-string x)
   (match-case x
      (("a" b) 1)
      (("a" c) 2)
      (else 3)))

;*---------------------------------------------------------------------*/
;*    test-match ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-match)
   (test-module "match" "match.scm")
   (test "let" (match-test) 'ok)
   (test "expand-define" (match-test-2 '(define (foo x) x)) 'do-define-lambda)
   (test "expand-define" (match-test-2 '(define foo (lambda (x) x)))
	 'do-define-lambda)
   (test "expand-define" (match-test-2 '(define foo 3))
	 'do-define-value)
   (test "expand-define" (match-test-2 '(define foo))
	 'else)
   (test "expand" (match-test-3 8) #t)
   (test "match-test-4" (match-test-4 '((labels ((gee (x) x)) gee) 1)) 'app)
   (test "atom" (match-test-5 '(1 2 3)) 'else)
   (test "atom" (match-test-5 '#(1 2 3)) 'atom)
   (test "atom" (match-test-5 (make-s)) 'atom)
   (test "structure" (is-in-circle? 10 p-int) #t)
   (test "structure" (is-in-circle? 10 p-real) #t)
   (test "structure" (begin (match-lambda (#{int-point} 'toto)) #t) #t)
   (test "??- vs trigraph" (pair? (expand '(match-case x ((??- . ?x) 1)))) #t)
   (test "match-eq.1" (match-eq (list 'toto "toto")) 'toto)
   (test "match-eq.2" (match-eq (list 'toto "tutu")) 'else)
   (test "match-eq.3" (match-eq (list 'toto 'toto)) 'toto-bis)
   (test "match-eq.4" (match-eq (list 'toto 'tata)) 'else)
   (test "match-eq.5" (match-eq (list 'toto 4)) 4)
   (test "match-eq.6" (match-eq (list 'toto 3)) 'else)
   (test "match-eq.7" (match-eq (list 'toto 3.3)) 33)
   (test "match-eq.8" (match-eq (list 'toto 3.2)) 'else)
   (test "match-non-linear.1" (match-non-linear '((* a) (* b))) 3)
   (test "match-non-linear.2" (match-non-linear '((* a) (* a))) 2)
   (test "match-non-linear.3" (match-non-linear '(1 2)) 3)
   (test "match-non-linear.4" (match-non-linear '(1 1)) 1)
   (test "match-not" (match-not '(foo)) 1)
   (test "match-not" (match-not '((foo))) 2)
   (test "match-not" (match-not '(())) 3)
   (test "match-not" (match-not 'foo) 4)
   (test "match-string" (match-string '("a" b)) 1)
   (test "match-string" (match-string '("a" c)) 2)
   (test "match-string" (match-string '("a" d)) 3)
   (test "match-string" (match-string '("a")) 3)
   (test "match-string" (match-string '("a" c c)) 3)
   (test-match-isa))
 
;*---------------------------------------------------------------------*/
;*    Class pattern matching tests (isa)                               */
;*---------------------------------------------------------------------*/
(define-class mpoint
   (x (default 0))
   (y (default 0)))

(define-class mpoint3d::mpoint
   (z (default 0)))

(define-class msegment
   (start (default #unspecified))
   (end (default #unspecified)))

(define-class mrect
   (width (default 0))
   (height (default 0)))

(define mp1 (instantiate::mpoint (x 3) (y 4)))
(define mp3 (instantiate::mpoint3d (x 1) (y 2) (z 3)))

(define (isa-basic p)
   (match-case p
      ((isa mpoint (x ?x) (y ?y)) (list x y))
      (else 'fail)))

(define (isa-partial p)
   (match-case p
      ((isa mpoint (x ?x)) x)
      (else 'fail)))

(define (isa-type-only p)
   (match-case p
      ((isa mpoint) 'yes)
      (else 'no)))

(define (isa-literal p)
   (match-case p
      ((isa mpoint (x 3) (y ?y)) y)
      (else 'fail)))

(define (isa-subclass p)
   (match-case p
      ((isa mpoint (x ?x) (y ?y)) (list x y))
      (else 'fail)))

(define (isa-or p)
   (match-case p
      ((or (isa mpoint3d (z ?v)) (isa mpoint (x ?v))) v)
      (else 'fail)))

(define (isa-and p)
   (match-case p
      ((and ?whole (isa mpoint (x ?x))) (list whole x))
      (else 'fail)))

(define (isa-nested seg)
   (match-case seg
      ((isa msegment (start (isa mpoint (x ?x1) (y ?y1)))
                     (end (isa mpoint (x ?x2) (y ?y2))))
       (list x1 y1 x2 y2))
      (else 'fail)))

(define (isa-repeated r)
   (match-case r
      ((isa mrect (width ?s) (height ?s)) s)
      (else 'fail)))

(define (isa-not p)
   (match-case p
      ((not (isa mpoint)) 'not-point)
      (else 'is-point)))

(define (test-match-isa)
   ;; Segment variable tests
   (test "segment.1" (match-case '(a b c)
                        ((??x) x)
                        (else 'fail))
      '(a b c))
   (test "segment.2" (match-case '(a b c d)
                        ((a ??x d) x)
                        (else 'fail))
      '(b c))
   (test "segment.3" (match-case '(a b c b c)
                        ((a ??x ??x) x)
                        (else 'fail))
      '(b c))
   (test "segment.4" (match-case '(a b c d)
                        ((a ??x c d) x)
                        (else 'fail))
      '(b))
   (test "segment.5" (match-case '(a b)
                        ((a ??x b ??x) x)
                        (else 'fail))
      '())
   ;; Class pattern tests
   (test "isa-basic" (isa-basic mp1) '(3 4))
   (test "isa-partial" (isa-partial mp1) 3)
   (test "isa-type-only" (isa-type-only mp1) 'yes)
   (test "isa-type-only.2" (isa-type-only 42) 'no)
   (test "isa-literal.1" (isa-literal mp1) 4)
   (test "isa-literal.2" (isa-literal (instantiate::mpoint (x 0) (y 9))) 'fail)
   (test "isa-subclass" (isa-subclass mp3) '(1 2))
   (test "isa-or.1" (isa-or mp3) 3)
   (test "isa-or.2" (isa-or (instantiate::mpoint (x 99) (y 0))) 99)
   (test "isa-and" (isa-and mp1) (list mp1 3))
   (test "isa-nested"
      (isa-nested (instantiate::msegment
                     (start (instantiate::mpoint (x 10) (y 20)))
                     (end (instantiate::mpoint (x 30) (y 40)))))
      '(10 20 30 40))
   (test "isa-repeated.1" (isa-repeated (instantiate::mrect (width 5) (height 5))) 5)
   (test "isa-repeated.2" (isa-repeated (instantiate::mrect (width 3) (height 7))) 'fail)
   (test "isa-not.1" (isa-not mp1) 'is-point)
   (test "isa-not.2" (isa-not 42) 'not-point))

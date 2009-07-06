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
   (test "match-string" (match-string '("a" c c)) 3))
 
